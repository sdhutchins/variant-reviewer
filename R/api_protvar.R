# ProtVar (EBI) client: protein-level functional context and known variants at
# a residue. Docs: https://www.ebi.ac.uk/ProtVar/
#
# ProtVar normalizes supported variant inputs through /mapping. Its per-position
# endpoints then use the returned canonical UniProt accession and residue.

PROTVAR_BASE <- "https://www.ebi.ac.uk/ProtVar/api"

# RefSeq accessions for the GRCh38 primary assembled chromosomes. ProtVar maps
# genomic results to GRCh38, so the accession version must match those returned
# coordinates rather than the assembly used to interpret the original input.
.protvar_grch38_refseq <- c(
  `1` = "NC_000001.11",
  `2` = "NC_000002.12",
  `3` = "NC_000003.12",
  `4` = "NC_000004.12",
  `5` = "NC_000005.10",
  `6` = "NC_000006.12",
  `7` = "NC_000007.14",
  `8` = "NC_000008.11",
  `9` = "NC_000009.12",
  `10` = "NC_000010.11",
  `11` = "NC_000011.10",
  `12` = "NC_000012.12",
  `13` = "NC_000013.11",
  `14` = "NC_000014.9",
  `15` = "NC_000015.10",
  `16` = "NC_000016.10",
  `17` = "NC_000017.11",
  `18` = "NC_000018.10",
  `19` = "NC_000019.10",
  `20` = "NC_000020.11",
  `21` = "NC_000021.9",
  `22` = "NC_000022.11",
  X = "NC_000023.11",
  Y = "NC_000024.10"
)

protvar_genomic_hgvs <- function(chromosome, position, ref_base, alt_base) {
  chromosome <- sub("^chr", "", as.character(chromosome), ignore.case = TRUE)
  accession <- unname(.protvar_grch38_refseq[chromosome])
  if (
    is_blank(accession) ||
      is_blank(position) ||
      is_blank(ref_base) ||
      is_blank(alt_base)
  ) {
    return(NA_character_)
  }
  paste0(accession, ":g.", position, ref_base, ">", alt_base)
}

# ProtVar requires an NC_ accession for genomic HGVS. The app also accepts the
# common UCSC-style chrN:g. spelling and translates only that spelling to
# ProtVar's equivalent internal genomic syntax.
protvar_query_term <- function(variant) {
  term <- trimws(as.character(variant %||% ""))
  match <- regexec(
    "^chr([0-9]+|X|Y):g\\.([0-9]+)([ACGT])>([ACGT])$",
    term,
    ignore.case = TRUE
  )
  parts <- regmatches(term, match)[[1]]
  if (length(parts) != 5) {
    return(term)
  }
  paste(
    toupper(parts[[2]]),
    parts[[3]],
    toupper(parts[[4]]),
    toupper(parts[[5]])
  )
}

# Resolve any single-variant syntax supported by ProtVar to its normalized
# genomic and canonical protein representation. ProtVar is the app's one input
# normalizer, so downstream sources do not need to understand every accepted
# spelling themselves.
protvar_find_mappings <- function(variant, assembly = "AUTO") {
  term <- trimws(as.character(variant %||% ""))
  if (is_blank(term)) {
    return(list(ok = FALSE, error = "Enter a variant."))
  }
  assemblies <- c(assembly, if (identical(assembly, "AUTO")) "GRCh37")
  parsed <- NULL
  for (assembly_attempt in assemblies) {
    response <- vr_api_get(
      PROTVAR_BASE,
      path = "mapping",
      query = list(
        q = protvar_query_term(term),
        assembly = assembly_attempt
      ),
      source = "ProtVar"
    )
    if (!response$ok) {
      parsed <- list(ok = FALSE, error = response$error)
      next
    }
    parsed <- protvar_parse_mappings(response$data, term)
    if (isTRUE(parsed$ok)) {
      parsed$assembly <- assembly_attempt
      return(parsed)
    }
  }
  parsed
}

# Parse ProtVar's canonical protein mappings. Nested non-canonical isoforms are
# projections of the same allele and therefore remain context, not choices.
protvar_parse_mappings <- function(data, term = NA_character_) {
  inputs <- pluck_at(data, "content", "inputs")
  if (is.null(inputs) || length(inputs) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("No ProtVar mapping for '", term, "'.")
    ))
  }
  make_row <- function(input, variant, gene, isoform) {
    accession <- as.character(pluck_at(
      isoform,
      "accession",
      default = NA_character_
    ))
    position <- suppressWarnings(as.integer(pluck_at(
      isoform,
      "isoformPosition"
    )))
    ref_aa <- protvar_aa_one(pluck_at(isoform, "refAA"))
    alt_aa <- protvar_aa_one(pluck_at(isoform, "variantAA"))
    gene_name <- as.character(pluck_at(
      gene,
      "geneName",
      default = NA_character_
    ))
    chromosome <- as.character(pluck_at(
      variant,
      "chromosome",
      default = NA_character_
    ))
    genomic_position <- suppressWarnings(as.integer(pluck_at(
      variant,
      "position"
    )))
    ref_base <- as.character(pluck_at(
      variant,
      "refBase",
      default = NA_character_
    ))
    alt_base <- as.character(pluck_at(
      variant,
      "altBase",
      default = NA_character_
    ))
    genomic <- if (
      any(is.na(c(chromosome, genomic_position, ref_base, alt_base)))
    ) {
      NA_character_
    } else {
      paste(chromosome, genomic_position, ref_base, alt_base, sep = "-")
    }
    normalized_hgvs <- protvar_genomic_hgvs(
      chromosome,
      genomic_position,
      ref_base,
      alt_base
    )
    consequence <- as.character(
      pluck_at(isoform, "consequences", default = NA_character_)
    )
    protein_change <- if (
      is_blank(accession) || is.na(position) || is.na(alt_aa)
    ) {
      NA_character_
    } else {
      paste0(
        accession,
        ":p.",
        if (is.na(ref_aa)) "?" else ref_aa,
        position,
        alt_aa
      )
    }
    label_parts <- c(genomic, gene_name, protein_change, consequence)
    label_parts <- label_parts[!is.na(label_parts) & nzchar(label_parts)]
    data.frame(
      id = paste(accession, position, ref_aa, alt_aa, genomic, sep = "|"),
      label = paste(label_parts, collapse = " | "),
      accession = accession,
      position = position,
      ref_aa = ref_aa,
      alt_aa = alt_aa,
      gene = gene_name,
      genomic = genomic,
      normalized_hgvs = normalized_hgvs,
      consequence = consequence,
      cadd_score = suppressWarnings(as.numeric(pluck_at(gene, "caddScore"))),
      format = as.character(pluck_at(input, "format", default = NA_character_)),
      type = as.character(pluck_at(input, "type", default = NA_character_)),
      stringsAsFactors = FALSE
    )
  }
  rows <- lapply(inputs, function(input) {
    candidates <- list()
    target_accession <- pluck_at(input, "derivedUniprotAcc") %||%
      pluck_at(input, "accession")
    target_position <- suppressWarnings(as.integer(
      pluck_at(input, "derivedProtPos") %||%
        pluck_at(input, "aaPos") %||%
        pluck_at(input, "position")
    ))
    target_alt <- protvar_aa_one(pluck_at(input, "altAA"))
    for (variant in pluck_at(input, "derivedGenomicVariants") %||% list()) {
      for (gene in pluck_at(variant, "genes") %||% list()) {
        for (isoform in pluck_at(gene, "isoforms") %||% list()) {
          matches_input <- !is_blank(target_accession) &&
            identical(
              as.character(pluck_at(isoform, "accession")),
              as.character(target_accession)
            ) &&
            identical(
              suppressWarnings(as.integer(pluck_at(
                isoform,
                "isoformPosition"
              ))),
              target_position
            ) &&
            (is.na(target_alt) ||
              identical(
                protvar_aa_one(pluck_at(isoform, "variantAA")),
                target_alt
              ))
          if (isTRUE(pluck_at(isoform, "canonical")) || matches_input) {
            candidates[[length(candidates) + 1L]] <- make_row(
              input,
              variant,
              gene,
              isoform
            )
          }
        }
      }
    }
    candidates
  })
  rows <- Filter(Negate(is.null), do.call(c, rows))
  if (length(rows) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("ProtVar could not map '", term, "' to a protein variant.")
    ))
  }
  mappings <- unique(do.call(rbind, rows))
  mappings <- mappings[
    !is.na(mappings$accession) &
      !is.na(mappings$position) &
      !is.na(mappings$alt_aa),
    ,
    drop = FALSE
  ]
  if (nrow(mappings) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("ProtVar could not map '", term, "' to a protein variant.")
    ))
  }
  rownames(mappings) <- NULL
  list(ok = TRUE, mappings = mappings)
}

protvar_mapping_record <- function(mapping) {
  value <- function(name, default = NA_character_) {
    if (name %in% names(mapping)) mapping[[name]][[1]] else default
  }
  list(
    accession = value("accession"),
    position = value("position", NA_integer_),
    ref_aa = value("ref_aa"),
    alt_aa = value("alt_aa"),
    gene = value("gene"),
    genomic = value("genomic"),
    normalized_hgvs = value("normalized_hgvs"),
    consequence = value("consequence"),
    cadd_score = value("cadd_score", NA_real_),
    format = value("format"),
    type = value("type")
  )
}

# Build the shared variant-card record from ProtVar's normalized mapping, then
# add fields that MyVariant can supply. The normalized identity remains stable
# even when MyVariant does not recognize the user's original notation.
protvar_variant_annotation <- function(
  mapping,
  original,
  myvariant = NULL,
  protvar_metadata = NULL
) {
  if (is.null(mapping)) {
    return(myvariant)
  }
  protein_change <- if (
    is_blank(mapping$position) || is_blank(mapping$alt_aa)
  ) {
    NA_character_
  } else {
    paste0(
      "p.",
      if (is_blank(mapping$ref_aa)) "?" else mapping$ref_aa,
      mapping$position,
      mapping$alt_aa
    )
  }
  myvariant_ok <- isTRUE(myvariant$ok)
  original_rsid <- if (grepl("^rs[0-9]+$", original, ignore.case = TRUE)) {
    tolower(original)
  } else {
    NA_character_
  }
  list(
    ok = TRUE,
    id = if (!is_blank(mapping$normalized_hgvs)) {
      mapping$normalized_hgvs
    } else {
      original
    },
    input = original,
    rsid = if (myvariant_ok && !is_blank(myvariant$rsid)) {
      myvariant$rsid
    } else if (isTRUE(protvar_metadata$ok)) {
      protvar_metadata$rsid
    } else {
      original_rsid
    },
    gene = if (!is_blank(mapping$gene)) {
      mapping$gene
    } else if (myvariant_ok) {
      myvariant$gene
    } else {
      NA_character_
    },
    hgvsp = protein_change,
    uniprot = mapping$accession %||% NA_character_,
    consequence = mapping$consequence %||% NA_character_,
    input_format = mapping$format %||% NA_character_,
    clinvar_significance = if (
      myvariant_ok && !is_blank(myvariant$clinvar_significance)
    ) {
      myvariant$clinvar_significance
    } else if (isTRUE(protvar_metadata$ok)) {
      protvar_metadata$clinical_significance
    } else {
      NA_character_
    },
    clinvar_id = if (isTRUE(protvar_metadata$ok)) {
      protvar_metadata$clinvar_id
    } else {
      NA_character_
    },
    sources = c("ProtVar", if (myvariant_ok) "MyVariant")
  )
}

# Prefer an allele-specific ClinVar cross-reference over a shared dbSNP rsID.
# This preserves the variant selected when one rsID maps to several alleles.
protvar_clinvar_lookup_id <- function(annotation) {
  if (is.null(annotation) || !isTRUE(annotation$ok)) {
    return(NULL)
  }
  if (!is_blank(annotation$clinvar_id)) {
    annotation$clinvar_id
  } else if (!is_blank(annotation$rsid)) {
    annotation$rsid
  } else {
    NULL
  }
}

# Extract the reference and alternate amino acids from a missense protein
# change. This supports one- or three-letter HGVS forms and bare changes.
protvar_parse_substitution <- function(variant) {
  if (is_blank(variant)) {
    return(NULL)
  }
  term <- trimws(as.character(variant))
  match <- regexec(
    "(?:.*:)?(?:p\\.)?([A-Za-z]{1,3})([0-9]+)([A-Za-z]{1,3})$",
    term
  )
  parts <- regmatches(term, match)[[1]]
  if (length(parts) != 4) {
    return(NULL)
  }
  ref_aa <- protvar_aa_one(parts[[2]])
  alt_aa <- protvar_aa_one(parts[[4]])
  if (is.na(ref_aa) || is.na(alt_aa)) {
    return(NULL)
  }
  list(ref_aa = ref_aa, position = as.integer(parts[[3]]), alt_aa = alt_aa)
}

# Combine ProtVar function context and variants at a residue into one result.
# Returns:
#   list(ok = TRUE, accession, position, function_text,
#        variants = data.frame with one row per amino-acid substitution)
#   list(ok = FALSE, error = "...")
protvar_annotate <- function(accession, position) {
  if (is_blank(accession)) {
    return(list(ok = FALSE, error = "No UniProt accession available."))
  }
  if (is_blank(position)) {
    return(list(ok = FALSE, error = "No protein position available."))
  }
  path_pos <- paste0(accession, "/", position)

  fn <- vr_api_get(
    PROTVAR_BASE,
    path = paste0("function/", path_pos),
    source = "ProtVar"
  )
  if (!fn$ok) {
    return(list(ok = FALSE, error = fn$error))
  }

  pop <- vr_api_get(
    PROTVAR_BASE,
    path = paste0("population/", path_pos),
    source = "ProtVar"
  )

  list(
    ok = TRUE,
    accession = accession,
    position = position,
    function_text = protvar_function_text(fn$data),
    variants = if (pop$ok) protvar_variants_df(pop$data) else NULL
  )
}

.protvar_amino_acids <- c(
  ALA = "A",
  ARG = "R",
  ASN = "N",
  ASP = "D",
  CYS = "C",
  GLN = "Q",
  GLU = "E",
  GLY = "G",
  HIS = "H",
  ILE = "I",
  LEU = "L",
  LYS = "K",
  MET = "M",
  PHE = "F",
  PRO = "P",
  SER = "S",
  THR = "T",
  TRP = "W",
  TYR = "Y",
  VAL = "V",
  SEC = "U",
  PYL = "O"
)

# ProtVar uses both one- and three-letter amino-acid codes across endpoints.
protvar_aa_one <- function(amino_acid) {
  if (is_blank(amino_acid)) {
    return(NA_character_)
  }
  code <- toupper(trimws(as.character(amino_acid)))
  if (nchar(code) == 1 && code %in% unname(.protvar_amino_acids)) {
    return(code)
  }
  unname(.protvar_amino_acids[code])
}

protvar_cadd_call <- function(score) {
  if (is.na(score)) {
    return(NA_character_)
  }
  if (score < 15) {
    "likely benign"
  } else if (score < 20) {
    "potentially deleterious"
  } else if (score < 25) {
    "quite likely deleterious"
  } else if (score < 30) {
    "probably deleterious"
  } else {
    "highly likely deleterious"
  }
}

protvar_esm_call <- function(score) {
  if (is.na(score)) {
    return(NA_character_)
  }
  if (score >= -5) {
    "benign"
  } else if (score >= -10) {
    "uncertain"
  } else {
    "pathogenic"
  }
}

protvar_foldx_call <- function(score) {
  if (is.na(score)) {
    return(NA_character_)
  }
  if (score >= 2) {
    "likely to be destabilising"
  } else {
    "unlikely to be destabilising"
  }
}

# UniProt FUNCTION comments carry their evidence inline, e.g. "...binding to its
# target DNA sequence (PubMed:11025664, PubMed:12524540, PubMed:12810724)". For
# TP53 the citations are longer than the prose they support, which is noise on a
# dashboard card. Drop the citation groups and tidy the punctuation left behind,
# keeping non-citation notes like "(By similarity)".
protvar_strip_citations <- function(text) {
  if (is_blank(text)) {
    return(text)
  }
  ref <- "(?:PubMed:\\d+|Ref\\.\\s*\\d+|ECO:[0-9|.A-Za-z:-]+)"
  out <- as.character(text)
  # Parentheticals that are nothing but citations.
  out <- gsub(
    sprintf("\\s*\\(%s(?:\\s*[,;]\\s*%s)*\\)", ref, ref),
    "",
    out,
    perl = TRUE
  )
  # Citations mixed into a parenthetical that also says something else.
  out <- gsub(sprintf("\\s*[,;]?\\s*%s", ref), "", out, perl = TRUE)
  # Tidy up: empty brackets, space before punctuation, doubled spaces.
  out <- gsub("\\(\\s*[,;]*\\s*\\)", "", out, perl = TRUE)
  out <- gsub("\\s+([.,;:])", "\\1", out, perl = TRUE)
  out <- gsub("\\s{2,}", " ", out, perl = TRUE)
  trimws(out)
}

# Pull the first FUNCTION comment text from a /function response.
protvar_function_text <- function(data) {
  comments <- pluck_at(data, "comments")
  if (is.null(comments)) {
    return(NA_character_)
  }
  for (cm in comments) {
    if (identical(pluck_at(cm, "type"), "FUNCTION")) {
      txt <- pluck_at(cm, "text")
      if (!is.null(txt) && length(txt) > 0) {
        return(protvar_strip_citations(as.character(pluck_at(
          txt[[1]],
          "value",
          default = NA_character_
        ))))
      }
    }
  }
  NA_character_
}

# Summarize known variants at the residue. Genomic alleles stay in a list column
# because one protein substitution can be produced by more than one allele.
protvar_variants_df <- function(data) {
  variants <- pluck_at(data, "variants")
  if (is.null(variants) || length(variants) == 0) {
    return(NULL)
  }
  change <- vapply(
    variants,
    function(v) {
      as.character(pluck_at(v, "alternativeSequence", default = NA_character_))
    },
    character(1)
  )
  wild_type <- vapply(
    variants,
    function(v) {
      as.character(pluck_at(v, "wildType", default = NA_character_))
    },
    character(1)
  )
  sources <- vapply(
    variants,
    function(v) {
      xrefs <- pluck_at(v, "xrefs")
      if (is.null(xrefs)) {
        return(NA_character_)
      }
      names <- unique(vapply(
        xrefs,
        function(x) {
          as.character(pluck_at(x, "name", default = NA_character_))
        },
        character(1)
      ))
      names <- names[!is.na(names)]
      if (length(names) == 0) NA_character_ else paste(names, collapse = ", ")
    },
    character(1)
  )

  genomic <- lapply(variants, function(v) {
    locations <- as.character(unlist(
      pluck_at(v, "genomicLocation"),
      use.names = FALSE
    ))
    unique(locations[!is.na(locations) & nzchar(locations)])
  })
  df <- data.frame(
    wild_type = wild_type,
    change = change,
    amino_acid = vapply(change, protvar_aa_one, character(1)),
    sources = sources,
    stringsAsFactors = FALSE
  )
  df$genomic <- genomic
  rownames(df) <- NULL
  df[!is.na(df$change), , drop = FALSE]
}

# Extract identifiers and clinical evidence for the selected genomic allele
# from ProtVar's residue-level population response. Matching the full RefSeq
# HGVS prevents an rsID from a different substitution at the residue leaking
# into downstream cards.
protvar_parse_variant_metadata <- function(data, genomic_hgvs) {
  variants <- pluck_at(data, "variants") %||% list()
  matches <- Filter(
    function(variant) {
      locations <- as.character(unlist(
        pluck_at(variant, "genomicLocation"),
        use.names = FALSE
      ))
      genomic_hgvs %in% locations
    },
    variants
  )
  if (length(matches) == 0) {
    return(list(
      ok = FALSE,
      error = "ProtVar has no matching population record."
    ))
  }
  variant <- matches[[1]]
  xrefs <- pluck_at(variant, "xrefs") %||% list()
  xref_id <- function(name, pattern = NULL) {
    ids <- vapply(
      xrefs,
      function(xref) {
        xref_name <- as.character(pluck_at(xref, "name", default = ""))
        id <- as.character(pluck_at(xref, "id", default = NA_character_))
        name_match <- identical(tolower(xref_name), tolower(name))
        pattern_match <- !is.null(pattern) &&
          !is.na(id) &&
          grepl(pattern, id, ignore.case = TRUE)
        if (name_match || pattern_match) id else NA_character_
      },
      character(1)
    )
    ids <- unique(ids[!is.na(ids) & nzchar(ids)])
    if (length(ids) == 0) NA_character_ else ids[[1]]
  }
  significance <- vapply(
    pluck_at(variant, "clinicalSignificances") %||% list(),
    function(entry) {
      as.character(pluck_at(entry, "type", default = NA_character_))
    },
    character(1)
  )
  significance <- unique(significance[
    !is.na(significance) & nzchar(significance)
  ])
  frequencies <- vapply(
    pluck_at(variant, "populationFrequencies") %||% list(),
    function(entry) {
      suppressWarnings(as.numeric(pluck_at(entry, "frequency", default = NA)))
    },
    numeric(1)
  )
  list(
    ok = TRUE,
    rsid = xref_id("dbSNP", "^rs[0-9]+$"),
    clinvar_id = xref_id("ClinVar", "^(RCV|VCV)[0-9]+"),
    clinical_significance = if (length(significance) == 0) {
      NA_character_
    } else {
      paste(significance, collapse = "; ")
    },
    population_frequency = if (all(is.na(frequencies))) {
      NA_real_
    } else {
      max(frequencies, na.rm = TRUE)
    }
  )
}

protvar_variant_metadata <- function(mapping) {
  if (
    is.null(mapping) ||
      is_blank(mapping$accession) ||
      is_blank(mapping$position) ||
      is_blank(mapping$normalized_hgvs)
  ) {
    return(list(ok = FALSE, error = "No normalized ProtVar allele."))
  }
  response <- vr_api_get(
    PROTVAR_BASE,
    path = paste0("population/", mapping$accession, "/", mapping$position),
    source = "ProtVar"
  )
  if (!response$ok) {
    return(list(ok = FALSE, error = response$error))
  }
  protvar_parse_variant_metadata(response$data, mapping$normalized_hgvs)
}

# Extract allele-specific CADD scores from a ProtVar mapping response. The
# accession and position filters prevent scores from another mapped isoform from
# being attached to the displayed canonical protein substitution.
protvar_parse_mapping_cadd <- function(data, accession, position) {
  inputs <- pluck_at(data, "content", "inputs")
  if (is.null(inputs)) {
    return(NULL)
  }
  rows <- list()
  for (input in inputs) {
    genomic_variants <- pluck_at(input, "derivedGenomicVariants") %||% list()
    for (variant in genomic_variants) {
      allele <- paste(
        pluck_at(variant, "chromosome"),
        pluck_at(variant, "position"),
        pluck_at(variant, "refBase"),
        pluck_at(variant, "altBase"),
        sep = "-"
      )
      for (gene in pluck_at(variant, "genes") %||% list()) {
        score <- suppressWarnings(as.numeric(pluck_at(gene, "caddScore")))
        if (length(score) != 1 || is.na(score)) {
          next
        }
        for (isoform in pluck_at(gene, "isoforms") %||% list()) {
          same_accession <- identical(
            as.character(pluck_at(isoform, "accession")),
            as.character(accession)
          )
          same_position <- identical(
            suppressWarnings(as.integer(pluck_at(isoform, "isoformPosition"))),
            as.integer(position)
          )
          if (!same_accession || !same_position) {
            next
          }
          rows[[length(rows) + 1L]] <- data.frame(
            amino_acid = protvar_aa_one(pluck_at(isoform, "variantAA")),
            allele = allele,
            score = score,
            call = protvar_cadd_call(score),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  result <- unique(do.call(rbind, rows))
  rownames(result) <- NULL
  result
}

# Parse the substitution-specific score response. Classification text is either
# supplied by ProtVar or derived from the ranges used by the ProtVar interface.
protvar_parse_scores <- function(data) {
  find_type <- function(type) {
    matches <- Filter(
      function(entry) identical(pluck_at(entry, "type"), type),
      data %||% list()
    )
    if (length(matches) == 0) NULL else matches[[1]]
  }
  alpha <- find_type("AM")
  esm <- find_type("ESM")
  missense3d <- find_type("M3D")
  alpha_score <- suppressWarnings(as.numeric(pluck_at(
    alpha,
    "amPathogenicity",
    default = NA
  )))
  esm_score <- suppressWarnings(as.numeric(pluck_at(
    esm,
    "score",
    default = NA
  )))
  m3d_prediction <- as.character(pluck_at(
    missense3d,
    "prediction",
    default = NA_character_
  ))
  m3d_feature <- as.character(pluck_at(
    missense3d,
    "damagingFeature",
    default = NA_character_
  ))
  list(
    alpha_score = alpha_score,
    alpha_call = if (is_blank(alpha)) {
      NA_character_
    } else {
      tolower(as.character(pluck_at(alpha, "amClass", default = NA_character_)))
    },
    esm_score = esm_score,
    esm_call = protvar_esm_call(esm_score),
    missense3d = if (is_blank(m3d_prediction)) {
      NA_character_
    } else if (!is_blank(m3d_feature) && !identical(m3d_feature, "-")) {
      paste(tolower(m3d_prediction), m3d_feature, sep = ": ")
    } else {
      tolower(m3d_prediction)
    }
  )
}

protvar_parse_foldx <- function(data) {
  if (is.null(data) || length(data) == 0) {
    return(NULL)
  }
  rows <- lapply(data, function(entry) {
    score <- suppressWarnings(as.numeric(pluck_at(entry, "foldxDdg")))
    data.frame(
      amino_acid = protvar_aa_one(pluck_at(entry, "mutatedType")),
      score = score,
      call = protvar_foldx_call(score),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result <- result[!duplicated(result$amino_acid), , drop = FALSE]
  rownames(result) <- NULL
  result
}

# Retrieve the five ProtVar predictions for one selected substitution. CADD is
# accepted from the original mapping when available because it is a genomic
# allele score, while the other predictors are protein-substitution scores.
protvar_predict_variant <- function(
  accession,
  position,
  alt_aa,
  cadd_score = NA_real_
) {
  if (is_blank(accession) || is_blank(position) || is_blank(alt_aa)) {
    return(list(ok = FALSE, error = "No resolved protein substitution."))
  }
  path_pos <- paste0(accession, "/", position)
  scores_response <- vr_api_get(
    PROTVAR_BASE,
    path = paste0("score/", path_pos),
    query = list(mt = alt_aa),
    source = "ProtVar"
  )
  if (!scores_response$ok) {
    return(list(ok = FALSE, error = scores_response$error))
  }
  scores <- protvar_parse_scores(scores_response$data)
  if (is.na(suppressWarnings(as.numeric(cadd_score)))) {
    mapping_response <- vr_api_get(
      PROTVAR_BASE,
      path = "mapping",
      query = list(q = paste(accession, position)),
      source = "ProtVar"
    )
    cadd <- if (mapping_response$ok) {
      protvar_parse_mapping_cadd(mapping_response$data, accession, position)
    }
    cadd_match <- if (is.null(cadd)) {
      NULL
    } else {
      cadd[!is.na(cadd$amino_acid) & cadd$amino_acid == alt_aa, , drop = FALSE]
    }
    # Do not collapse different genomic alleles into one score. Protein HGVS
    # inputs carry their allele-specific CADD value from /mapping.
    if (!is.null(cadd_match) && nrow(cadd_match) == 1) {
      cadd_score <- cadd_match$score[[1]]
    }
  }
  foldx_response <- vr_api_get(
    PROTVAR_BASE,
    path = paste0("prediction/foldx/", path_pos),
    query = list(variantAA = alt_aa),
    source = "ProtVar"
  )
  foldx <- if (foldx_response$ok) {
    protvar_parse_foldx(foldx_response$data)
  }
  foldx_match <- if (is.null(foldx)) {
    NULL
  } else {
    foldx[!is.na(foldx$amino_acid) & foldx$amino_acid == alt_aa, , drop = FALSE]
  }
  foldx_score <- if (is.null(foldx_match) || nrow(foldx_match) == 0) {
    NA_real_
  } else {
    foldx_match$score[[1]]
  }
  warnings <- if (foldx_response$ok) {
    character()
  } else {
    paste("FoldX prediction unavailable.", foldx_response$error)
  }
  list(
    ok = TRUE,
    accession = accession,
    position = as.integer(position),
    alt_aa = alt_aa,
    warnings = warnings,
    predictions = list(
      list(
        name = "AlphaMissense",
        score = scores$alpha_score,
        call = scores$alpha_call
      ),
      list(
        name = "CADD",
        score = suppressWarnings(as.numeric(cadd_score)),
        call = protvar_cadd_call(suppressWarnings(as.numeric(cadd_score)))
      ),
      list(name = "ESM-1b", score = scores$esm_score, call = scores$esm_call),
      list(
        name = "FoldX - Stability change (ΔΔG)",
        score = foldx_score,
        call = protvar_foldx_call(foldx_score)
      ),
      list(name = "Missense3D", score = NA_real_, call = scores$missense3d)
    )
  )
}
