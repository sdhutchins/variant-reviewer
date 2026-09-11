# Ensembl VEP client: predicted consequences of a variant.
# REST API docs: https://rest.ensembl.org/documentation/info/vep_id_get

ENSEMBL_BASE <- "https://rest.ensembl.org"

# Run VEP for a dbSNP rsID or ProtVar's normalized GRCh38 variant ID.
# Returns:
#   list(ok = TRUE, most_severe, assembly,
#        data = data.frame(gene, transcript, consequence, impact, sift, polyphen))
#   list(ok = FALSE, error = "...")
ensembl_vep <- function(identifier, rsid = NULL) {
  if (is_blank(identifier)) {
    return(list(ok = FALSE, error = "No variant identifier for VEP lookup."))
  }

  parts <- strsplit(identifier, "-", fixed = TRUE)[[1]]
  is_variant_id <- length(parts) == 4 &&
    grepl("^[0-9]+$", parts[[2]]) &&
    grepl("^[ACGT]+$", parts[[4]], ignore.case = TRUE)
  path <- if (is_variant_id && !is_blank(rsid)) {
    paste0("vep/human/id/", rsid)
  } else if (is_variant_id) {
    region <- paste0(parts[[1]], ":", parts[[2]], "-", parts[[2]], ":1")
    paste0("vep/human/region/", region, "/", parts[[4]])
  } else {
    paste0("vep/human/id/", identifier)
  }

  res <- vr_api_get(
    ENSEMBL_BASE,
    path = path,
    query = list(`content-type` = "application/json"),
    source = "Ensembl VEP",
    timeout = if (is_variant_id && is_blank(rsid)) 30 else 15
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  records <- res$data
  if (is.null(records) || length(records) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("Ensembl VEP has no record for ", identifier, ".")
    ))
  }

  if (is_variant_id) {
    chromosome <- parts[[1]]
    position <- suppressWarnings(as.integer(parts[[2]]))
    matching <- Filter(
      function(record) {
        identical(
          as.character(pluck_at(record, "seq_region_name")),
          chromosome
        ) &&
          identical(
            suppressWarnings(as.integer(pluck_at(record, "start"))),
            position
          )
      },
      records
    )
    if (length(matching) > 0) {
      records <- matching
    }
  }
  ensembl_parse_vep(
    records[[1]],
    alt_allele = if (is_variant_id) parts[[4]] else NULL
  )
}

# Pure parser: a VEP record -> normalized result.
ensembl_parse_vep <- function(record, alt_allele = NULL) {
  consequences <- pluck_at(record, "transcript_consequences")
  if (!is_blank(alt_allele) && length(consequences) > 0) {
    allele_matches <- vapply(
      consequences,
      function(consequence) {
        allele <- pluck_at(consequence, "variant_allele")
        is_blank(allele) ||
          identical(toupper(as.character(allele)), toupper(alt_allele))
      },
      logical(1)
    )
    consequences <- consequences[allele_matches]
  }
  list(
    ok = TRUE,
    most_severe = pluck_at(
      record,
      "most_severe_consequence",
      default = NA_character_
    ),
    assembly = pluck_at(record, "assembly_name", default = NA_character_),
    data = ensembl_consequences_df(consequences)
  )
}

# Exon/transcript model for a gene, from Ensembl's lookup endpoint (canonical
# transcript). Drives the gene-model card, which draws the exons and marks the
# exon the variant falls in. Looked up by Ensembl gene id.
# Returns:
#   list(ok = TRUE, transcript, strand, region, gene_start, gene_end,
#        exons = data.frame(start, end, number))
#   list(ok = FALSE, error = "...")
ensembl_gene_model <- function(gene_id) {
  if (is_blank(gene_id)) {
    return(list(ok = FALSE, error = "No Ensembl gene id for the gene model."))
  }
  res <- vr_api_get(
    ENSEMBL_BASE,
    path = paste0("lookup/id/", gene_id),
    query = list(expand = 1, `content-type` = "application/json"),
    source = "Ensembl"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  ensembl_parse_gene_model(res$data)
}

# Pure parser: pick the canonical transcript (else the first) and tidy its exons
# into a start-sorted, numbered data frame.
ensembl_parse_gene_model <- function(record) {
  transcripts <- pluck_at(record, "Transcript")
  if (is.null(transcripts) || length(transcripts) == 0) {
    return(list(ok = FALSE, error = "Ensembl returned no transcripts."))
  }
  canonical <- Filter(
    function(t) isTRUE(as.logical(pluck_at(t, "is_canonical"))),
    transcripts
  )
  tx <- if (length(canonical) > 0) canonical[[1]] else transcripts[[1]]
  exons <- pluck_at(tx, "Exon")
  if (is.null(exons) || length(exons) == 0) {
    return(list(ok = FALSE, error = "Ensembl returned no exons."))
  }
  rows <- lapply(exons, function(e) {
    data.frame(
      start = suppressWarnings(as.numeric(pluck_at(e, "start", default = NA))),
      end = suppressWarnings(as.numeric(pluck_at(e, "end", default = NA))),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df <- df[!is.na(df$start) & !is.na(df$end), , drop = FALSE]
  if (nrow(df) == 0) {
    return(list(ok = FALSE, error = "Ensembl returned no exon coordinates."))
  }
  df <- df[order(df$start), , drop = FALSE]
  strand <- suppressWarnings(as.numeric(pluck_at(tx, "strand", default = NA)))
  # Number exons in transcription (5'->3') order: on the minus strand that is
  # decreasing genomic coordinate, so the highest-coordinate exon is exon 1.
  df$number <- if (isTRUE(strand < 0)) {
    rev(seq_len(nrow(df)))
  } else {
    seq_len(nrow(df))
  }
  rownames(df) <- NULL
  list(
    ok = TRUE,
    transcript = as.character(pluck_at(tx, "id", default = NA_character_)),
    strand = strand,
    region = as.character(pluck_at(record, "seq_region_name", default = NA)),
    gene_start = suppressWarnings(as.numeric(
      pluck_at(record, "start", default = min(df$start))
    )),
    gene_end = suppressWarnings(as.numeric(
      pluck_at(record, "end", default = max(df$end))
    )),
    exons = df
  )
}

# Build a data.frame of protein-coding transcript consequences (the rows worth
# showing), or NULL when there are none.
ensembl_consequences_df <- function(consequences) {
  if (is.null(consequences) || length(consequences) == 0) {
    return(NULL)
  }
  is_coding <- vapply(
    consequences,
    function(x) {
      identical(pluck_at(x, "biotype"), "protein_coding")
    },
    logical(1)
  )
  consequences <- consequences[is_coding]
  if (length(consequences) == 0) {
    return(NULL)
  }

  chr_field <- function(key) {
    vapply(
      consequences,
      function(x) {
        as.character(pluck_at(x, key, default = NA_character_))
      },
      character(1)
    )
  }
  data.frame(
    gene = chr_field("gene_symbol"),
    transcript = chr_field("transcript_id"),
    consequence = vapply(
      consequences,
      function(x) {
        terms <- pluck_at(x, "consequence_terms")
        if (is.null(terms)) {
          NA_character_
        } else {
          paste(unlist(terms), collapse = ", ")
        }
      },
      character(1)
    ),
    impact = chr_field("impact"),
    sift = chr_field("sift_prediction"),
    polyphen = chr_field("polyphen_prediction"),
    stringsAsFactors = FALSE
  )
}
