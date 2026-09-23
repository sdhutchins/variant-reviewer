# Parsing helpers for compound clinical variant descriptions. These descriptions
# often put the gene before the RefSeq transcript, while downstream APIs expect
# the transcript-qualified HGVS description on its own.

# Parse a description such as
# COG4(NM_015386.3):c.1750del p.(Glu584SerfsTer29). Unrecognized inputs retain
# their original lookup term so existing ProtVar-supported formats are unchanged.
vr_normalize_variant_input <- function(variant) {
  term <- trimws(as.character(variant %||% ""))
  match <- regexec(
    paste0(
      "^([A-Za-z][A-Za-z0-9-]*)",
      "\\((N[MR]_[0-9]+\\.[0-9]+)\\):",
      "(c\\.[^[:space:]]+)",
      "(?:[[:space:]]+(p\\.\\([^()]+\\)|p\\.[^[:space:]]+))?$"
    ),
    term,
    perl = TRUE
  )
  parts <- regmatches(term, match)[[1]]
  if (length(parts) == 0) {
    return(list(
      recognized = FALSE,
      input = term,
      lookup = term,
      gene = NA_character_,
      transcript = NA_character_,
      hgvsc = NA_character_,
      hgvsp = NA_character_
    ))
  }

  transcript <- parts[[3]]
  coding_change <- parts[[4]]
  list(
    recognized = TRUE,
    input = term,
    lookup = paste0(transcript, ":", coding_change),
    gene = parts[[2]],
    transcript = transcript,
    hgvsc = paste0(transcript, ":", coding_change),
    hgvsp = if (length(parts) >= 5 && nzchar(parts[[5]])) {
      parts[[5]]
    } else {
      NA_character_
    }
  )
}

# Build a useful variant-card record when a valid compound HGVS description is
# outside ProtVar's supported variant classes. The input supplies identity and
# context; MyVariant is used only when it independently resolves the allele.
vr_input_variant_annotation <- function(normalized, myvariant = NULL) {
  if (is.null(normalized) || !isTRUE(normalized$recognized)) {
    return(myvariant)
  }

  myvariant_ok <- isTRUE(myvariant$ok)
  list(
    ok = TRUE,
    id = normalized$hgvsc,
    input = normalized$input,
    rsid = if (myvariant_ok) myvariant$rsid else NA_character_,
    gene = normalized$gene,
    hgvsp = normalized$hgvsp,
    uniprot = NA_character_,
    consequence = NA_character_,
    input_format = "compound coding HGVS",
    clinvar_significance = if (myvariant_ok) {
      myvariant$clinvar_significance
    } else {
      NA_character_
    },
    clinvar_id = NA_character_,
    sources = if (myvariant_ok) "MyVariant" else character()
  )
}
