# Protein summary card (ProtVar). Shows functional context and known variants
# at the residue, given the gene's UniProt accession + a protein position.

protein_summary_ui <- function(id) {
  vr_result_card(id, "Protein (ProtVar)", "120px", copy = TRUE)
}

# resolved:   reactive() -> mygene_resolve() result (for UniProt accession).
# search:     reactive() -> list(gene, variant) (for the protein position).
# annotation: reactive() -> myvariant_annotate() result (shared; supplies the
#             protein position for rsID/HGVS inputs via its hgvsp).
protein_summary_server <- function(id, resolved, search, annotation) {
  moduleServer(id, function(input, output, session) {
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    protein <- reactive({
      retry$dep()
      res <- resolved()
      query <- search()
      if (
        is.null(res) ||
          !isTRUE(res$ok) ||
          is.null(query) ||
          is_blank(query$variant)
      ) {
        return(NULL)
      }
      context <- protein_variant_context(query, res, annotation())
      if (is.null(context)) {
        return(list(
          ok = FALSE,
          error = "Could not determine the protein substitution."
        ))
      }
      protvar_annotate(context$accession, context$position)
    })

    output$source <- renderUI({
      res <- protein()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link("https://www.ebi.ac.uk/ProtVar/", "ProtVar")
    })

    output$content <- renderUI({
      vr_result_ui(
        protein(),
        "Enter a variant to see protein-level context.",
        function(res) {
          tagList(
            vr_field("Accession", res$accession),
            vr_field("Position", res$position),
            if (!is_blank(res$function_text)) {
              tags$p(
                class = "mt-2",
                tags$strong("Function: "),
                res$function_text
              )
            },
            protein_variants_ui(res$variants)
          )
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    protein
  })
}

# Prefer ProtVar's canonical normalization for protein HGVS input. Other input
# formats retain the existing MyGene accession plus MyVariant protein change.
protein_variant_context <- function(query, resolved, annotation = NULL) {
  if (is.null(query) || is_blank(query$variant)) {
    return(NULL)
  }
  if (!is.null(query$protvar)) {
    return(query$protvar)
  }
  if (is.null(resolved) || !isTRUE(resolved$ok) || is_blank(resolved$uniprot)) {
    return(NULL)
  }
  protein_change <- if (isTRUE(annotation$ok) && !is_blank(annotation$hgvsp)) {
    annotation$hgvsp
  } else {
    query$variant
  }
  substitution <- protvar_parse_substitution(protein_change)
  if (is.null(substitution)) {
    return(NULL)
  }
  c(
    list(accession = resolved$uniprot, cadd_score = NA_real_),
    substitution
  )
}

# Render the known substitutions at the selected residue without duplicating
# the selected variant's scores from the dedicated predictions card.
protein_variants_ui <- function(variants) {
  if (is.null(variants) || nrow(variants) == 0) {
    return(vr_empty("No catalogued variants at this residue."))
  }
  tagList(
    tags$p(class = "mt-2 mb-1", tags$strong("Known variants at this residue:")),
    tags$ul(
      class = "mb-0",
      lapply(seq_len(nrow(variants)), function(i) {
        tags$li(
          tags$strong(variants$change[[i]]),
          if (!is_blank(variants$sources[[i]])) {
            tags$span(
              class = "text-muted",
              paste0(" (", variants$sources[[i]], ")")
            )
          }
        )
      })
    )
  )
}
