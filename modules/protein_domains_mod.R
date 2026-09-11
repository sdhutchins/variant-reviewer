# Protein domains & features card. Lists the protein's UniProt features
# (domains, regions, sites) and calls out which one the variant residue is in.

protein_domains_ui <- function(id) {
  vr_result_card(id, "Protein domains & features", "200px", download = TRUE)
}

protein_features_table_data <- function(features) {
  data.frame(
    Feature = features$label,
    Description = ifelse(
      nzchar(features$description),
      features$description,
      features$label
    ),
    Range = ifelse(
      features$begin == features$end,
      as.character(features$begin),
      paste0(features$begin, "–", features$end)
    ),
    stringsAsFactors = FALSE
  )
}

# resolved:   reactive() -> mygene_resolve() result (uses the UniProt accession)
# search:     reactive() -> list(gene, variant)
# annotation: reactive() -> myvariant_annotate() result (for the HGVS position)
protein_domains_server <- function(id, resolved, search, annotation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    features <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      query <- search()
      context <- if (is.null(query) || is_blank(query$variant)) {
        NULL
      } else {
        protein_variant_context(query, res, annotation())
      }
      accession <- context$accession %||% res$uniprot
      if (is_blank(accession)) {
        return(list(ok = FALSE, error = "No UniProt accession for this gene."))
      }
      out <- proteins_features(accession)
      # Attach the variant residue (may be NULL) so the card can highlight it.
      if (isTRUE(out$ok)) {
        out$position <- if (is.null(query) || is_blank(query$variant)) {
          NULL
        } else {
          context$position
        }
      }
      out
    })

    # TRUE for a feature row spanning the variant residue (for highlighting).
    row_at_variant <- function(df, position) {
      pos <- suppressWarnings(as.integer(position %||% NA))
      if (is.na(pos)) {
        return(rep(FALSE, nrow(df)))
      }
      !is.na(df$begin) & !is.na(df$end) & df$begin <= pos & df$end >= pos
    }

    output$table <- reactable::renderReactable({
      res <- features()
      req(res, isTRUE(res$ok), nrow(res$features) > 0)
      df <- res$features
      hot <- row_at_variant(df, res$position)
      display <- protein_features_table_data(df)
      vr_reactable(
        display,
        page_size = 8,
        searchable = FALSE,
        # Emphasize the feature(s) the variant residue falls in.
        rowStyle = function(index) {
          if (isTRUE(hot[index])) {
            list(fontWeight = "600", background = "var(--bs-secondary-bg)")
          }
        }
      )
    })

    vr_result_csv(
      output,
      features,
      "protein-domains.csv",
      extract = function(value) protein_features_table_data(value$features),
      ns = ns
    )

    output$source <- renderUI({
      res <- features()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(
        src_uniprot(res$accession, "family_and_domains"),
        "UniProt"
      )
    })

    output$content <- renderUI({
      vr_result_ui(
        features(),
        "Search for a gene to see protein domains and features.",
        function(res) {
          if (nrow(res$features) == 0) {
            return(vr_empty(
              "No annotated domains or features for this protein."
            ))
          }
          pos <- res$position
          header <- if (
            !is.null(pos) && !is.na(suppressWarnings(as.integer(pos)))
          ) {
            hits <- proteins_features_at(res$features, pos)
            if (nrow(hits) > 0) {
              labels <- paste(
                vapply(
                  seq_len(nrow(hits)),
                  function(i) {
                    proteins_feature_text(hits, i)
                  },
                  character(1)
                ),
                collapse = ", "
              )
              tags$p(
                class = "mb-2",
                tags$strong(sprintf("Residue %s is in: ", pos)),
                labels
              )
            } else {
              tags$p(
                class = "text-muted mb-2",
                sprintf(
                  "Residue %s is not within an annotated domain or site.",
                  pos
                )
              )
            }
          }
          tagList(
            header,
            reactable::reactableOutput(ns("table"))
          )
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    features
  })
}
