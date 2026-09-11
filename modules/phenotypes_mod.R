# HPO phenotype card. Lists the human phenotypes associated with the gene
# (Monarch Initiative, aggregating HPO annotations) as a reactable, each term
# linked to its Monarch page (hpo.jax.org's own term-browser URL no longer
# resolves; Monarch mirrors the same HPO term pages under its CURIE scheme).

phenotypes_ui <- function(id) {
  vr_result_card(id, "Phenotypes (HPO)", "200px", download = TRUE)
}

# resolved: reactive() -> mygene_resolve() result (uses the HGNC id).
phenotypes_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    phenotypes <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      if (is_blank(res$hgnc)) {
        return(list(
          ok = FALSE,
          error = "No HGNC id for this gene, so phenotypes can't be looked up."
        ))
      }
      monarch_phenotypes(res$hgnc)
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_monarch_gene(res$hgnc), "Monarch")
    })

    output$table <- reactable::renderReactable({
      res <- phenotypes()
      req(res, isTRUE(res$ok))
      df <- res$data
      vr_reactable(
        df,
        columns = list(
          hpo_id = reactable::colDef(
            name = "HPO term",
            maxWidth = 140,
            html = TRUE,
            # Link each term to its Monarch page.
            cell = function(value) {
              vr_external_link_html(
                paste0("https://monarchinitiative.org/", value),
                value
              )
            }
          ),
          phenotype = reactable::colDef(name = "Phenotype")
        )
      )
    })

    vr_result_csv(
      output,
      phenotypes,
      "hpo-phenotypes.csv",
      ns = ns
    )

    output$content <- renderUI({
      vr_result_ui(
        phenotypes(),
        "Search for a gene to see associated phenotypes.",
        function(res) {
          tagList(
            reactable::reactableOutput(ns("table")),
            if (!is_blank(res$count)) {
              tags$p(
                class = "text-muted small mt-2 mb-0",
                sprintf(
                  "Showing %d of %s associated HPO phenotypes.",
                  nrow(res$data),
                  format(res$count, big.mark = ",")
                )
              )
            }
          )
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    phenotypes
  })
}
