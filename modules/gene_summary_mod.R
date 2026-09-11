# Gene summary card (MyGene). Renders the shared resolved-gene reactive.

gene_summary_ui <- function(id) {
  vr_result_card(id, "Gene", "120px", copy = TRUE)
}

# resolved: reactive() returning the mygene_resolve() result (or NULL before a
# search has run).
# retry_resolved: the shared `resolved` reactive's retry-bump function (see
# app_server.R). This card has no fetch of its own -- it just renders
# `resolved` directly -- so its refresh button has to retry that instead.
gene_summary_server <- function(id, resolved, retry_resolved) {
  moduleServer(id, function(input, output, session) {
    vr_card_refresh_observer(input, retry_resolved)

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_ncbi_gene(res$entrez), "NCBI Gene")
    })

    output$content <- renderUI({
      vr_result_ui(
        resolved(),
        "Search for a gene to see its summary.",
        function(res) {
          tagList(
            tags$h5(
              res$symbol,
              tags$small(class = "text-muted", paste0("  ", res$name))
            ),
            vr_field("Type", res$type_of_gene),
            vr_field("Entrez", res$entrez),
            vr_field("Ensembl", res$ensembl_gene),
            vr_field("UniProt", res$uniprot),
            if (!is_blank(res$summary)) {
              tags$p(class = "mt-2 mb-0", res$summary)
            }
          )
        }
      )
    })
  })
}
