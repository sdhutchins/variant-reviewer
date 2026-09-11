# Literature card. Lists recent publications for the gene (Europe PMC), refined
# by the loaded variant's rsID when there is one, as a reactable with each title
# linked to its Europe PMC article page.

literature_ui <- function(id) {
  vr_result_card(id, "Literature (Europe PMC)", "200px", download = TRUE)
}

# resolved: reactive() -> mygene_resolve() result (uses the gene symbol).
# variant_rsid: reactive() -> the loaded variant's rsID (or NULL); when present
# it refines the search to that variant.
literature_server <- function(id, resolved, variant_rsid) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    literature <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      europepmc_search(res$symbol, refine = variant_rsid())
    })

    output$source <- renderUI({
      res <- literature()
      req(res, isTRUE(res$ok))
      vr_source_link(src_europepmc_search(res$query), "Europe PMC")
    })

    output$table <- reactable::renderReactable({
      res <- literature()
      req(res, isTRUE(res$ok))
      df <- res$data
      vr_reactable(
        df,
        columns = list(
          id = reactable::colDef(show = FALSE),
          source = reactable::colDef(show = FALSE),
          doi = reactable::colDef(show = FALSE),
          title = reactable::colDef(
            name = "Title",
            minWidth = 240,
            html = TRUE,
            # Link each title to its Europe PMC article page.
            cell = function(value, index) {
              src <- df$source[index]
              aid <- df$id[index]
              href <- if (is_blank(src) || is_blank(aid)) {
                NULL
              } else {
                paste0("https://europepmc.org/article/", src, "/", aid)
              }
              vr_external_link_html(
                href,
                value
              )
            }
          ),
          authors = reactable::colDef(name = "Authors", minWidth = 140),
          journal = reactable::colDef(name = "Journal", minWidth = 120),
          year = reactable::colDef(name = "Year", maxWidth = 70),
          cited_by = reactable::colDef(
            name = "Cited by",
            maxWidth = 90,
            align = "right"
          )
        )
      )
    })

    vr_result_csv(
      output,
      literature,
      "literature.csv",
      extract = function(value) {
        value$data[c("title", "authors", "journal", "year", "cited_by")]
      },
      ns = ns
    )

    output$content <- renderUI({
      vr_result_ui(
        literature(),
        "Search for a gene to see recent literature.",
        function(res) {
          tagList(
            reactable::reactableOutput(ns("table")),
            if (!is_blank(res$count)) {
              tags$p(
                class = "text-muted small mt-2 mb-0",
                sprintf(
                  "Showing %d of %s matching publications.",
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
    literature
  })
}
