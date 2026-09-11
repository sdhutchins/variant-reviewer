# STRING protein-protein interaction table. Lists interaction partners and
# evidence scores for the gene as a reactable.

string_ppi_ui <- function(id) {
  vr_result_card(id, "Protein interactions (STRING)", "200px", download = TRUE)
}

# resolved: reactive() -> mygene_resolve() result (uses the gene symbol).
string_ppi_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    partners <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      string_interaction_partners(res$symbol)
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_string(res$symbol), "STRING")
    })

    output$table <- reactable::renderReactable({
      res <- partners()
      req(res, isTRUE(res$ok))
      score_col <- function(name) {
        reactable::colDef(
          name = name,
          format = reactable::colFormat(digits = 3),
          align = "right"
        )
      }
      vr_reactable(
        res$data,
        columns = list(
          partner = reactable::colDef(name = "Partner", minWidth = 120),
          score = score_col("Combined"),
          experimental = score_col("Experimental"),
          database = score_col("Database"),
          coexpression = score_col("Coexpression"),
          textmining = score_col("Text mining")
        )
      )
    })

    vr_result_csv(
      output,
      partners,
      "string-interactions.csv",
      ns = ns
    )

    output$content <- renderUI({
      vr_result_ui(
        partners(),
        "Search for a gene to see interaction partners.",
        function(res) reactable::reactableOutput(ns("table"))
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    partners
  })
}
