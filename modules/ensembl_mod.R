# Ensembl VEP card. Shows the most severe consequence plus a table of
# protein-coding transcript consequences for the variant.

ensembl_ui <- function(id) {
  vr_result_card(
    id,
    "Variant consequences (Ensembl VEP)",
    download = TRUE
  )
}

# identifier: reactive() -> normalized genomic variant ID or dbSNP rsID.
ensembl_server <- function(id, identifier, rsid = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    vep <- reactive({
      retry$dep()
      id_value <- identifier()
      if (is_blank(id_value)) {
        return(NULL)
      }
      ensembl_vep(id_value, rsid())
    })

    output$source <- renderUI({
      id_value <- identifier()
      req(!is_blank(id_value))
      vr_source_link(src_ensembl_variant(id_value), "Ensembl")
    })

    output$table <- reactable::renderReactable({
      res <- vep()
      req(res, isTRUE(res$ok), !is.null(res$data))
      vr_reactable(
        res$data,
        page_size = 5,
        columns = list(
          gene = reactable::colDef(name = "Gene", maxWidth = 90),
          transcript = reactable::colDef(name = "Transcript", minWidth = 130),
          consequence = reactable::colDef(name = "Consequence", minWidth = 150),
          impact = reactable::colDef(name = "Impact", maxWidth = 90),
          sift = reactable::colDef(name = "SIFT"),
          polyphen = reactable::colDef(name = "PolyPhen")
        )
      )
    })

    vr_result_csv(
      output,
      vep,
      "variant-consequences.csv",
      ns = ns
    )

    output$content <- renderUI({
      vr_result_ui(
        vep(),
        "Enter a variant (rsID or HGVS) to see VEP consequences.",
        function(res) {
          tagList(
            tags$p(
              class = "mb-2",
              tags$strong("Most severe consequence: "),
              tags$span(
                class = "fw-semibold",
                gsub("_", " ", res$most_severe)
              ),
              if (!is_blank(res$assembly)) {
                tags$span(
                  class = "text-muted",
                  paste0("  (", res$assembly, ")")
                )
              }
            ),
            if (is.null(res$data)) {
              vr_empty("No protein-coding transcript consequences.")
            } else {
              reactable::reactableOutput(ns("table"))
            }
          )
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    vep
  })
}
