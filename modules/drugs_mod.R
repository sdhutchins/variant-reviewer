# Known-drugs card. Lists the drugs and clinical candidates that target the
# gene's protein (Open Targets), with their highest clinical stage and the
# indication(s) they have been tried against, as a reactable.

drug_indication_preview <- function(value, max_characters = 250L) {
  if (is_blank(value) || nchar(value, type = "chars") <= max_characters) {
    return(value)
  }
  paste0(substr(value, 1L, max_characters), "\u2026")
}

drug_indication_details <- function(value, max_characters = 250L) {
  if (is_blank(value) || nchar(value, type = "chars") <= max_characters) {
    return(NULL)
  }
  tags$div(
    class = "px-3 py-2",
    tags$strong("Full indication(s)"),
    tags$p(class = "mb-0 mt-1", value)
  )
}

drugs_ui <- function(id) {
  vr_result_card(id, "Known drugs (Open Targets)", "200px", download = TRUE)
}

# resolved: reactive() -> mygene_resolve() result (uses the Ensembl gene ID).
drugs_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    drugs <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      opentargets_drugs(res$ensembl_gene)
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_opentargets_drugs(res$ensembl_gene), "Open Targets")
    })

    output$table <- reactable::renderReactable({
      res <- drugs()
      req(res, isTRUE(res$ok))
      df <- res$data
      vr_reactable(
        df,
        class = "vr-drugs-table",
        columns = list(
          drug_id = reactable::colDef(show = FALSE),
          drug = reactable::colDef(
            name = "Drug",
            minWidth = 140,
            html = TRUE,
            rowHeader = TRUE,
            # Link each drug to its Open Targets page.
            cell = function(value, index) {
              id <- df$drug_id[index]
              href <- if (is_blank(id)) {
                NULL
              } else {
                paste0("https://platform.opentargets.org/drug/", id)
              }
              vr_external_link_html(
                href,
                value
              )
            }
          ),
          drug_type = reactable::colDef(name = "Type", maxWidth = 130),
          max_phase = reactable::colDef(name = "Max phase", maxWidth = 110),
          disease = reactable::colDef(
            name = "Indication(s)",
            minWidth = 160,
            cell = function(value) {
              tags$span(drug_indication_preview(value))
            },
            details = function(index) {
              drug_indication_details(df$disease[index])
            }
          )
        ),
        language = reactable::reactableLang(
          detailsExpandLabel = "Show full indication",
          detailsCollapseLabel = "Hide full indication"
        )
      )
    })

    vr_result_csv(
      output,
      drugs,
      "known-drugs.csv",
      extract = function(value) {
        value$data[c("drug", "drug_type", "max_phase", "disease")]
      },
      ns = ns
    )

    output$content <- renderUI({
      vr_result_ui(
        drugs(),
        "Search for a gene to see known drugs.",
        function(res) {
          tagList(
            reactable::reactableOutput(ns("table")),
            if (!is_blank(res$count)) {
              tags$p(
                class = "text-muted small mt-2 mb-0",
                sprintf(
                  "Showing %d of %s drugs and clinical candidates.",
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
    drugs
  })
}
