# Variant-specific predictions supplied by ProtVar. The module receives the
# same normalized protein context as the protein, domain, and structure cards.

protvar_predictions_ui <- function(id) {
  vr_result_card(id, "ProtVar predictions", download = TRUE)
}

protvar_predictions_table_data <- function(predictions) {
  data.frame(
    Predictor = vapply(predictions, function(item) item$name, character(1)),
    Score = vapply(predictions, function(item) item$score, numeric(1)),
    `ProtVar interpretation` = vapply(
      predictions,
      function(item) {
        if (is_blank(item$call)) "Not available from ProtVar" else item$call
      },
      character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

protvar_predictions_server <- function(id, resolved, search, annotation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    predictions <- reactive({
      retry$dep()
      query <- search()
      gene <- resolved()
      if (is.null(query) || is_blank(query$variant)) {
        return(NULL)
      }
      context <- protein_variant_context(query, gene, annotation())
      if (is.null(context)) {
        return(list(
          ok = FALSE,
          error = "Could not determine the protein substitution."
        ))
      }
      protvar_predict_variant(
        context$accession,
        context$position,
        context$alt_aa,
        context$cadd_score %||% NA_real_
      )
    })

    output$source <- renderUI({
      result <- predictions()
      req(!is.null(result), isTRUE(result$ok))
      vr_source_link("https://www.ebi.ac.uk/ProtVar/", "ProtVar")
    })

    vr_result_csv(
      output,
      predictions,
      "protvar-predictions.csv",
      extract = function(value) {
        protvar_predictions_table_data(value$predictions)
      },
      ns = ns
    )

    output$content <- renderUI({
      vr_result_ui(
        predictions(),
        "Enter a missense variant to see ProtVar predictions.",
        function(result) {
          rows <- lapply(result$predictions, function(prediction) {
            digits <- if (identical(prediction$name, "CADD")) 1 else 2
            tags$tr(
              tags$td(tags$strong(prediction$name)),
              tags$td(
                class = "text-end",
                if (is.na(prediction$score)) {
                  "Not available"
                } else {
                  vr_num(prediction$score, digits)
                }
              ),
              tags$td(
                class = "text-muted",
                if (is_blank(prediction$call)) {
                  "Not available from ProtVar"
                } else {
                  prediction$call
                }
              )
            )
          })
          tagList(
            tags$p(
              class = "small text-muted mb-2",
              sprintf(
                "%s residue %s, alternate amino acid %s",
                result$accession,
                result$position,
                result$alt_aa
              )
            ),
            if (length(result$warnings %||% character()) > 0) {
              tags$div(
                class = "alert alert-warning py-2 px-3 mb-2",
                role = "status",
                paste(result$warnings, collapse = " ")
              )
            },
            tags$table(
              class = "table table-sm align-middle mb-2",
              tags$thead(tags$tr(
                tags$th("Predictor"),
                tags$th(class = "text-end", "Score"),
                tags$th("ProtVar interpretation")
              )),
              tags$tbody(rows)
            ),
            tags$p(
              class = "text-muted small mb-0",
              "Computational predictions are supporting evidence and should not",
              " be interpreted alone."
            )
          )
        }
      )
    })

    predictions
  })
}
