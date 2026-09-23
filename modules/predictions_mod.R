# Additional in-silico predictions from dbNSFP via MyVariant: REVEL,
# PolyPhen-2, SIFT, MetaLR, and MetaSVM. ProtVar supplies AlphaMissense and CADD.

predictions_ui <- function(id) {
  vr_result_card(id, "Additional in-silico predictions", download = TRUE)
}

predictions_table_data <- function(predictions) {
  data.frame(
    Predictor = vapply(predictions, function(item) item$name, character(1)),
    Score = vapply(predictions, function(item) item$score, numeric(1)),
    Call = vapply(
      predictions,
      function(item) if (is.na(item$call)) "" else item$call,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

# search: reactive() -> list(gene, variant) (uses the variant string).
predictions_server <- function(id, search) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    predictions <- reactive({
      retry$dep()
      query <- search()
      if (is.null(query) || is_blank(query$variant)) {
        return(NULL)
      }
      normalized <- query$protvar$normalized_hgvs %||%
        query$normalized$lookup %||%
        NA_character_
      term <- if (is_blank(normalized)) query$variant else normalized
      myvariant_predictions(term)
    })

    output$source <- renderUI({
      res <- predictions()
      req(!is.null(res), isTRUE(res$ok))
      variant <- trimws((search())$variant %||% "")
      rsid <- if (grepl("^rs[0-9]+$", variant, ignore.case = TRUE)) {
        tolower(variant)
      }
      vr_source_link(src_dbsnp(rsid), "dbSNP")
    })

    vr_result_csv(
      output,
      predictions,
      "additional-predictions.csv",
      extract = function(value) predictions_table_data(value$predictions),
      ns = ns
    )

    output$content <- renderUI({
      vr_result_ui(
        predictions(),
        "Enter a variant (rsID or HGVS) to see in-silico predictions.",
        function(res) {
          rows <- lapply(res$predictions, function(p) {
            tags$tr(
              tags$td(tags$strong(p$name)),
              tags$td(
                class = "text-end",
                if (is.na(p$score)) "—" else vr_num(p$score, 3)
              ),
              tags$td(class = "text-muted", if (is.na(p$call)) "" else p$call)
            )
          })
          tagList(
            tags$table(
              class = "table table-sm align-middle mb-2",
              tags$thead(
                tags$tr(
                  tags$th("Predictor"),
                  tags$th(class = "text-end", "Score"),
                  tags$th("Call")
                )
              ),
              tags$tbody(rows)
            ),
            tags$p(
              class = "text-muted small mb-0",
              "Additional scores from dbNSFP via MyVariant. Thresholds differ by",
              " tool; use",
              " alongside other evidence, not on their own."
            )
          )
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    predictions
  })
}
