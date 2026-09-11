# GTEx tissue expression card. Plots median TPM across tissues for the gene.

gtex_expression_ui <- function(id) {
  vr_result_card(id, "Tissue expression (GTEx)", "300px")
}

# resolved: reactive() -> mygene_resolve() result (uses the gene symbol).
gtex_expression_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    expression <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      gtex_median_expression(res$symbol)
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_gtex(res$symbol), "GTEx")
    })

    output$plot <- renderPlot(
      {
        res <- expression()
        req(res, isTRUE(res$ok))
        df <- res$data
        df <- df[order(df$median_tpm, decreasing = TRUE), , drop = FALSE]
        df$tissue <- factor(df$tissue, levels = df$tissue)
        ggplot2::ggplot(df, ggplot2::aes(x = tissue, y = median_tpm)) +
          ggplot2::geom_col(fill = vr_colors$primary) +
          ggplot2::labs(x = NULL, y = "Median TPM") +
          ggplot2::theme_minimal(base_size = 15) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = 60,
              hjust = 1,
              vjust = 1,
              size = 9
            ),
            axis.text.y = ggplot2::element_text(size = 12),
            axis.title = ggplot2::element_text(size = 14),
            plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 70)
          )
      },
      alt = paste(
        "Bar chart of median gene expression across GTEx human tissues,",
        "ordered from highest to lowest median TPM."
      )
    )

    output$content <- renderUI({
      vr_result_ui(
        expression(),
        "Search for a gene to see tissue expression.",
        function(res) {
          # Width now carries the tissue count, so the card no longer grows one
          # row per tissue and dominates the rest of the dashboard.
          plotOutput(ns("plot"), height = "360px")
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    expression
  })
}
