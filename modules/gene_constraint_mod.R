# gnomAD gene-constraint card. How intolerant the gene is to loss-of-function
# (pLI, LOEUF) and to missense/synonymous variation (Z-scores).

gene_constraint_ui <- function(id) {
  vr_result_card(id, "Gene constraint (gnomAD)", copy = TRUE)
}

# resolved: reactive() -> mygene_resolve() result (uses the gene symbol).
gene_constraint_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    constraint <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      gnomad_gene_constraint(res$symbol)
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_gnomad_gene(res$ensembl_gene), "gnomAD")
    })

    output$content <- renderUI({
      vr_result_ui(
        constraint(),
        "Search for a gene to see constraint metrics.",
        function(res) {
          pli_note <- if (!is.na(res$pli)) {
            if (res$pli >= 0.9) {
              " (LoF-intolerant)"
            } else if (res$pli <= 0.1) {
              " (LoF-tolerant)"
            } else {
              " (intermediate)"
            }
          } else {
            ""
          }
          loeuf_note <- if (!is.na(res$loeuf) && res$loeuf < 0.35) {
            " (constrained)"
          } else {
            ""
          }
          tagList(
            vr_field(
              "pLI",
              if (is.na(res$pli)) NULL else paste0(vr_num(res$pli, 3), pli_note)
            ),
            vr_field(
              "LOEUF",
              if (is.na(res$loeuf)) {
                NULL
              } else {
                paste0(vr_num(res$loeuf, 2), loeuf_note)
              }
            ),
            vr_field(
              "Missense Z",
              if (is.na(res$mis_z)) NULL else vr_num(res$mis_z, 2)
            ),
            vr_field(
              "Synonymous Z",
              if (is.na(res$syn_z)) NULL else vr_num(res$syn_z, 2)
            ),
            vr_field(
              "Observed/expected LoF",
              if (is.na(res$oe_lof)) NULL else vr_num(res$oe_lof, 2)
            ),
            vr_field(
              "Observed/expected missense",
              if (is.na(res$oe_mis)) NULL else vr_num(res$oe_mis, 2)
            ),
            tags$p(
              class = "text-muted small mb-0 mt-2",
              "pLI ≥ 0.9 or LOEUF < 0.35 indicates loss-of-function intolerance."
            )
          )
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    constraint
  })
}
