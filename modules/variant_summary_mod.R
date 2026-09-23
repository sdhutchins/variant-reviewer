# Variant summary card. The normalized input or ProtVar supplies the identity,
# and MyVariant enriches fields it can resolve for the selected allele.

variant_summary_ui <- function(id) {
  vr_result_card(id, "Variant", "120px", copy = TRUE)
}

# annotation: reactive() returning the combined variant annotation, or NULL
# when no variant was supplied.
# retry_annotation: the shared `annotation_raw` reactive's retry-bump function
# (see app_server.R). This card has no fetch of its own -- it just renders
# `annotation` directly -- so its refresh button has to retry that instead.
variant_summary_server <- function(id, annotation, retry_annotation) {
  moduleServer(id, function(input, output, session) {
    vr_card_refresh_observer(input, retry_annotation)

    output$source <- renderUI({
      res <- annotation()
      req(!is.null(res), isTRUE(res$ok))
      sources <- res$sources %||% character()
      tags$span(
        class = "d-inline-flex align-items-center gap-2",
        if ("ProtVar" %in% sources) {
          vr_source_link("https://www.ebi.ac.uk/ProtVar/", "ProtVar")
        },
        if ("MyVariant" %in% sources) {
          vr_source_link("https://myvariant.info/", "MyVariant")
        }
      )
    })

    output$content <- renderUI({
      vr_result_ui(
        annotation(),
        "Enter a variant (rsID or HGVS) to annotate it.",
        function(res) {
          tagList(
            vr_field("Normalized variant", res$id),
            vr_field("dbSNP", res$rsid),
            vr_field("Gene", res$gene),
            vr_field("UniProt", res$uniprot),
            vr_field("Protein change", res$hgvsp),
            vr_field("Consequence", res$consequence),
            vr_field("ClinVar accession", res$clinvar_id),
            vr_field("ClinVar significance", res$clinvar_significance)
          )
        }
      )
    })
  })
}
