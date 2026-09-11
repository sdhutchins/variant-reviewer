test_that("app_version() returns the current version string", {
  expect_type(app_version(), "character")
  expect_match(app_version(), "^[0-9]+[.][0-9]+[.][0-9]+$")
})

test_that("About provenance distinguishes pinned and live data releases", {
  protvar <- Filter(
    function(entry) identical(entry[[1]], "ProtVar"),
    .about_provenance
  )[[1]]
  expect_identical(protvar[[2]], "API 2.0")
  expect_match(protvar[[3]], "Data 2.1")
  expect_match(protvar[[3]], "CADD v1.7")
  expect_true(any(vapply(
    .about_provenance,
    function(entry) grepl("not pinned", entry[[3]], fixed = TRUE),
    logical(1)
  )))
  expect_true(all(vapply(
    .about_provenance,
    function(entry) startsWith(entry[[4]], "https://"),
    logical(1)
  )))
})

test_that("About table downloads preserve every displayed row", {
  annotations <- about_annotations_table_data()
  provenance <- about_provenance_table_data()

  expect_equal(nrow(annotations), length(.about_annotations))
  expect_equal(
    names(annotations),
    c("Annotation", "Source", "What it shows")
  )
  expect_equal(nrow(provenance), length(.about_provenance))
  expect_equal(
    names(provenance),
    c("Source", "API", "Data release used", "Documentation", "Release notes")
  )
})

test_that("safe_read_rds() returns the default for a missing file", {
  expect_null(safe_read_rds(tempfile()))
  expect_identical(safe_read_rds(tempfile(), default = "fallback"), "fallback")
})

test_that("safe_read_rds() reads an existing file", {
  path <- tempfile(fileext = ".rds")
  saveRDS(mtcars, path)
  on.exit(unlink(path), add = TRUE)

  expect_equal(safe_read_rds(path), mtcars)
})

test_that("vr_csv_button() creates a compact accessible download", {
  button <- vr_csv_button("download", "Download results as CSV")
  html <- as.character(button)

  expect_match(html, "vr-csv-download", fixed = TRUE)
  expect_match(html, "vr-header-action", fixed = TRUE)
  expect_match(html, "CSV", fixed = TRUE)
  expect_match(html, 'aria-label="Download results as CSV"', fixed = TRUE)
})

test_that("vr_copy_button() exposes an accessible clipboard action", {
  html <- as.character(vr_copy_button("Copy Gene text"))

  expect_match(html, "vr-card-copy", fixed = TRUE)
  expect_match(html, "vr-header-action", fixed = TRUE)
  expect_match(html, "btn-outline-secondary", fixed = TRUE)
  expect_match(html, "vrCopyCardText(this)", fixed = TRUE)
  expect_match(html, "vr-copy-label", fixed = TRUE)
  expect_match(html, ">Copy<", fixed = TRUE)
  expect_match(html, "vr-copy-icon-success", fixed = TRUE)
  expect_match(html, "vr-copy-icon-error", fixed = TRUE)
  expect_match(html, 'aria-label="Copy Gene text"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
})

test_that("vr_result_card() preserves the standard card contract", {
  html <- as.character(vr_result_card(
    "example",
    "Example results",
    "240px",
    copy = TRUE,
    download = TRUE
  ))

  expect_match(html, "bslib-card", fixed = TRUE)
  expect_match(html, "Example results", fixed = TRUE)
  expect_match(html, 'id="example-content"', fixed = TRUE)
  expect_match(html, "example-source", fixed = TRUE)
  expect_match(html, "example-refresh", fixed = TRUE)
  expect_match(html, "example-download_control", fixed = TRUE)
  expect_match(html, "vr-card-copy", fixed = TRUE)
  expect_match(html, 'style="height:240px"', fixed = TRUE)
})

test_that("vr_result_ui() applies shared empty and error states", {
  empty <- vr_result_ui(NULL, "Nothing loaded.", identity)
  error <- vr_result_ui(
    list(ok = FALSE, error = "Provider failed."),
    "Nothing loaded.",
    identity
  )
  success <- vr_result_ui(
    list(ok = TRUE, value = "ready"),
    "Nothing loaded.",
    function(result) tags$p(result$value)
  )

  expect_match(as.character(empty), "Nothing loaded", fixed = TRUE)
  expect_match(as.character(error), "Provider failed", fixed = TRUE)
  expect_match(as.character(success), "ready", fixed = TRUE)
})

test_that("vr_reactable() applies the shared table behavior", {
  table <- vr_reactable(
    data.frame(value = 1),
    page_size = 7,
    searchable = FALSE
  )
  attributes <- table$x$tag$attribs

  expect_identical(attributes$defaultPageSize, 7)
  expect_true(attributes$showPageSizeOptions)
  expect_true(attributes$compact)
  expect_true(attributes$highlight)
  expect_null(attributes$searchable)
})

test_that("vr_external_link_html() escapes provider-controlled values", {
  html <- vr_external_link_html(
    "https://example.org/?value=<unsafe>",
    "<script>alert('unsafe')</script>"
  )
  fallback <- vr_external_link_html(NULL, "<strong>plain</strong>")

  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "value=&lt;unsafe&gt;", fixed = TRUE)
  expect_identical(fallback, "&lt;strong&gt;plain&lt;/strong&gt;")
})

test_that("vr_result_csv() exposes a populated card download control", {
  testServer(
    function(id) {
      moduleServer(id, function(input, output, session) {
        result <- reactive(list(
          ok = TRUE,
          data = data.frame(value = 1)
        ))
        vr_result_csv(
          output,
          result,
          "results.csv",
          ns = session$ns
        )
      })
    },
    {
      html <- paste(as.character(output$download_control), collapse = " ")
      expect_match(html, "vr-csv-download", fixed = TRUE)
      expect_match(html, 'id="proxy1-download"', fixed = TRUE)
    }
  )
})

test_that("copy controls appear only on text cards", {
  dashboard_text_cards <- paste(
    as.character(gene_summary_ui("gene_summary")),
    as.character(variant_summary_ui("variant_summary")),
    as.character(protein_summary_ui("protein_summary")),
    as.character(clinvar_ui("clinvar")),
    as.character(gnomad_ui("gnomad")),
    as.character(gene_constraint_ui("constraint"))
  )
  about_html <- as.character(about_page)
  table_html <- as.character(predictions_ui("predictions"))

  expect_equal(
    length(gregexpr("vr-card-copy", dashboard_text_cards, fixed = TRUE)[[1]]),
    6L
  )
  expect_equal(
    length(gregexpr("vr-card-copy", about_html, fixed = TRUE)[[1]]),
    5L
  )
  expect_false(grepl("vr-card-copy", table_html, fixed = TRUE))
})

test_that("CSV controls appear in every table card header", {
  dashboard_tables <- paste(
    as.character(predictions_ui("predictions")),
    as.character(protvar_predictions_ui("protvar_predictions")),
    as.character(protein_domains_ui("domains")),
    as.character(ensembl_ui("ensembl")),
    as.character(string_ppi_ui("string_ppi")),
    as.character(opentargets_ui("opentargets")),
    as.character(phenotypes_ui("phenotypes")),
    as.character(drugs_ui("drugs")),
    as.character(pharmacogenomics_ui("pharmacogenomics")),
    as.character(literature_ui("literature"))
  )
  about_html <- as.character(about_page)

  expect_equal(
    length(gregexpr("download_control", dashboard_tables, fixed = TRUE)[[1]]),
    10L
  )
  expect_equal(
    length(gregexpr("vr-csv-download", about_html, fixed = TRUE)[[1]]),
    2L
  )
})

test_that("valid empty results are visually distinct from errors", {
  empty <- as.character(vr_error("No publications found."))
  error <- as.character(vr_error("Europe PMC request failed."))

  expect_match(empty, "vr-empty-result", fixed = TRUE)
  expect_match(empty, 'role="status"', fixed = TRUE)
  expect_false(grepl("alert-warning", empty, fixed = TRUE))
  expect_match(error, "alert-warning", fixed = TRUE)
  expect_match(error, 'role="alert"', fixed = TRUE)
})
