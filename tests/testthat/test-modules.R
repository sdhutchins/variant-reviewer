# Reactive-logic tests for modules using shiny::testServer() (no browser).

test_that("gene_search_server is NULL before submit, emits the query after", {
  testServer(gene_search_server, {
    # Before any submit the value is NULL (not an error), so result cards can
    # show their placeholder messages.
    expect_null(session$returned())

    session$setInputs(
      input_type = "gene",
      gene = "TP53",
      variant = "R175H",
      submit = 1
    )
    query <- session$returned()
    expect_equal(query$gene, "TP53")
    expect_null(query$variant)
  })
})

test_that("gene_search_server submits only the selected input type", {
  original <- myvariant_find_matches
  myvariant_find_matches <<- function(variant, ...) {
    list(
      ok = TRUE,
      matches = data.frame(id = "chr17:g.7675088C>T", label = "TP53 | R175H")
    )
  }
  on.exit(myvariant_find_matches <<- original, add = TRUE)

  testServer(gene_search_server, {
    session$setInputs(
      input_type = "variant",
      gene = "TP53",
      variant = "R175H",
      submit = 1
    )
    query <- session$returned()
    expect_equal(query$gene, "")
    expect_equal(query$variant, "R175H")
  })
})

test_that("gene_search_server asks the user to resolve multiple matches", {
  original <- myvariant_find_matches
  myvariant_find_matches <<- function(variant, ...) {
    list(
      ok = TRUE,
      matches = data.frame(
        id = c("chr7:g.140453136A>G", "chr7:g.140453136A>T"),
        label = c("BRAF | p.V600A", "BRAF | p.V600E")
      )
    )
  }
  on.exit(myvariant_find_matches <<- original, add = TRUE)

  testServer(gene_search_server, {
    session$setInputs(
      input_type = "variant",
      variant = "rs113488022",
      submit = 1
    )
    expect_null(session$returned())
    match_html <- paste(as.character(output$variant_match_ui), collapse = " ")
    expect_match(match_html, "p.V600E")

    session$setInputs(
      variant_match = "chr7:g.140453136A>T",
      submit = 2
    )
    expect_equal(session$returned()$variant, "chr7:g.140453136A>T")
  })
})

test_that("gene_search_server treats a blank gene as no query", {
  testServer(gene_search_server, {
    session$setInputs(gene = "   ", variant = "", submit = 1)
    expect_null(session$returned())
  })
})

test_that("gene_search_server example click fills inputs but does not submit", {
  testServer(gene_search_server, {
    # Clicking the example only populates the inputs; the user still clicks
    # Review, so no query is emitted yet.
    session$setInputs(example = 1)
    expect_null(session$returned())
  })
})

test_that("gene_search_server loads variant suggestions for a valid gene", {
  original <- myvariant_gene_variants
  myvariant_gene_variants <<- function(symbol, ...) {
    list(
      ok = TRUE,
      variants = data.frame(
        rsid = "rs113488022",
        label = "V600E",
        significance = "Pathogenic",
        cadd = 32
      )
    )
  }
  on.exit(myvariant_gene_variants <<- original, add = TRUE)

  testServer(gene_search_server, {
    session$setInputs(input_type = "gene", gene = "BRAF")
    session$elapse(700)
    hint_html <- paste(as.character(output$gene_variant_hint), collapse = " ")
    expect_match(hint_html, "1 known pathogenic")
  })
})

test_that("an outside request runs through the same submit as a Review click", {
  # The assistant hands the module a request; the module fills its inputs and
  # submits, so the query comes out exactly as if the user had clicked Review.
  requested <- reactiveVal(NULL)
  original <- myvariant_find_matches
  myvariant_find_matches <<- function(variant, ...) {
    list(
      ok = TRUE,
      matches = data.frame(id = "chr7:g.140453136A>T", label = "BRAF | p.V600E")
    )
  }
  on.exit(myvariant_find_matches <<- original, add = TRUE)

  testServer(gene_search_server, args = list(requested = requested), {
    expect_null(session$returned())

    requested(list(gene = "BRAF", variant = "rs113488022", nonce = 1L))
    session$flushReact()
    query <- session$returned()
    expect_equal(query$gene, "")
    expect_equal(query$variant, "rs113488022")
  })
})

test_that("an outside request is validated like a typed one", {
  requested <- reactiveVal(NULL)
  testServer(gene_search_server, args = list(requested = requested), {
    # A malformed gene is rejected at the same gate, so no query is emitted and
    # no downstream lookups fire.
    requested(list(gene = "not a gene!", variant = NULL, nonce = 1L))
    session$flushReact()
    expect_null(session$returned())
    expect_false(is.null(validation()))
  })
})

test_that("gene_search_server returns NULL variant when omitted", {
  testServer(gene_search_server, {
    session$setInputs(gene = "BRCA1", variant = "", submit = 1)
    query <- session$returned()
    expect_equal(query$gene, "BRCA1")
    expect_null(query$variant)
  })
})

test_that("gene_summary_server renders an error state without crashing", {
  resolved <- reactive(list(ok = FALSE, error = "No gene found."))
  retried <- 0L
  testServer(
    gene_summary_server,
    args = list(resolved = resolved, retry_resolved = function() {
      retried <<- retried + 1L
    }),
    {
      # renderUI evaluates lazily; force it and confirm it builds HTML.
      expect_no_error(output$content)

      # The header's refresh button (see vr_card_header()) bumps the retry
      # function this card was given, since it has no fetch of its own.
      session$setInputs(refresh = 1)
      expect_equal(retried, 1L)
    }
  )
})
