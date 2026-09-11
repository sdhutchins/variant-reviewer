# Reactive-logic tests for modules using shiny::testServer() (no browser).

test_that("gene search shows compact examples for supported variant formats", {
  html <- paste(as.character(gene_search_ui("search")), collapse = " ")

  expect_match(html, "Protein HGVS · TBC1D7 Pro267Ser", fixed = TRUE)
  expect_match(html, "rsID · rs113488022", fixed = TRUE)
  expect_match(html, "Genomic · chr7:g.140453136A&gt;T", fixed = TRUE)
  expect_equal(length(gregexpr("vr-example-pill", html, fixed = TRUE)[[1]]), 3L)
})

test_that("long drug indications have a preview and expandable details", {
  indication <- paste(rep("a", 251), collapse = "")

  preview <- drug_indication_preview(indication)
  expect_equal(nchar(preview, type = "chars"), 251L)
  expect_equal(substr(preview, 251L, 251L), "\u2026")
  expect_match(as.character(drug_indication_details(indication)), indication)

  short_indication <- "Pulmonary arterial hypertension"
  expect_identical(drug_indication_preview(short_indication), short_indication)
  expect_null(drug_indication_details(short_indication))
})

test_that("protein variant UI lists substitutions without predictor scores", {
  variants <- data.frame(
    wild_type = "Val",
    change = "Glu",
    amino_acid = "E",
    sources = "ClinVar",
    stringsAsFactors = FALSE
  )
  variants$genomic <- list("7-140753336-A-T")
  html <- paste(as.character(protein_variants_ui(variants)), collapse = " ")
  expect_match(html, "Glu")
  expect_match(html, "ClinVar")
  expect_false(grepl("AlphaMissense", html, fixed = TRUE))
})

test_that("protein HGVS search uses one canonical ProtVar mapping", {
  original <- protvar_find_mappings
  protvar_find_mappings <<- function(variant, ...) {
    list(
      ok = TRUE,
      mappings = data.frame(
        id = "Q9P0N9|267|P|S|6-13305184-G-A",
        label = "Q9P0N9:p.P267S (canonical)",
        accession = "Q9P0N9",
        position = 267L,
        ref_aa = "P",
        alt_aa = "S",
        gene = "TBC1D7",
        genomic = "6-13305184-G-A",
        cadd_score = 27.5
      )
    )
  }
  on.exit(protvar_find_mappings <<- original, add = TRUE)

  testServer(gene_search_server, {
    session$setInputs(
      input_type = "variant",
      variant = "NP_001305738.1:p.Pro267Ser",
      submit = 1
    )
    query <- session$returned()
    expect_equal(query$variant, "NP_001305738.1:p.Pro267Ser")
    expect_equal(query$protvar$accession, "Q9P0N9")
    expect_equal(query$protvar$position, 267L)
    expect_equal(query$protvar$alt_aa, "S")
  })
})

test_that("protein HGVS search asks only for distinct canonical mappings", {
  original <- protvar_find_mappings
  protvar_find_mappings <<- function(variant, ...) {
    mappings <- data.frame(
      id = c("P11111|10|A|V|1-10-A-T", "P22222|20|A|V|1-20-A-T"),
      label = c("P11111:p.A10V (canonical)", "P22222:p.A20V (canonical)"),
      accession = c("P11111", "P22222"),
      position = c(10L, 20L),
      ref_aa = c("A", "A"),
      alt_aa = c("V", "V"),
      gene = c("GENE1", "GENE2"),
      genomic = c("1-10-A-T", "1-20-A-T"),
      cadd_score = c(20, 25)
    )
    list(ok = TRUE, mappings = mappings)
  }
  on.exit(protvar_find_mappings <<- original, add = TRUE)

  testServer(gene_search_server, {
    session$setInputs(
      input_type = "variant",
      variant = "NP_000001.1:p.Ala10Val",
      submit = 1
    )
    expect_null(session$returned())
    expect_match(
      paste(as.character(output$variant_match_ui), collapse = " "),
      "Select the matching variant or residue"
    )
    session$setInputs(variant_match = "P22222|20|A|V|1-20-A-T", submit = 2)
    expect_equal(session$returned()$protvar$accession, "P22222")
  })
})

test_that("ProtVar prediction card renders the selected substitution", {
  original <- protvar_predict_variant
  protvar_predict_variant <<- function(
    accession,
    position,
    alt_aa,
    cadd_score = NA_real_
  ) {
    list(
      ok = TRUE,
      accession = accession,
      position = position,
      alt_aa = alt_aa,
      warnings = "FoldX prediction unavailable. Timed out.",
      predictions = list(
        list(name = "AlphaMissense", score = 0.7085, call = "pathogenic"),
        list(name = "CADD", score = cadd_score, call = "probably deleterious"),
        list(name = "ESM-1b", score = -5.506, call = "uncertain"),
        list(
          name = "FoldX - Stability change (ΔΔG)",
          score = 1.49267,
          call = "unlikely to be destabilising"
        ),
        list(name = "Missense3D", score = NA_real_, call = "neutral")
      )
    )
  }
  on.exit(protvar_predict_variant <<- original, add = TRUE)
  query <- reactive(list(
    gene = "",
    variant = "NP_001305738.1:p.Pro267Ser",
    protvar = list(
      accession = "Q9P0N9",
      position = 267L,
      ref_aa = "P",
      alt_aa = "S",
      cadd_score = 27.5
    )
  ))

  testServer(
    protvar_predictions_server,
    args = list(
      resolved = reactive(list(ok = TRUE, uniprot = "Q9P0N9")),
      search = query,
      annotation = reactive(NULL)
    ),
    {
      html <- paste(as.character(output$content), collapse = " ")
      expect_match(html, "AlphaMissense")
      expect_match(html, "27.5")
      expect_match(html, "-5.51")
      expect_match(html, "Missense3D")
      expect_match(html, "neutral")
      expect_match(html, "FoldX prediction unavailable")
      expect_match(html, "role=\"status\"")
    }
  )
})

test_that("prediction downloads use plain table values", {
  predictions <- list(
    list(name = "AlphaMissense", score = 0.71, call = "pathogenic"),
    list(name = "Missense3D", score = NA_real_, call = "")
  )

  protvar <- protvar_predictions_table_data(predictions)
  additional <- predictions_table_data(predictions)

  expect_equal(
    names(protvar),
    c("Predictor", "Score", "ProtVar interpretation")
  )
  expect_equal(protvar[[3]][2], "Not available from ProtVar")
  expect_equal(names(additional), c("Predictor", "Score", "Call"))
  expect_true(is.na(additional$Score[2]))
})

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
  original <- protvar_find_mappings
  protvar_find_mappings <<- function(variant, ...) {
    mapping <- data.frame(
      id = "P04637|175|R|H|17-7675088-C-T",
      label = "17-7675088-C-T | TP53 | P04637:p.R175H",
      accession = "P04637",
      position = 175L,
      ref_aa = "R",
      alt_aa = "H",
      gene = "TP53",
      genomic = "17-7675088-C-T",
      cadd_score = 25
    )
    list(ok = TRUE, mappings = mapping)
  }
  on.exit(protvar_find_mappings <<- original, add = TRUE)

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
  original <- protvar_find_mappings
  protvar_find_mappings <<- function(variant, ...) {
    list(
      ok = TRUE,
      mappings = data.frame(
        id = c(
          "P15056|600|V|A|7-140753336-A-G",
          "P15056|600|V|E|7-140753336-A-T"
        ),
        label = c("BRAF | P15056:p.V600A", "BRAF | P15056:p.V600E"),
        accession = c("P15056", "P15056"),
        position = c(600L, 600L),
        ref_aa = c("V", "V"),
        alt_aa = c("A", "E"),
        gene = c("BRAF", "BRAF"),
        genomic = c("7-140753336-A-G", "7-140753336-A-T"),
        cadd_score = c(20, 30)
      )
    )
  }
  on.exit(protvar_find_mappings <<- original, add = TRUE)

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
      variant_match = "P15056|600|V|E|7-140753336-A-T",
      submit = 2
    )
    expect_equal(session$returned()$variant, "rs113488022")
    expect_equal(session$returned()$protvar$alt_aa, "E")
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
  original <- protvar_find_mappings
  protvar_find_mappings <<- function(variant, ...) {
    mapping <- data.frame(
      id = "P15056|600|V|E|7-140753336-A-T",
      label = "BRAF | P15056:p.V600E",
      accession = "P15056",
      position = 600L,
      ref_aa = "V",
      alt_aa = "E",
      gene = "BRAF",
      genomic = "7-140753336-A-T",
      cadd_score = 30
    )
    list(ok = TRUE, mappings = mapping)
  }
  on.exit(protvar_find_mappings <<- original, add = TRUE)

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
