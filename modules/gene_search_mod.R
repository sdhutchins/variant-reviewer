# Gene-or-variant search box. Returns a reactive carrying the submitted query so
# the parent can fan it out to the result modules.

# Representative formats that exercise the app's ProtVar normalization path.
.gene_search_examples <- list(
  protein_hgvs = list(
    label = "Protein HGVS · TBC1D7 Pro267Ser",
    variant = "NP_001305738.1:p.Pro267Ser"
  ),
  rsid = list(
    label = "rsID · rs113488022",
    variant = "rs113488022"
  ),
  genomic = list(
    label = "Genomic · chr7:g.140453136A>T",
    variant = "chr7:g.140453136A>T"
  )
)

# The navbar Demo continues to use the protein HGVS example and its shorter
# display name while sharing the exact value used by the matching pill.
.gene_search_example <- list(
  label = "TBC1D7 Pro267Ser",
  variant = .gene_search_examples$protein_hgvs$variant
)

gene_search_ui <- function(id) {
  ns <- NS(id)

  card(
    card_body(
      radioButtons(
        ns("input_type"),
        label = "Search by",
        choices = c("Gene" = "gene", "Variant" = "variant"),
        selected = "gene",
        inline = TRUE
      ),
      layout_columns(
        col_widths = c(9, 3),
        div(
          conditionalPanel(
            condition = sprintf("input['%s'] === 'gene'", ns("input_type")),
            layout_columns(
              col_widths = c(7, 5),
              textInput(
                ns("gene"),
                label = "Gene symbol",
                placeholder = "e.g. TP53",
                width = "100%"
              ),
              selectizeInput(
                ns("gene_variant"),
                label = "Variant (optional)",
                choices = NULL,
                multiple = FALSE,
                width = "100%",
                options = list(
                  create = TRUE,
                  placeholder = "Select or enter a variant",
                  onInitialize = I('function() { this.setValue(""); }'),
                  render = I(
                    "{ option_create: function(data, escape) {
                       return '<div class=\"create\">Use \"' +
                         escape(data.input) + '\"</div>'; } }"
                  )
                )
              )
            ),
            uiOutput(ns("gene_variant_hint"))
          ),
          conditionalPanel(
            condition = sprintf(
              "input['%s'] === 'variant'",
              ns("input_type")
            ),
            layout_columns(
              col_widths = c(8, 4),
              textInput(
                ns("variant"),
                label = "Variant",
                placeholder = "e.g. NP_001305738.1:p.Pro267Ser",
                width = "100%"
              ),
              selectInput(
                ns("assembly"),
                label = "Genome assembly",
                choices = c(
                  "Auto-detect" = "AUTO",
                  "GRCh38" = "GRCh38",
                  "GRCh37" = "GRCh37"
                ),
                selected = "AUTO",
                width = "100%"
              )
            )
          ),
          uiOutput(ns("variant_match_ui"))
        ),
        div(
          class = "d-grid align-self-end mb-1",
          actionButton(
            ns("submit"),
            label = "Review",
            icon = icon("magnifying-glass"),
            class = "btn-primary"
          )
        )
      ),
      tags$div(
        class = "small text-muted",
        paste(
          "Choose a gene symbol or enter one ProtVar-supported variant:",
          "HGVS, VCF fields, genomic coordinates, UniProt protein notation,",
          "gnomAD, dbSNP, ClinVar, or COSMIC ID."
        )
      ),
      # Format-specific examples fill the form but preserve Review as the only
      # action that starts external API requests.
      tags$div(
        class = paste(
          "small text-muted d-flex flex-wrap align-items-center",
          "gap-2 mt-2"
        ),
        tags$span("Try an example:"),
        lapply(
          names(.gene_search_examples),
          function(example_id) {
            example <- .gene_search_examples[[example_id]]
            actionLink(
              ns(paste0("example_", example_id)),
              example$label,
              class = "vr-example-pill"
            )
          }
        )
      ),
      # Inline validation feedback: shown when the gene/variant fails the
      # format check, in which case no search is submitted.
      uiOutput(ns("validation"))
    )
  )
}

# Returns a reactive carrying list(gene, variant, protvar), or NULL before the
# first submit (and when the gene is blank). A reactiveVal is used
# instead of eventReactive so reading it before any submit yields NULL rather
# than a silent error, which lets the result cards show their initial
# placeholder messages.
#
# `requested` is an optional reactive carrying list(gene, variant, nonce) from
# outside the module (the assistant). It is handled exactly like a Review click:
# the visible inputs are filled and the same submit path runs. Callers cannot
# write the query directly, so the search box stays the only way a search
# starts.
gene_search_server <- function(id, requested = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    query <- reactiveVal(NULL)
    # Validation messages from the last submit (character vector), or NULL.
    validation <- reactiveVal(NULL)
    # Multiple genomic records matching the current variant entry, or NULL.
    variant_matches <- reactiveVal(NULL)
    # Parsed gene-level suggestions used for the count beneath the inputs.
    gene_variant_suggestions <- reactiveVal(NULL)

    # Update the gene-mode variant choices while preserving a custom value the
    # user has already typed.
    refresh_gene_variant_choices <- function(choices = character()) {
      current <- isolate(input$gene_variant) %||% ""
      if (nzchar(current) && !(current %in% choices)) {
        choices <- c(stats::setNames(current, current), choices)
      }
      updateSelectizeInput(
        session,
        "gene_variant",
        choices = choices,
        selected = if (nzchar(current)) current else "",
        server = FALSE
      )
    }

    # Restore the original gene-driven list of notable variants. Debouncing
    # avoids querying while the user is still typing.
    gene_debounced <- debounce(reactive(trimws(input$gene %||% "")), 600)
    observeEvent(gene_debounced(), {
      gene <- gene_debounced()
      if (!isTRUE(vr_validate_gene(gene)$ok)) {
        gene_variant_suggestions(NULL)
        refresh_gene_variant_choices()
        return()
      }
      parsed <- myvariant_gene_variants(gene)
      gene_variant_suggestions(parsed)
      refresh_gene_variant_choices(myvariant_variant_choices(parsed))
    })

    output$gene_variant_hint <- renderUI({
      parsed <- gene_variant_suggestions()
      if (is.null(parsed) || !isTRUE(parsed$ok)) {
        return(NULL)
      }
      n <- nrow(parsed$variants)
      tags$div(
        class = "small text-muted mt-1",
        sprintf(
          "%d known pathogenic/likely-pathogenic variant%s for %s.",
          n,
          if (n == 1) "" else "s",
          trimws(input$gene %||% "")
        )
      )
    })

    output$variant_match_ui <- renderUI({
      resolved <- variant_matches()
      choices <- resolved$mappings
      if (is.null(resolved) || is.null(choices) || nrow(choices) <= 1) {
        return(NULL)
      }
      tagList(
        selectInput(
          session$ns("variant_match"),
          "Select the matching variant or residue",
          choices = c(
            "Choose a match" = "",
            stats::setNames(choices$id, choices$label)
          ),
          selected = "",
          width = "100%"
        ),
        tags$div(
          class = "small text-muted",
          "ProtVar returned more than one distinct canonical mapping."
        )
      )
    })

    # Hide a previous match list as soon as the user changes the variant text.
    observeEvent(
      input$variant,
      {
        resolved <- variant_matches()
        if (
          !is.null(resolved) &&
            !identical(trimws(input$variant %||% ""), resolved$term)
        ) {
          variant_matches(NULL)
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      list(input$gene_variant, input$assembly),
      {
        variant_matches(NULL)
      },
      ignoreInit = TRUE
    )
    # The one place a query is published. Gates every downstream API call on a
    # format-level check of the inputs, so a malformed gene/variant is caught
    # here rather than firing failing lookups across the cards.
    submit_query <- function(gene, variant, protvar = NULL, assembly = "AUTO") {
      check <- vr_validate_query(gene, if (variant == "") NULL else variant)
      if (!isTRUE(check$ok)) {
        validation(check$errors)
        query(NULL)
        return(invisible(FALSE))
      }
      validation(NULL)
      query(list(
        gene = gene,
        variant = if (variant == "") NULL else variant,
        protvar = protvar,
        assembly = assembly
      ))
      invisible(TRUE)
    }

    resolve_variant <- function(variant, gene = "") {
      assembly <- input$assembly %||% "AUTO"
      previous <- variant_matches()
      if (
        !is.null(previous) &&
          identical(previous$term, variant) &&
          identical(previous$requested_assembly, assembly)
      ) {
        selected <- trimws(input$variant_match %||% "")
        if (!nzchar(selected)) {
          validation("Select one of the matching variants.")
          query(NULL)
          return(invisible(FALSE))
        }
        mapping <- previous$mappings[
          previous$mappings$id == selected,
          ,
          drop = FALSE
        ]
        variant_matches(NULL)
        return(submit_query(
          gene,
          variant,
          protvar = protvar_mapping_record(mapping),
          assembly = previous$assembly %||% assembly
        ))
      }

      check <- vr_validate_variant(variant)
      if (!isTRUE(check$ok)) {
        validation(check$error)
        query(NULL)
        return(invisible(FALSE))
      }
      resolved <- protvar_find_mappings(variant, assembly = assembly)
      if (!isTRUE(resolved$ok)) {
        validation(resolved$error)
        query(NULL)
        return(invisible(FALSE))
      }
      if (nrow(resolved$mappings) == 1) {
        variant_matches(NULL)
        return(submit_query(
          gene,
          variant,
          protvar = protvar_mapping_record(resolved$mappings),
          assembly = resolved$assembly %||% assembly
        ))
      }
      resolved$term <- variant
      resolved$requested_assembly <- assembly
      variant_matches(resolved)
      validation(NULL)
      query(NULL)
      invisible(FALSE)
    }

    observeEvent(input$submit, {
      input_type <- input$input_type %||% "gene"
      if (identical(input_type, "variant")) {
        resolve_variant(trimws(input$variant %||% ""))
      } else {
        gene <- trimws(input$gene %||% "")
        variant <- trimws(input$gene_variant %||% "")
        if (nzchar(variant)) {
          resolve_variant(variant, gene)
        } else {
          submit_query(gene, "")
        }
      }
    })

    # An outside request (the assistant) fills the search box and then goes
    # through the same submit as a Review click, so it gets the same validation
    # and the user can see what was searched. `nonce` makes a repeat of the same
    # gene/variant a fresh event.
    observeEvent(requested(), {
      req <- requested()
      if (is.null(req)) {
        return()
      }
      gene <- trimws(as.character(req$gene %||% ""))
      variant <- trimws(as.character(req$variant %||% ""))
      input_type <- if (nzchar(variant)) "variant" else "gene"
      if (identical(input_type, "variant")) {
        gene <- ""
      }
      updateRadioButtons(session, "input_type", selected = input_type)
      updateTextInput(session, "gene", value = gene)
      updateSelectizeInput(
        session,
        "gene_variant",
        choices = character(),
        selected = "",
        server = FALSE
      )
      updateTextInput(
        session,
        "variant",
        value = variant
      )
      if (identical(input_type, "variant")) {
        resolve_variant(variant)
      } else {
        submit_query(gene, "")
      }
    })

    output$validation <- renderUI({
      msgs <- validation()
      if (is.null(msgs)) {
        return(NULL)
      }
      div(
        class = "alert alert-warning py-2 px-3 small mt-2 mb-0",
        role = "alert",
        lapply(msgs, tags$div)
      )
    })

    # Each pill fills the same variant-mode controls. The loop creates one
    # observer per example while keeping the submit boundary unchanged.
    for (example_id in names(.gene_search_examples)) {
      local({
        current_id <- example_id
        observeEvent(input[[paste0("example_", current_id)]], {
          example <- .gene_search_examples[[current_id]]
          updateRadioButtons(session, "input_type", selected = "variant")
          updateTextInput(session, "gene", value = "")
          updateTextInput(session, "variant", value = example$variant)
          validation(NULL)
          variant_matches(NULL)
        })
      })
    }

    query
  })
}
