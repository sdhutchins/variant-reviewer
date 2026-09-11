# Gene-or-variant search box. Returns a reactive carrying the submitted query so
# the parent can fan it out to the result modules.

# A coherent, well-supported example (BRAF V600E) that populates every card. Its
# genomic HGVS identifies the V600E allele precisely, without the other alleles
# sharing its rsID. Shared by the UI label and the server handler.
.gene_search_example <- list(
  label = "BRAF V600E",
  variant = "chr7:g.140453136A>T"
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
            textInput(
              ns("variant"),
              label = "Variant",
              placeholder = "e.g. rs113488022 or chr7:g.140453136A>T",
              width = "100%"
            ),
            uiOutput(ns("variant_match_ui"))
          )
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
        "Choose a gene symbol or enter a variant (rsID or HGVS)."
      ),
      # One-click example so a first-time visitor can see a populated dashboard
      # without knowing a gene/variant off-hand.
      tags$div(
        class = "small text-muted",
        "Not sure where to start? ",
        actionLink(
          ns("example"),
          paste0("Load an example (", .gene_search_example$label, ")")
        )
      ),
      # Inline validation feedback: shown when the gene/variant fails the
      # format check, in which case no search is submitted.
      uiOutput(ns("validation"))
    )
  )
}

# Returns a reactive carrying list(gene = <chr>, variant = <chr|NULL>), or NULL
# before the first submit (and when the gene is blank). A reactiveVal is used
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
      if (is.null(resolved) || nrow(resolved$matches) <= 1) {
        return(NULL)
      }
      tagList(
        selectInput(
          session$ns("variant_match"),
          "Select the matching variant",
          choices = c(
            "Choose a match" = "",
            stats::setNames(resolved$matches$id, resolved$matches$label)
          ),
          selected = "",
          width = "100%"
        ),
        tags$div(
          class = "small text-muted",
          "This entry matches more than one genomic allele."
        )
      )
    })

    # Hide a previous match list as soon as the user changes the variant text.
    observeEvent(input$variant, {
      resolved <- variant_matches()
      if (!is.null(resolved) &&
          !identical(trimws(input$variant %||% ""), resolved$term)) {
        variant_matches(NULL)
      }
    }, ignoreInit = TRUE)
    # The one place a query is published. Gates every downstream API call on a
    # format-level check of the inputs, so a malformed gene/variant is caught
    # here rather than firing failing lookups across the cards.
    submit_query <- function(gene, variant) {
      check <- vr_validate_query(gene, if (variant == "") NULL else variant)
      if (!isTRUE(check$ok)) {
        validation(check$errors)
        query(NULL)
        return(invisible(FALSE))
      }
      validation(NULL)
      query(list(
        gene = gene,
        variant = if (variant == "") NULL else variant
      ))
      invisible(TRUE)
    }

    resolve_variant <- function(variant) {
      previous <- variant_matches()
      if (!is.null(previous) && identical(previous$term, variant)) {
        selected <- trimws(input$variant_match %||% "")
        if (!nzchar(selected)) {
          validation("Select one of the matching variants.")
          query(NULL)
          return(invisible(FALSE))
        }
        updateTextInput(session, "variant", value = selected)
        variant_matches(NULL)
        return(submit_query("", selected))
      }

      check <- vr_validate_variant(variant)
      if (!isTRUE(check$ok)) {
        validation(check$error)
        query(NULL)
        return(invisible(FALSE))
      }
      resolved <- myvariant_find_matches(variant)
      if (!isTRUE(resolved$ok)) {
        validation(resolved$error)
        query(NULL)
        return(invisible(FALSE))
      }
      if (nrow(resolved$matches) == 1) {
        variant_matches(NULL)
        return(submit_query("", variant))
      }
      resolved$term <- variant
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
        submit_query(
          trimws(input$gene %||% ""),
          trimws(input$gene_variant %||% "")
        )
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

    # Fill the variant input with the example but leave submitting to the user,
    # so they can review or change it before clicking Review. Variant mode
    # matches the worked example loaded by the navbar Demo.
    observeEvent(input$example, {
      updateRadioButtons(session, "input_type", selected = "variant")
      updateTextInput(session, "gene", value = "")
      updateTextInput(session, "variant", value = .gene_search_example$variant)
      variant_matches(NULL)
    })

    query
  })
}
