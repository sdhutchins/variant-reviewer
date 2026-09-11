# Small presentation helpers shared by the result modules, so every card
# renders fields, empty states, and errors the same way.

# Null-coalescing operator (base R has this from 4.4.0; defined for older R).
`%||%` <- function(x, y) if (is.null(x)) y else x # nolint: object_name_linter.

# App palette (organic, mirrors the color palette in _brand.yml). Used where a
# color must be set in R (e.g. ggplot geoms) rather than via the CSS theme.
vr_colors <- list(
  primary = "#4c7a5b", # sage/forest green
  accent = "#c07a52", # warm clay
  accent_text = "#8f4f2d", # darker clay for small text and white-label fills
  stone = "#7a7468" # muted stone/taupe
)

# A "Label: value" row. Hides the row entirely when the value is blank.
vr_field <- function(label, value) {
  if (is_blank(value)) {
    return(NULL)
  }
  tags$p(
    class = "mb-2",
    tags$strong(paste0(label, ": ")),
    tags$span(as.character(value))
  )
}

# Neutral placeholder shown before a search or when a section has no data.
vr_empty <- function(message) {
  tags$p(class = "text-muted fst-italic mb-0", message)
}

# Distinguish a valid no-result response from an API or parsing failure. Several
# providers report both through the same `ok = FALSE` contract, so classify only
# explicit no-result wording and leave missing-input or service errors alone.
vr_is_empty_result <- function(message) {
  patterns <- c(
    "^No .* found[.]?$",
    "^No pharmacogenomics annotations",
    " found no ",
    " returned no ",
    " has no matching "
  )
  any(vapply(
    patterns,
    function(pattern) grepl(pattern, message, ignore.case = TRUE),
    logical(1)
  ))
}

vr_empty_result <- function(message) {
  tags$div(class = "vr-empty-result", role = "status", message)
}

# Error/warning state for failed API calls. Valid empty results use a neutral
# treatment so absence of an annotation does not look like a service failure.
vr_error <- function(message) {
  if (vr_is_empty_result(message)) {
    return(vr_empty_result(message))
  }
  tags$div(
    class = "alert alert-warning mb-0 py-2 px-3",
    role = "alert",
    message
  )
}

# Format a number for display, returning a dash for missing values.
vr_num <- function(x, digits = 2) {
  if (is_blank(x) || is.na(suppressWarnings(as.numeric(x)))) {
    return("—")
  }
  formatC(as.numeric(x), format = "f", digits = digits, drop0trailing = TRUE)
}

# A compact CSV control for the far-right side of a table card header.
vr_csv_button <- function(id, description) {
  downloadButton(
    id,
    "CSV",
    class = paste(
      "btn-sm btn-outline-secondary vr-header-action",
      "vr-csv-download"
    ),
    title = description,
    `aria-label` = description
  )
}

# A compact labeled action for copying the nearest card body's rendered text.
# Separate icons let JavaScript show clear success or failure feedback without
# rebuilding the control or shifting the surrounding header layout.
vr_copy_button <- function(description) {
  tagList(
    tags$button(
      type = "button",
      class = paste(
        "btn btn-sm btn-outline-secondary vr-header-action",
        "vr-card-copy"
      ),
      title = description,
      `aria-label` = description,
      `data-copy-label` = description,
      onclick = "vrCopyCardText(this)",
      tags$span(class = "vr-copy-icon-default", icon("copy")),
      tags$span(class = "vr-copy-icon-success d-none", icon("check")),
      tags$span(
        class = "vr-copy-icon-error d-none",
        icon("triangle-exclamation")
      ),
      tags$span(class = "vr-copy-label", "Copy")
    ),
    tags$span(
      class = "visually-hidden vr-copy-status",
      `aria-live` = "polite"
    )
  )
}

# Standard shell for result cards whose body is rendered through `content`.
# Keep unusual layouts, such as the AlphaFold viewer, explicit in their module.
vr_result_card <- function(
  id,
  title,
  proxy_height = "160px",
  copy = FALSE,
  download = FALSE
) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header(title, ns, copy = copy, download = download),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = proxy_height
    ))
  )
}

# Apply the shared unloaded/error states before rendering card-specific content.
vr_result_ui <- function(result, empty_message, content) {
  if (is.null(result)) {
    return(vr_empty(empty_message))
  }
  if (!isTRUE(result$ok)) {
    return(vr_error(result$error))
  }
  content(result)
}

# Shared reactable behavior. Individual modules still own their columns,
# details, row styles, and any intentionally different page size.
vr_reactable <- function(
  data,
  ...,
  page_size = 10L,
  searchable = TRUE
) {
  reactable::reactable(
    data,
    searchable = searchable,
    compact = TRUE,
    highlight = TRUE,
    defaultPageSize = page_size,
    showPageSizeOptions = TRUE,
    ...
  )
}

# Render external API labels as escaped link HTML for reactable columns that
# opt into `html = TRUE`. Plain fallback labels are escaped for the same reason.
vr_external_link_html <- function(href, label) {
  label <- if (is_blank(label)) "" else as.character(label)[[1]]
  if (is_blank(href)) {
    return(htmltools::htmlEscape(label))
  }
  as.character(tags$a(
    href = href,
    target = "_blank",
    rel = "noopener noreferrer",
    label
  ))
}

# Register one consistent server-side CSV export. Server-side writing includes
# every row loaded into a card rather than only the current pagination page.
vr_csv_download <- function(output, id, data, filename, ns = NULL) {
  output[[id]] <- downloadHandler(
    filename = function() filename,
    content = function(file) {
      export_data <- data()
      req(is.data.frame(export_data))
      utils::write.csv(export_data, file, row.names = FALSE, na = "")
    }
  )

  if (!is.null(ns)) {
    output[[paste0(id, "_control")]] <- renderUI({
      export_data <- data()
      req(is.data.frame(export_data), nrow(export_data) > 0)
      vr_csv_button(ns(id), "Download table as CSV")
    })
  }
}

# Register the conventional `download` output for a card result reactive.
# `extract` keeps source-specific column selection in the owning module.
vr_result_csv <- function(
  output,
  result,
  filename,
  extract = function(value) value$data,
  ns
) {
  vr_csv_download(
    output,
    "download",
    reactive({
      value <- result()
      req(value, isTRUE(value$ok))
      export_data <- extract(value)
      req(is.data.frame(export_data))
      export_data
    }),
    filename,
    ns = ns
  )
}
