# End-to-end smoke test: launch the real app in a headless browser.
# Skipped automatically when no Chrome/Chromium is available.

test_that("app launches with the search controls present", {
  testthat::skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    app_dir = test_path("..", ".."),
    name = "app-smoke",
    height = 900,
    width = 1200
  )
  withr::defer(app$stop())

  # Both mode-specific input regions mount, with CSS showing the selected one.
  expect_no_error(app$get_value(input = "search-input_type"))
  expect_no_error(app$get_value(input = "search-gene"))
  expect_no_error(app$get_value(input = "search-gene_variant"))
  expect_no_error(app$get_value(input = "search-variant"))

  # The assistant starts hidden, opens from the floating button, and keeps its
  # mounted input state when closed and reopened.
  expect_false(app$get_js(
    "document.getElementById('tour_chat').classList.contains('show')"
  ))
  provider <- app$get_value(input = "chat-provider")
  app$get_js("document.getElementById('chat_toggle').click()")
  app$wait_for_js(
    "document.getElementById('tour_chat').classList.contains('show')"
  )
  expect_true(app$get_js(
    "document.getElementById('tour_chat').classList.contains('show')"
  ))
  app$get_js("document.getElementById('chat_toggle').click()")
  app$wait_for_js(
    paste(
      "!document.getElementById('tour_chat').classList.contains('show') &&",
      "!document.getElementById('tour_chat').classList.contains('collapsing')"
    )
  )
  app$get_js("document.getElementById('chat_toggle').click()")
  app$wait_for_js(
    "document.getElementById('tour_chat').classList.contains('show')"
  )
  expect_identical(app$get_value(input = "chat-provider"), provider)
})

test_that("searching a gene populates the gene summary card", {
  testthat::skip_if_not_installed("shinytest2")
  # Hits live APIs; skip on CI to keep the pipeline deterministic.
  testthat::skip_on_ci()

  app <- shinytest2::AppDriver$new(
    app_dir = test_path("..", ".."),
    name = "app-gene-search",
    height = 900,
    width = 1200
  )
  withr::defer(app$stop())

  app$set_inputs(`search-gene` = "TP53")
  app$click("search-submit")
  app$wait_for_idle(timeout = 30000)

  html <- app$get_html("#gene_summary-content")
  expect_match(html, "TP53")
})
