test_that("volvelle_widget() returns an htmlwidget-compatible object", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  w        <- volvelle_widget(result)
  # browsable htmltools output is a shiny.tag or tagList
  expect_true(inherits(w, c("shiny.tag", "shiny.tag.list", "html", "htmltools_tagged")))
})

test_that("volvelle_widget() light theme renders without error", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_no_error(volvelle_widget(result, theme = "light"))
})

test_that("volvelle_widget() dark theme renders without error", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_no_error(volvelle_widget(result, theme = "dark"))
})

test_that("volvelle_widget() accepts optional title", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_no_error(volvelle_widget(result, title = "My Portfolio"))
})

test_that("volvelle_widget() errors on wrong input type", {
  expect_error(volvelle_widget(list(a = 1)), class = "volvelle_data_error")
})

test_that("volvelle_widget() errors on invalid theme", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_error(volvelle_widget(result, theme = "blue"), class = "error")
})
