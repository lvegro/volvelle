test_that("parse_config() accepts a valid YAML path", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  cfg <- parse_config(cfg_path)

  expect_named(cfg, c("hierarchy", "measures", "derived", "filters"))
  expect_equal(cfg$hierarchy, c("segment", "sub_segment", "counterparty"))
  expect_true("exposure" %in% names(cfg$measures))
  expect_true("performing" %in% names(cfg$filters))
  expect_true("expected_loss" %in% names(cfg$derived))
})

test_that("parse_config() accepts a pre-parsed list", {
  raw <- list(
    hierarchy = list("a", "b"),
    measures  = list(
      x = list(col = "x", fn = "sum")
    )
  )
  cfg <- parse_config(raw)
  expect_equal(cfg$hierarchy, c("a", "b"))
  expect_null(cfg$filters)
})

test_that("parse_config() errors on non-existent file", {
  expect_error(
    parse_config("/nonexistent/file.yaml"),
    class = "error"
  )
})

test_that("parse_config() errors on invalid config type", {
  expect_error(
    parse_config(42),
    class = "volvelle_config_error"
  )
})

test_that("validate_config() rejects unsupported fn", {
  raw <- list(
    hierarchy = list("a"),
    measures  = list(
      bad = list(col = "a", fn = "variance")
    )
  )
  expect_error(validate_config(raw), class = "volvelle_config_error")
})

test_that("validate_config() rejects weighted_mean without weight", {
  raw <- list(
    hierarchy = list("a"),
    measures  = list(
      m = list(col = "v", fn = "weighted_mean")
    )
  )
  expect_error(validate_config(raw), class = "volvelle_config_error")
})

test_that("validate_config() rejects missing hierarchy", {
  raw <- list(
    measures = list(x = list(col = "x", fn = "sum"))
  )
  expect_error(validate_config(raw), class = "volvelle_config_error")
})

test_that("validate_config() rejects missing measures", {
  raw <- list(
    hierarchy = list("a")
  )
  expect_error(validate_config(raw), class = "volvelle_config_error")
})

test_that("validate_config() accepts weighted_mean with weight", {
  raw <- list(
    hierarchy = list("a"),
    measures  = list(
      wm = list(col = "v", fn = "weighted_mean", weight = "w")
    )
  )
  expect_no_error(validate_config(raw))
})

test_that("validate_config() accepts all supported fn values", {
  fns <- c("sum", "mean", "n", "n_distinct", "min", "max")
  for (fn in fns) {
    raw <- list(
      hierarchy = list("a"),
      measures  = list(m = list(col = "x", fn = fn))
    )
    expect_no_error(validate_config(raw), label = paste("fn =", fn))
  }
})
