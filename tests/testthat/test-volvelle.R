test_that("volvelle() returns a volvelle_result", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_s3_class(result, "volvelle_result")
})

test_that("volvelle() result has correct facet names", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_true("full" %in% names(result))
  expect_true("performing" %in% names(result))
  expect_true("non_performing" %in% names(result))
  expect_true("large_exposure" %in% names(result))
})

test_that("volvelle() 'full' facet contains grand total row", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  grand    <- result$full[result$full$.rollup_depth == 0L, ]
  expect_equal(nrow(grand), 1L)
})

test_that("volvelle() measure sums match manual computation", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  grand    <- result$full[result$full$.rollup_depth == 0L, ]
  expected <- sum(credit_portfolio$nominal_exposure)
  expect_equal(grand$exposure, expected)
})

test_that("volvelle() 'performing' facet has fewer rows than 'full'", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_lt(nrow(result$performing), nrow(result$full))
})

test_that("volvelle() config attribute is accessible", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  cfg      <- attr(result, "config")
  expect_equal(cfg$hierarchy, c("segment", "sub_segment", "counterparty"))
})

test_that("volvelle() derived field 'expected_loss' is present", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_true("expected_loss" %in% names(result$full))
})

test_that("volvelle() errors on missing column", {
  cfg <- list(
    hierarchy = list("segment"),
    measures  = list(x = list(col = "nonexistent_col", fn = "sum"))
  )
  expect_error(volvelle(credit_portfolio, cfg), class = "volvelle_data_error")
})

test_that("volvelle() accepts extra_filters", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  extra <- list(
    watch_only = list(col = "status", op = "==", val = "watch")
  )
  result <- volvelle(credit_portfolio, cfg_path, extra_filters = extra)
  expect_true("watch_only" %in% names(result))
})

test_that("facets() returns correct names", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  f        <- facets(result)
  expect_true("full" %in% f)
  expect_true(is.character(f))
})

test_that("as.data.frame() extracts a facet as data.frame", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  df       <- as.data.frame(result, facet = "full")
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "data.table"))
})

test_that("print.volvelle_result() produces output", {
  cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_output(print(result), "volvelle_result")
})
