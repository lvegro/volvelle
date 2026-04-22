# ── Helpers ──────────────────────────────────────────────────────────────────

make_timed_dt <- function() {
  data.table::data.table(
    grp    = c("A", "A", "B", "B"),
    val    = c(10,  20,  30,  40),
    period = c("Q1", "Q2", "Q1", "Q2")
  )
}

make_timed_cfg <- function(derived_deltas = FALSE) {
  list(
    hierarchy = "grp",
    measures  = list(total = list(col = "val", fn = "sum")),
    derived   = NULL,
    filters   = NULL,
    time_var  = list(col = "period", periods = c("Q1", "Q2"),
                     derived_deltas = derived_deltas)
  )
}

# ── .rollup_sets_timed() ─────────────────────────────────────────────────────

test_that(".rollup_sets_timed() pins time_col in every set", {
  sets <- volvelle:::.rollup_sets_timed(c("a", "b"), "t")
  expect_length(sets, 3L)
  expect_true(all(vapply(sets, function(s) "t" %in% s, logical(1))))
  expect_equal(sets[[1L]], c("a", "b", "t"))
  expect_equal(sets[[2L]], c("a", "t"))
  expect_equal(sets[[3L]], c(character(0), "t"))
})

# ── .build_rollup() with time_var ────────────────────────────────────────────

test_that(".build_rollup() with time_var has period column in output", {
  dt  <- make_timed_dt()
  cfg <- make_timed_cfg()
  res <- volvelle:::.build_rollup(dt, cfg)
  expect_true("period" %in% names(res))
})

test_that(".build_rollup() with time_var has correct .rollup_depth scale", {
  dt  <- make_timed_dt()
  cfg <- make_timed_cfg()
  res <- volvelle:::.build_rollup(dt, cfg)
  # 1-level hierarchy: depths should be 0 and 1
  expect_setequal(unique(res$.rollup_depth), c(0L, 1L))
})

test_that(".build_rollup() with time_var has one grand-total row per period", {
  dt    <- make_timed_dt()
  cfg   <- make_timed_cfg()
  res   <- volvelle:::.build_rollup(dt, cfg)
  grand <- res[res$.rollup_depth == 0L, ]
  expect_equal(nrow(grand), 2L)  # one per period
  expect_setequal(grand$period, c("Q1", "Q2"))
})

# ── .pivot_time() ─────────────────────────────────────────────────────────────

test_that(".pivot_time() produces wide output with period-suffixed columns", {
  dt    <- make_timed_dt()
  cfg   <- make_timed_cfg()
  long  <- volvelle:::.build_rollup(dt, cfg)
  wide  <- volvelle:::.pivot_time(long, cfg)
  expect_true("total_Q1" %in% names(wide))
  expect_true("total_Q2" %in% names(wide))
  expect_false("period"  %in% names(wide))   # time col absorbed into columns
})

test_that(".pivot_time() grand total values are correct", {
  dt   <- make_timed_dt()
  cfg  <- make_timed_cfg()
  long <- volvelle:::.build_rollup(dt, cfg)
  wide <- volvelle:::.pivot_time(long, cfg)
  grand <- wide[wide$.rollup_depth == 0L, ]
  expect_equal(nrow(grand), 1L)
  expect_equal(grand$total_Q1, 40)  # A:10 + B:30
  expect_equal(grand$total_Q2, 60)  # A:20 + B:40
})

test_that(".pivot_time() group totals are correct", {
  dt   <- make_timed_dt()
  cfg  <- make_timed_cfg()
  long <- volvelle:::.build_rollup(dt, cfg)
  wide <- volvelle:::.pivot_time(long, cfg)
  grp  <- wide[wide$.rollup_depth == 1L & !is.na(wide$grp), ]
  a_q1 <- grp[grp$grp == "A", "total_Q1"][[1L]]
  b_q2 <- grp[grp$grp == "B", "total_Q2"][[1L]]
  expect_equal(a_q1, 10)
  expect_equal(b_q2, 40)
})

test_that(".pivot_time() appends delta columns when derived_deltas = TRUE", {
  dt   <- make_timed_dt()
  cfg  <- make_timed_cfg(derived_deltas = TRUE)
  long <- volvelle:::.build_rollup(dt, cfg)
  wide <- volvelle:::.pivot_time(long, cfg)
  expect_true("total_delta" %in% names(wide))
  grand <- wide[wide$.rollup_depth == 0L, ]
  expect_equal(grand$total_delta, 60 - 40)   # Q2 - Q1
})

test_that(".pivot_time() respects declared period order", {
  dt  <- make_timed_dt()
  cfg <- make_timed_cfg()
  cfg$time_var$periods <- c("Q2", "Q1")  # reversed
  long <- volvelle:::.build_rollup(dt, cfg)
  wide <- volvelle:::.pivot_time(long, cfg)
  expect_equal(attr(wide, "periods"), c("Q2", "Q1"))
})

test_that(".pivot_time() errors when declared periods absent from data", {
  dt  <- make_timed_dt()
  cfg <- make_timed_cfg()
  cfg$time_var$periods <- c("Q3", "Q4")
  long <- volvelle:::.build_rollup(dt, cfg)
  expect_error(volvelle:::.pivot_time(long, cfg), class = "volvelle_data_error")
})

test_that(".pivot_time() attaches 'periods' and 'value_cols' attributes", {
  dt   <- make_timed_dt()
  cfg  <- make_timed_cfg()
  long <- volvelle:::.build_rollup(dt, cfg)
  wide <- volvelle:::.pivot_time(long, cfg)
  expect_equal(attr(wide, "periods"),    c("Q1", "Q2"))
  expect_equal(attr(wide, "value_cols"), "total")
})

# ── validate_config() with time_var ──────────────────────────────────────────

test_that("validate_config() rejects time_var col in hierarchy", {
  raw <- list(
    hierarchy = list("a", "t"),
    measures  = list(m = list(col = "v", fn = "sum")),
    time_var  = list(col = "t")
  )
  expect_error(validate_config(raw), class = "volvelle_config_error")
})

test_that("validate_config() accepts valid time_var", {
  raw <- list(
    hierarchy = list("a"),
    measures  = list(m = list(col = "v", fn = "sum")),
    time_var  = list(col = "period", periods = list("Q1", "Q2"),
                     derived_deltas = TRUE)
  )
  expect_no_error(validate_config(raw))
})

# ── volvelle() end-to-end with time_var ──────────────────────────────────────

test_that("volvelle() with time config returns wide-format facets", {
  cfg_path <- system.file("extdata", "demo_time_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)

  expect_s3_class(result, "volvelle_result")
  expect_true("exposure_Q1-2024" %in% names(result$full))
  expect_true("exposure_Q2-2024" %in% names(result$full))
  expect_true("exposure_delta"   %in% names(result$full))
})

test_that("volvelle() with time config: grand total row per wide table", {
  cfg_path <- system.file("extdata", "demo_time_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  grand    <- result$full[result$full$.rollup_depth == 0L, ]
  expect_equal(nrow(grand), 1L)
})

test_that("volvelle_widget() renders without error for timed result", {
  cfg_path <- system.file("extdata", "demo_time_config.yaml", package = "volvelle")
  result   <- volvelle(credit_portfolio, cfg_path)
  expect_no_error(volvelle_widget(result, title = "Period Comparison"))
})
