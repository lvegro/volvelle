test_that(".apply_derived() appends derived column", {
  dt <- data.table::data.table(a = c(2, 4), b = c(3, 5))
  derived <- list(ab_sum = list(expr = "a + b"))
  result  <- volvelle:::.apply_derived(dt, derived)
  expect_true("ab_sum" %in% names(result))
  expect_equal(result$ab_sum, c(5, 9))
})

test_that(".apply_derived() is numerically correct for product expression", {
  dt <- data.table::data.table(
    avg_pd  = c(0.05, 0.10),
    avg_lgd = c(0.40, 0.50),
    exposure = c(1000000, 2000000)
  )
  derived <- list(
    expected_loss = list(expr = "avg_pd * avg_lgd * exposure")
  )
  result <- volvelle:::.apply_derived(dt, derived)

  expect_true("expected_loss" %in% names(result))
  expect_equal(result$expected_loss[1], 0.05 * 0.40 * 1000000)
  expect_equal(result$expected_loss[2], 0.10 * 0.50 * 2000000)
})

test_that(".apply_derived() handles multiple derived fields", {
  dt <- data.table::data.table(x = c(10, 20), y = c(2, 4))
  derived <- list(
    ratio  = list(expr = "x / y"),
    double = list(expr = "x * 2")
  )
  result <- volvelle:::.apply_derived(dt, derived)
  expect_true("ratio"  %in% names(result))
  expect_true("double" %in% names(result))
  expect_equal(result$ratio,  c(5, 5))
  expect_equal(result$double, c(20, 40))
})

test_that(".apply_derived() errors on unparseable expression", {
  dt      <- data.table::data.table(x = 1:3)
  derived <- list(bad = list(expr = "((()))"))
  expect_error(volvelle:::.apply_derived(dt, derived), class = "volvelle_config_error")
})
