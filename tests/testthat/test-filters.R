test_that(".apply_filters() always includes 'full'", {
  dt <- data.table::data.table(x = 1:5, v = letters[1:5])
  result <- volvelle:::.apply_filters(dt, NULL)
  expect_named(result, "full")
  expect_equal(nrow(result$full), 5L)
})

test_that(".apply_filters() creates additional facets for each filter", {
  dt <- data.table::data.table(
    x   = c(1, 2, 3, 4, 5),
    grp = c("a", "a", "b", "b", "b")
  )
  filters <- list(
    group_a = list(col = "grp", op = "==", val = "a"),
    group_b = list(col = "grp", op = "==", val = "b")
  )
  result <- volvelle:::.apply_filters(dt, filters)
  expect_named(result, c("full", "group_a", "group_b"))
  expect_equal(nrow(result$group_a), 2L)
  expect_equal(nrow(result$group_b), 3L)
})

test_that(".apply_filters() filtered facet has fewer rows than full", {
  dt <- data.table::data.table(val = 1:10, flag = rep(c(TRUE, FALSE), 5))
  filters <- list(trues = list(col = "flag", op = "==", val = TRUE))
  result  <- volvelle:::.apply_filters(dt, filters)
  expect_lt(nrow(result$trues), nrow(result$full))
})

test_that(".eval_filter() %in% operator works", {
  dt <- data.table::data.table(
    cat = c("A", "B", "C", "D"),
    v   = 1:4
  )
  f    <- list(col = "cat", op = "%in%", val = list("A", "C"))
  keep <- volvelle:::.eval_filter(dt, f)
  sub  <- dt[keep]
  expect_equal(nrow(sub), 2L)
  expect_equal(sort(sub$cat), c("A", "C"))
})

test_that(".eval_filter() >= operator works", {
  dt <- data.table::data.table(x = c(1, 5, 10, 20))
  f  <- list(col = "x", op = ">=", val = 10)
  keep <- volvelle:::.eval_filter(dt, f)
  sub  <- dt[keep]
  expect_equal(nrow(sub), 2L)
})

test_that(".eval_filter() != operator works", {
  dt <- data.table::data.table(grp = c("a", "b", "a"))
  f  <- list(col = "grp", op = "!=", val = "b")
  keep <- volvelle:::.eval_filter(dt, f)
  sub  <- dt[keep]
  expect_equal(nrow(sub), 2L)
})

test_that(".apply_filters() handles empty filter list", {
  dt <- data.table::data.table(x = 1:3)
  result <- volvelle:::.apply_filters(dt, list())
  expect_named(result, "full")
})
