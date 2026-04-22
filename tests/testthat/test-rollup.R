test_that(".rollup_sets() returns correct sets for length-1 hierarchy", {
  sets <- volvelle:::.rollup_sets("a")
  expect_length(sets, 2L)
  expect_equal(sets[[1]], "a")
  expect_equal(sets[[2]], character(0))
})

test_that(".rollup_sets() returns correct sets for length-2 hierarchy", {
  sets <- volvelle:::.rollup_sets(c("a", "b"))
  expect_length(sets, 3L)
  expect_equal(sets[[1]], c("a", "b"))
  expect_equal(sets[[2]], "a")
  expect_equal(sets[[3]], character(0))
})

test_that(".rollup_sets() returns correct sets for length-3 hierarchy", {
  sets <- volvelle:::.rollup_sets(c("a", "b", "c"))
  expect_length(sets, 4L)
  expect_equal(sets[[1]], c("a", "b", "c"))
  expect_equal(sets[[2]], c("a", "b"))
  expect_equal(sets[[3]], "a")
  expect_equal(sets[[4]], character(0))
})

test_that(".build_rollup() produces correct row count for simple input", {
  dt <- data.table::data.table(
    grp = c("A", "A", "B"),
    val = c(10, 20, 30)
  )
  cfg <- list(
    hierarchy = "grp",
    measures  = list(total = list(col = "val", fn = "sum")),
    derived   = NULL
  )
  result <- volvelle:::.build_rollup(dt, cfg)

  # Rows: 2 groups + 1 grand total = 3
  expect_equal(nrow(result), 3L)
  expect_true(".rollup_depth" %in% names(result))
  expect_true(".id" %in% names(result))
})

test_that(".build_rollup() grand total row has .rollup_depth = 0", {
  dt <- data.table::data.table(
    grp = c("A", "B"),
    val = c(10, 20)
  )
  cfg <- list(
    hierarchy = "grp",
    measures  = list(total = list(col = "val", fn = "sum")),
    derived   = NULL
  )
  result <- volvelle:::.build_rollup(dt, cfg)
  expect_true(any(result$.rollup_depth == 0L))
})

test_that(".build_rollup() grand total sum is correct", {
  dt <- data.table::data.table(
    grp = c("A", "A", "B"),
    val = c(10L, 20L, 30L)
  )
  cfg <- list(
    hierarchy = "grp",
    measures  = list(total = list(col = "val", fn = "sum")),
    derived   = NULL
  )
  result <- volvelle:::.build_rollup(dt, cfg)
  grand <- result[result$.rollup_depth == 0L, ]
  expect_equal(grand$total, 60)
})

test_that(".build_rollup() with 3-level hierarchy has correct depth range", {
  dt <- data.table::data.table(
    a = c("X", "X", "Y"),
    b = c("P", "Q", "P"),
    c = c("i", "ii", "iii"),
    v = c(1, 2, 3)
  )
  cfg <- list(
    hierarchy = c("a", "b", "c"),
    measures  = list(s = list(col = "v", fn = "sum")),
    derived   = NULL
  )
  result <- volvelle:::.build_rollup(dt, cfg)
  depths <- unique(result$.rollup_depth)
  expect_true(0L %in% depths)   # grand total
  expect_true(3L %in% depths)   # leaf level
})
