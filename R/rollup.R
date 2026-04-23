# Internal ROLLUP aggregation engine
#
# Generates the set list for data.table::groupingsets(), builds the aggregation
# expression list, runs the aggregation, and attaches .rollup_depth metadata.

utils::globalVariables(c(".id", ".rollup_depth", ".", "J", ".N"))

# ---------------------------------------------------------------------------
# Set generation
# ---------------------------------------------------------------------------

#' Generate the ROLLUP grouping sets for a column hierarchy
#'
#' Returns the explicit list of grouping sets that corresponds to a standard
#' SQL ROLLUP over the supplied hierarchy — finest level first, grand total last.
#'
#' @param hierarchy Character vector of column names, outermost level first.
#' @return A list of character vectors representing each grouping set.
#' @keywords internal
.rollup_sets <- function(hierarchy) {
  checkmate::assert_character(hierarchy, min.len = 1, any.missing = FALSE)
  n <- length(hierarchy)
  sets <- vector("list", n + 1L)
  for (i in seq_len(n)) {
    sets[[i]] <- hierarchy[seq_len(n - i + 1L)]
  }
  sets[[n + 1L]] <- character(0)
  sets
}

# ---------------------------------------------------------------------------
# Aggregation expression builder
# ---------------------------------------------------------------------------

#' Build the data.table j expression list for the configured measures
#'
#' Translates each measure definition into a data.table aggregation call.
#' The result is passed as the `j` argument to `data.table::groupingsets()`.
#'
#' @param measures Named list of measure definitions from the parsed config.
#' @return A named list of unevaluated R calls.
#' @keywords internal
.build_agg_j <- function(measures) {
  exprs <- vector("list", length(measures))
  names(exprs) <- names(measures)

  for (nm in names(measures)) {
    m   <- measures[[nm]]
    col <- m$col
    fn  <- m$fn

    exprs[[nm]] <- switch(fn,
      sum          = call("sum",  as.name(col), na.rm = TRUE),
      mean         = call("mean", as.name(col), na.rm = TRUE),
      weighted_mean = .weighted_mean_call(col, m$weight),
      n            = quote(.N),
      n_distinct   = call("uniqueN", as.name(col)),
      min          = call("min",  as.name(col), na.rm = TRUE),
      max          = call("max",  as.name(col), na.rm = TRUE),
      rlang::abort(
        sprintf("Unsupported fn: '%s'", fn),
        class = "volvelle_config_error"
      )
    )
  }

  as.call(c(list(quote(list)), exprs))
}

.weighted_mean_call <- function(col, weight) {
  # sum(col * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
  num   <- call("sum", call("*", as.name(col), as.name(weight)), na.rm = TRUE)
  denom <- call("sum", as.name(weight), na.rm = TRUE)
  call("/", num, denom)
}

# ---------------------------------------------------------------------------
# Core aggregation
# ---------------------------------------------------------------------------

#' Generate ROLLUP grouping sets that pin a time column in every set
#'
#' Like `.rollup_sets()` but appends `time_col` to each set so the time
#' dimension is always present and never rolled up over.
#'
#' @param hierarchy Character vector of hierarchy column names.
#' @param time_col  Single column name for the time/period dimension.
#' @return A list of character vectors, each ending with `time_col`.
#' @keywords internal
.rollup_sets_timed <- function(hierarchy, time_col) {
  lapply(.rollup_sets(hierarchy), function(s) c(s, time_col))
}

#' Run ROLLUP aggregation on a data.table
#'
#' When `config$time_var` is set the time column is pinned into every
#' grouping set — it is never aggregated over — and `.rollup_depth` is
#' computed ignoring the time column so the scale matches the non-timed case.
#'
#' @param dt A `data.table`.
#' @param config A validated, normalised config list (from `parse_config()`).
#' @return A `data.table` with all ROLLUP groups, `.id` (grouping set index),
#'   and `.rollup_depth` (0 = grand total, max = leaf level). When
#'   `time_var` is configured, a column for the period is also present
#'   (pivoting to wide format happens separately in `.pivot_time()`).
#' @keywords internal
.build_rollup <- function(dt, config) {
  checkmate::assert_data_table(dt)

  hierarchy <- config$hierarchy
  measures  <- config$measures
  derived   <- config$derived
  time_var  <- config$time_var

  j_expr <- .build_agg_j(measures)

  if (!is.null(time_var)) {
    time_col <- time_var$col
    sets     <- .rollup_sets_timed(hierarchy, time_col)
    by_cols  <- c(hierarchy, time_col)
  } else {
    sets    <- .rollup_sets(hierarchy)
    by_cols <- hierarchy
  }

  result <- data.table::groupingsets(
    dt,
    jj   = j_expr,
    by   = by_cols,
    sets = sets,
    id   = TRUE
  )

  # .rollup_depth: 0 = grand total, length(hierarchy) = finest/leaf.
  # The time column doesn't count as a hierarchy level.
  # Extract .id to a plain variable first — dot-prefixed names are ambiguous
  # inside data.table's j NSE and may not resolve as column references.
  id_vec <- result[[".id"]]
  result[, .rollup_depth := (length(hierarchy) + 1L) - id_vec]

  if (!is.null(derived)) {
    result <- .apply_derived(result, derived)
  }

  result
}
