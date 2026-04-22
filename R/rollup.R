# Internal ROLLUP aggregation engine
#
# Generates the set list for data.table::groupingsets(), builds the aggregation
# expression list, runs the aggregation, and attaches .rollup_depth metadata.

utils::globalVariables(c(".id", ".rollup_depth", ".", "J"))

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

#' Run ROLLUP aggregation on a data.table
#'
#' @param dt A `data.table`.
#' @param config A validated, normalised config list (from `parse_config()`).
#' @return A `data.table` with all ROLLUP groups, `.id` (grouping set index),
#'   and `.rollup_depth` (0 = grand total, max = leaf level).
#' @keywords internal
.build_rollup <- function(dt, config) {
  checkmate::assert_data_table(dt)

  hierarchy <- config$hierarchy
  measures  <- config$measures
  derived   <- config$derived

  sets   <- .rollup_sets(hierarchy)
  j_expr <- .build_agg_j(measures)

  result <- data.table::groupingsets(
    dt,
    jj   = j_expr,
    by   = hierarchy,
    sets = sets,
    id   = TRUE
  )

  # .rollup_depth: 0 = grand total, length(hierarchy) = finest/leaf
  result[, .rollup_depth := (length(hierarchy) + 1L) - .id]

  if (!is.null(derived)) {
    result <- .apply_derived(result, derived)
  }

  result
}
