# Filter application — creates named facets of the input data.table

# Allowed filter operators mapped to their R equivalents
.FILTER_OPS <- c(
  "=="  = "==",
  "!="  = "!=",
  ">"   = ">",
  ">="  = ">=",
  "<"   = "<",
  "<="  = "<=",
  "%in%" = "%in%"
)

#' Apply named filters to a data.table, returning a list of facets
#'
#' Always includes a `"full"` facet containing the unfiltered data.
#' Each additional entry corresponds to a named filter from the config.
#'
#' @param dt A `data.table` (already aggregated via `.build_rollup()`).
#' @param filters Named list of filter definitions, or `NULL` for no filters.
#' @return A named list of `data.table` objects: `"full"` always first, then
#'   one entry per filter.
#' @keywords internal
.apply_filters <- function(dt, filters) {
  checkmate::assert_data_table(dt)
  result <- list(full = dt)

  if (is.null(filters) || length(filters) == 0L) {
    return(result)
  }

  for (nm in names(filters)) {
    f           <- filters[[nm]]
    keep        <- .eval_filter(dt, f)
    result[[nm]] <- dt[keep]
  }

  result
}

#' Build a data.table filter expression from a single filter definition
#'
#' @param f A single filter list with elements `col`, `op`, and `val`.
#' @return An unevaluated R call suitable for use in `dt[expr]`.
#' @keywords internal
.build_filter_expr <- function(f) {
  checkmate::assert_string(f$col)
  checkmate::assert_choice(f$op, names(.FILTER_OPS))

  col_sym <- as.name(f$col)
  val     <- f$val

  if (f$op == "%in%") {
    if (!is.vector(val)) val <- list(val)
    val <- unlist(val)
    call("%in%", col_sym, val)
  } else {
    call(f$op, col_sym, val)
  }
}

#' Evaluate a single filter on a data.table, returning a logical index vector
#'
#' Uses direct column extraction (`dt[[col]]`) and `do.call()` to avoid NSE
#' issues with programmatically constructed filter expressions.
#'
#' @param dt A `data.table`.
#' @param f A single filter definition list with `col`, `op`, `val`.
#' @return A logical vector of length `nrow(dt)`.
#' @keywords internal
.eval_filter <- function(dt, f) {
  col_vec <- dt[[f$col]]
  val     <- f$val
  if (f$op == "%in%") {
    if (!is.vector(val)) val <- list(val)
    val <- unlist(val)
  }
  do.call(f$op, list(col_vec, val))
}

#' Apply filters to a raw (pre-aggregation) data.table to create raw facets,
#' then run ROLLUP aggregation on each facet independently
#'
#' Unlike `.apply_filters()` (which post-filters aggregated data), this
#' function pre-filters then re-aggregates — used internally by `volvelle()`
#' to ensure measure aggregations are correct within each filter scope.
#'
#' @param dt A raw `data.table` (the original input data).
#' @param config A validated config list.
#' @return A named list of ROLLUP-aggregated `data.table` objects.
#' @keywords internal
.build_all_facets <- function(dt, config) {
  filters  <- config$filters
  timed    <- !is.null(config$time_var)

  .rollup_and_pivot <- function(data) {
    res <- .build_rollup(data, config)
    if (timed) res <- .pivot_time(res, config)
    res
  }

  # Full facet (no pre-filter)
  result <- list(full = .rollup_and_pivot(dt))

  if (!is.null(filters) && length(filters) > 0L) {
    for (nm in names(filters)) {
      f    <- filters[[nm]]
      keep <- .eval_filter(dt, f)
      sub  <- dt[keep]

      if (nrow(sub) == 0L) {
        rlang::warn(
          sprintf("Filter '%s' produced 0 rows; facet will be empty.", nm),
          class = "volvelle_empty_facet"
        )
      }

      result[[nm]] <- .rollup_and_pivot(sub)
    }
  }

  result
}
