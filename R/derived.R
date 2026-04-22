utils::globalVariables(".SD")

# Post-aggregation derived field computation
#
# Derived fields are evaluated as R expressions using vectorized column
# arithmetic over the aggregated data.table. They are display-only and
# never re-aggregated.

#' Compute derived fields on an already-aggregated data.table
#'
#' Each derived field's `expr` string is parsed as an R expression and
#' evaluated against the data.table's columns (via `.SD`), producing a
#' new column appended in-place via `:=`.
#'
#' @param dt An aggregated `data.table` (output of `.build_rollup()`).
#' @param derived Named list of derived field definitions from the config.
#' @return The same `data.table` with derived columns appended in-place.
#' @keywords internal
.apply_derived <- function(dt, derived) {
  checkmate::assert_data_table(dt)
  checkmate::assert_list(derived, names = "named")

  for (nm in names(derived)) {
    d <- derived[[nm]]

    parsed <- tryCatch(
      parse(text = d$expr, keep.source = FALSE)[[1L]],
      error = function(e) {
        rlang::abort(
          sprintf("Derived field '%s': failed to parse expr '%s': %s", nm, d$expr, e$message),
          class = "volvelle_config_error"
        )
      }
    )

    col_name <- nm
    dt[, (col_name) := eval(parsed, envir = .SD)]
  }

  dt
}
