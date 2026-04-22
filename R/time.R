# Post-ROLLUP time-pivot: reshape long (one row per hierarchy group × period)
# to wide (one row per hierarchy group, one column group per period).

utils::globalVariables(c("..value_cols", "..id_cols"))

#' Pivot a time-aware ROLLUP result from long to wide format
#'
#' Requires that `config$time_var` is non-NULL. After a ROLLUP that includes
#' the time column in every grouping set, each hierarchy row appears once per
#' period. This function `dcast`s those rows so each period becomes its own
#' column group (e.g. `exposure_Q1`, `exposure_Q2`).
#'
#' When `config$time_var$derived_deltas` is `TRUE` and exactly two periods
#' are present, a `<measure>_delta` column (period2 - period1) is appended
#' for every measure and derived field.
#'
#' Period order follows `config$time_var$periods` if declared; otherwise
#' ascending sort of the unique values found in the data.
#'
#' @param dt A long-format `data.table` from `.build_rollup()` with a time
#'   column (the value of `config$time_var$col`).
#' @param config A normalised config list with `time_var` set.
#' @return A wide `data.table`. Attributes `"periods"` and `"value_cols"` are
#'   set for use by the widget renderer.
#' @keywords internal
.pivot_time <- function(dt, config) {
  tv       <- config$time_var
  time_col <- tv$col

  # Resolve and validate periods present in this facet's data
  found_periods <- unique(dt[[time_col]])
  found_periods <- found_periods[!is.na(found_periods)]

  if (!is.null(tv$periods)) {
    periods <- intersect(tv$periods, found_periods)
    if (length(periods) == 0L) {
      rlang::abort(
        sprintf(
          "time_var: none of the declared periods (%s) were found in the data.",
          paste(tv$periods, collapse = ", ")
        ),
        class = "volvelle_data_error"
      )
    }
    # Drop rows for undeclared periods (kept data clean)
    keep <- dt[[time_col]] %in% periods
    dt   <- dt[keep]
  } else {
    periods <- sort(found_periods)
  }

  measure_nms <- names(config$measures)
  derived_nms <- if (!is.null(config$derived)) names(config$derived) else character(0)
  value_cols  <- c(measure_nms, derived_nms)

  id_cols <- c(config$hierarchy, ".id", ".rollup_depth")

  # dcast: long -> wide
  # Produces columns named <value_col>_<period> (sep = "_")
  lhs     <- paste(id_cols, collapse = " + ")
  formula <- stats::as.formula(paste(lhs, "~", time_col))

  wide <- data.table::dcast(
    dt,
    formula   = formula,
    value.var = value_cols,
    sep       = "_"
  )

  # data.table dcast names value columns as <period>_<value> when multiple
  # value.var are supplied (pre-1.15) or <value>_<period> (post-1.15).
  # Normalise to the <value>_<period> convention.
  wide <- .normalise_dcast_names(wide, value_cols, periods)

  # Append delta columns when requested and exactly two periods
  if (isTRUE(tv$derived_deltas) && length(periods) == 2L) {
    p1 <- periods[[1L]]
    p2 <- periods[[2L]]
    for (vc in value_cols) {
      col1 <- paste0(vc, "_", p1)
      col2 <- paste0(vc, "_", p2)
      if (col1 %in% names(wide) && col2 %in% names(wide)) {
        delta_nm <- paste0(vc, "_delta")
        wide[, (delta_nm) := get(col2) - get(col1)]
      }
    }
  }

  # Attach metadata consumed by the widget
  data.table::setattr(wide, "periods",    periods)
  data.table::setattr(wide, "value_cols", value_cols)

  wide
}

# data.table >= 1.15.0 names dcast columns as <value>_<period>;
# earlier versions used <period>_<value>. Detect and normalise.
.normalise_dcast_names <- function(wide, value_cols, periods) {
  nms <- names(wide)

  # Check which convention is in use by looking for the first expected name
  expected_new <- paste0(value_cols[[1L]], "_", periods[[1L]])
  if (expected_new %in% nms) {
    return(wide)   # already <value>_<period>, nothing to do
  }

  # Try <period>_<value> (old convention) and rename
  renames <- character(0)
  for (vc in value_cols) {
    for (p in periods) {
      old_nm <- paste0(p, "_", vc)
      new_nm <- paste0(vc, "_", p)
      if (old_nm %in% nms) {
        renames[[old_nm]] <- new_nm
      }
    }
  }

  if (length(renames) > 0L) {
    data.table::setnames(wide, names(renames), unname(renames))
  }

  wide
}
