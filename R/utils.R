# Shared package-level helpers

#' Null-coalescing operator
#'
#' Returns `x` if non-NULL, otherwise `y`.
#' Re-exported from rlang for internal convenience.
#'
#' @param x,y Values to test and fall back to.
#' @return `x` if `!is.null(x)`, else `y`.
#' @keywords internal
`%||%` <- rlang::`%||%`

#' Coerce a data.frame or tibble to data.table
#'
#' @param x A data.frame, tibble, or data.table.
#' @return A `data.table`.
#' @keywords internal
.as_dt <- function(x) {
  if (!data.table::is.data.table(x)) {
    data.table::as.data.table(x)
  } else {
    data.table::copy(x)
  }
}

#' Validate that all hierarchy columns and measure columns exist in the data
#'
#' @param dt A `data.table`.
#' @param config A normalised config list.
#' @return Invisibly `TRUE`; throws `volvelle_data_error` if columns are missing.
#' @keywords internal
.check_columns <- function(dt, config) {
  needed_hier <- config$hierarchy
  needed_meas <- vapply(config$measures, `[[`, character(1L), "col")
  needed_wts  <- vapply(
    config$measures,
    function(m) if (!is.null(m$weight)) m$weight else NA_character_,
    character(1L)
  )
  needed_wts  <- needed_wts[!is.na(needed_wts)]

  all_needed <- unique(c(needed_hier, needed_meas, needed_wts))
  missing    <- setdiff(all_needed, names(dt))

  if (length(missing) > 0L) {
    rlang::abort(
      sprintf(
        "Columns required by config are missing from data: %s",
        paste(missing, collapse = ", ")
      ),
      class = "volvelle_data_error"
    )
  }

  # Check filter columns
  if (!is.null(config$filters)) {
    filter_cols <- vapply(config$filters, `[[`, character(1L), "col")
    missing_f   <- setdiff(filter_cols, names(dt))
    if (length(missing_f) > 0L) {
      rlang::abort(
        sprintf(
          "Filter column(s) missing from data: %s",
          paste(missing_f, collapse = ", ")
        ),
        class = "volvelle_data_error"
      )
    }
  }

  invisible(TRUE)
}

#' Get the display label for a measure or derived field
#'
#' Returns the `label` field if present, otherwise the field name.
#'
#' @param field_def A single measure or derived field definition list.
#' @param field_name The name of the field (used as fallback label).
#' @return A character string.
#' @keywords internal
.field_label <- function(field_def, field_name) {
  field_def$label %||% field_name
}
