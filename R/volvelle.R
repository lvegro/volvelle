#' Run ROLLUP aggregation across all filter facets
#'
#' Takes a data frame and a YAML config file (or pre-parsed list), runs a
#' SQL-style ROLLUP aggregation over the declared hierarchy, and returns one
#' aggregated data.table per filter facet (plus an unfiltered `"full"` facet).
#'
#' @param data A `data.frame`, `tibble`, or `data.table`.
#' @param config Path to a YAML config file, or a pre-parsed named list.
#' @param extra_filters Optional named list of additional filter definitions
#'   (same structure as the `filters` block in the YAML) to merge with any
#'   filters declared in the config. Names must not clash with config filters.
#' @return A `volvelle_result` S3 object: a named list of `data.table` objects,
#'   one per facet (`"full"` plus one per named filter). Each table contains
#'   ROLLUP aggregations plus `.id` and `.rollup_depth` metadata columns.
#'   Attribute `config` holds the parsed, validated config.
#' @export
#' @examples
#' cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
#' result   <- volvelle(credit_portfolio, cfg_path)
#' names(result)
#' result$full[.rollup_depth == 0]  # grand total row
volvelle <- function(data, config, extra_filters = NULL) {
  cfg <- parse_config(config)

  # Merge extra_filters into config
  if (!is.null(extra_filters)) {
    checkmate::assert_list(extra_filters, names = "named")
    clash <- intersect(names(extra_filters), names(cfg$filters))
    if (length(clash) > 0L) {
      rlang::abort(
        sprintf(
          "extra_filters names clash with config filters: %s",
          paste(clash, collapse = ", ")
        ),
        class = "volvelle_config_error"
      )
    }
    cfg$filters <- c(cfg$filters, extra_filters)
  }

  dt <- .as_dt(data)
  .check_columns(dt, cfg)

  facets <- .build_all_facets(dt, cfg)

  structure(
    facets,
    class  = c("volvelle_result", "list"),
    config = cfg
  )
}

# ---------------------------------------------------------------------------
# S3 methods
# ---------------------------------------------------------------------------

#' Print a `volvelle_result` object
#'
#' @param x A `volvelle_result` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.volvelle_result <- function(x, ...) {
  cfg    <- attr(x, "config")
  facets <- names(x)

  cat(
    sprintf(
      "<volvelle_result>\n  Hierarchy : %s\n  Measures  : %s\n  Facets    : %s\n",
      paste(cfg$hierarchy, collapse = " > "),
      paste(names(cfg$measures), collapse = ", "),
      paste(facets, collapse = ", ")
    )
  )

  if (!is.null(cfg$derived)) {
    cat(sprintf("  Derived   : %s\n", paste(names(cfg$derived), collapse = ", ")))
  }

  for (nm in facets) {
    cat(sprintf(
      "  [%s] %d rows, %d rollup levels\n",
      nm, nrow(x[[nm]]), length(cfg$hierarchy) + 1L
    ))
  }

  invisible(x)
}

#' Convert a `volvelle_result` facet to a plain data.frame
#'
#' @param x A `volvelle_result` object.
#' @param facet Name of the facet to extract; defaults to `"full"`.
#' @param ... Ignored.
#' @return A `data.frame`.
#' @export
as.data.frame.volvelle_result <- function(x, facet = "full", ...) {
  if (!facet %in% names(x)) {
    rlang::abort(
      sprintf("Facet '%s' not found. Available: %s", facet, paste(names(x), collapse = ", ")),
      class = "volvelle_data_error"
    )
  }
  as.data.frame(x[[facet]])
}

#' List available facets in a `volvelle_result`
#'
#' @param x A `volvelle_result` object.
#' @return A character vector of facet names.
#' @export
facets <- function(x) {
  UseMethod("facets")
}

#' @export
facets.volvelle_result <- function(x) {
  names(x)
}
