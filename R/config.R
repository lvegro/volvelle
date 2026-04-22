#' Parse and validate a volvelle configuration
#'
#' Reads a YAML config file (or accepts a pre-parsed list) and validates it
#' against the bundled JSON Schema. Returns a structured list suitable for use
#' by the aggregation engine.
#'
#' @param config Path to a YAML file or a named R list already parsed from YAML.
#' @return A validated, normalised config list with elements `hierarchy`,
#'   `measures`, `derived` (may be `NULL`), and `filters` (may be `NULL`).
#' @export
#' @examples
#' cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
#' cfg <- parse_config(cfg_path)
#' names(cfg)
parse_config <- function(config) {
  if (is.character(config)) {
    checkmate::assert_file_exists(config, extension = "yaml")
    raw <- yaml::read_yaml(config)
  } else if (is.list(config)) {
    raw <- config
  } else {
    rlang::abort(
      "config must be a path to a YAML file or a named list",
      class = "volvelle_config_error"
    )
  }

  validate_config(raw)
  .normalise_config(raw)
}

#' Validate a parsed config list against the JSON Schema
#'
#' @param raw A named list, typically from `yaml::read_yaml()`.
#' @return Invisibly returns `raw` on success; throws a `volvelle_config_error`
#'   on validation failure.
#' @export
#' @examples
#' cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
#' raw <- yaml::read_yaml(cfg_path)
#' validate_config(raw)
validate_config <- function(raw) {
  schema_path <- system.file("schema", "volvelle_schema.json", package = "volvelle")
  if (!nzchar(schema_path)) {
    rlang::abort("Bundled JSON Schema not found", class = "volvelle_config_error")
  }

  json_raw <- jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null")
  result   <- jsonvalidate::json_validate(
    json      = json_raw,
    schema    = schema_path,
    verbose   = TRUE,
    greedy    = TRUE
  )

  if (!isTRUE(result)) {
    errors <- attr(result, "errors")
    msg <- paste(
      "Config validation failed:",
      paste(errors$message, collapse = "\n  "),
      sep = "\n  "
    )
    rlang::abort(msg, class = "volvelle_config_error", errors = errors)
  }

  .check_measure_semantics(raw$measures)

  invisible(raw)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.normalise_config <- function(raw) {
  list(
    hierarchy = unlist(raw$hierarchy),   # YAML list -> character vector
    measures  = raw$measures,
    derived   = raw$derived %||% NULL,
    filters   = raw$filters %||% NULL
  )
}

.check_measure_semantics <- function(measures) {
  supported_fns <- c("sum", "mean", "weighted_mean", "n", "n_distinct", "min", "max")

  for (nm in names(measures)) {
    m <- measures[[nm]]

    if (!m$fn %in% supported_fns) {
      rlang::abort(
        sprintf(
          "Measure '%s': unsupported fn '%s'. Must be one of: %s",
          nm, m$fn, paste(supported_fns, collapse = ", ")
        ),
        class = "volvelle_config_error"
      )
    }

    if (identical(m$fn, "weighted_mean") && is.null(m$weight)) {
      rlang::abort(
        sprintf("Measure '%s': fn = 'weighted_mean' requires a 'weight' field", nm),
        class = "volvelle_config_error"
      )
    }
  }

  invisible(NULL)
}
