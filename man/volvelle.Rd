\name{volvelle}
\alias{volvelle}
\title{Run ROLLUP aggregation across all filter facets}
\usage{
volvelle(data, config, extra_filters = NULL)
}
\arguments{
\item{data}{A \code{data.frame}, \code{tibble}, or \code{data.table}.}

\item{config}{Path to a YAML config file, or a pre-parsed named list.}

\item{extra_filters}{Optional named list of additional filter definitions
(same structure as the \code{filters} block in the YAML) to merge with any
filters declared in the config. Names must not clash with config filters.}
}
\value{
A \code{volvelle_result} S3 object: a named list of \code{data.table} objects,
one per facet (\code{"full"} plus one per named filter). Each table contains
ROLLUP aggregations plus \code{.id} and \code{.rollup_depth} metadata columns.
Attribute \code{config} holds the parsed, validated config.
}
\description{
Takes a data frame and a YAML config file (or pre-parsed list), runs a
SQL-style ROLLUP aggregation over the declared hierarchy, and returns one
aggregated data.table per filter facet (plus an unfiltered \code{"full"} facet).
}
\examples{
cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
result   <- volvelle(credit_portfolio, cfg_path)
names(result)
result$full[result$full$.rollup_depth == 0, ]  # grand total row
}
