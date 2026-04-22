\name{facets}
\alias{facets}
\alias{facets.volvelle_result}
\title{List available facets in a \code{volvelle_result}}
\usage{
facets(x)

\method{facets}{volvelle_result}(x)
}
\arguments{
\item{x}{A \code{volvelle_result} object.}
}
\value{
A character vector of facet names.
}
\description{
Returns the names of all computed facets: always includes \code{"full"}, plus one
entry per named filter defined in the config.
}
\examples{
cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
result   <- volvelle(credit_portfolio, cfg_path)
facets(result)
}
