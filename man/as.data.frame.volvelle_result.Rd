\name{as.data.frame.volvelle_result}
\alias{as.data.frame.volvelle_result}
\title{Convert a \code{volvelle_result} facet to a plain data.frame}
\usage{
\method{as.data.frame}{volvelle_result}(x, facet = "full", ...)
}
\arguments{
\item{x}{A \code{volvelle_result} object.}

\item{facet}{Name of the facet to extract; defaults to \code{"full"}.}

\item{...}{Ignored.}
}
\value{
A \code{data.frame}.
}
\description{
Extracts one facet from a \code{volvelle_result} and returns it as a plain
\code{data.frame}, suitable for downstream use with standard R tooling.
}
\examples{
cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
result   <- volvelle(credit_portfolio, cfg_path)
df       <- as.data.frame(result, facet = "performing")
head(df)
}
