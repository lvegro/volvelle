\name{validate_config}
\alias{validate_config}
\title{Validate a parsed config list against the JSON Schema}
\usage{
validate_config(raw)
}
\arguments{
\item{raw}{A named list, typically from \code{yaml::read_yaml()}.}
}
\value{
Invisibly returns \code{raw} on success; throws a \code{volvelle_config_error}
on validation failure.
}
\description{
Validates a parsed config list against the JSON Schema and runs additional
semantic checks (e.g. \code{weighted_mean} requires a \code{weight} field).
}
\examples{
cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
raw <- yaml::read_yaml(cfg_path)
validate_config(raw)
}
