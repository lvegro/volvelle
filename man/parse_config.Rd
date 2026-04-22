\name{parse_config}
\alias{parse_config}
\title{Parse and validate a volvelle configuration}
\usage{
parse_config(config)
}
\arguments{
\item{config}{Path to a YAML file or a named R list already parsed from YAML.}
}
\value{
A validated, normalised config list with elements \code{hierarchy},
\code{measures}, \code{derived} (may be \code{NULL}), and \code{filters} (may be \code{NULL}).
}
\description{
Reads a YAML config file (or accepts a pre-parsed list) and validates it
against the bundled JSON Schema. Returns a structured list suitable for use
by the aggregation engine.
}
\examples{
cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
cfg <- parse_config(cfg_path)
names(cfg)
}
