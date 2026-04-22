\docType{data}
\name{credit_portfolio}
\alias{credit_portfolio}
\title{Synthetic credit portfolio dataset}
\format{
A data frame with 500 rows and 8 variables:
\describe{
  \item{segment}{Broad portfolio segment: \code{"Corporate"}, \code{"Retail"}, or \code{"Sovereign"}.}
  \item{sub_segment}{Finer segment classification: \code{"Large Cap"}, \code{"SME"}, or \code{"Micro"}.}
  \item{counterparty}{Counterparty identifier, e.g. \code{"CP_0042"}.}
  \item{nominal_exposure}{Nominal exposure in currency units, rounded to the nearest thousand. Range 50,000–5,000,000.}
  \item{pd}{Probability of default. Range 0.001–0.15.}
  \item{lgd}{Loss given default. Range 0.20–0.60.}
  \item{status}{Loan performance status: \code{"performing"} (75\%), \code{"watch"} (15\%), or \code{"non_performing"} (10\%).}
  \item{entity_id}{Legal entity identifier, e.g. \code{"E042"}.}
}
}
\source{
Generated synthetically via \code{data-raw/make_demo_data.R}.
}
\description{
A synthetic dataset representing 500 credit exposures across three
hierarchy levels (segment, sub-segment, counterparty). Generated with
\code{set.seed(42)} for reproducibility. Used in package examples and the
demo vignette.
}
\keyword{datasets}
