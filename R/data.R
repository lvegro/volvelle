#' Synthetic credit portfolio dataset
#'
#' A synthetic dataset representing 500 credit exposures across three
#' hierarchy levels (segment, sub-segment, counterparty). Generated with
#' `set.seed(42)` for reproducibility. Used in package examples and the
#' demo vignette.
#'
#' @format A data frame with 500 rows and 8 variables:
#' \describe{
#'   \item{segment}{Broad portfolio segment: `"Corporate"`, `"Retail"`, or `"Sovereign"`.}
#'   \item{sub_segment}{Finer segment classification: `"Large Cap"`, `"SME"`, or `"Micro"`.}
#'   \item{counterparty}{Counterparty identifier, e.g. `"CP_0042"`.}
#'   \item{nominal_exposure}{Nominal exposure in currency units, rounded to the nearest thousand. Range 50,000–5,000,000.}
#'   \item{pd}{Probability of default. Range 0.001–0.15.}
#'   \item{lgd}{Loss given default. Range 0.20–0.60.}
#'   \item{status}{Loan performance status: `"performing"` (75 %), `"watch"` (15 %), or `"non_performing"` (10 %).}
#'   \item{entity_id}{Legal entity identifier, e.g. `"E042"`.}
#' }
#' @source Generated synthetically via `data-raw/make_demo_data.R`.
"credit_portfolio"
