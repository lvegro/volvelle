## Script to generate the credit_portfolio demo dataset.
## Run once and commit the resulting data/credit_portfolio.rda.

set.seed(42)

credit_portfolio <- data.frame(
  segment          = sample(c("Corporate", "Retail", "Sovereign"), 500, replace = TRUE),
  sub_segment      = sample(c("Large Cap", "SME", "Micro"), 500, replace = TRUE),
  counterparty     = paste0("CP_", sprintf("%04d", sample(1:200, 500, replace = TRUE))),
  nominal_exposure = round(runif(500, 50000, 5000000), -3),
  pd               = round(runif(500, 0.001, 0.15), 4),
  lgd              = round(runif(500, 0.2, 0.6), 2),
  status           = sample(
    c("performing", "watch", "non_performing"),
    500,
    replace = TRUE,
    prob    = c(0.75, 0.15, 0.10)
  ),
  entity_id        = paste0("E", sample(1:150, 500, replace = TRUE)),
  stringsAsFactors = FALSE
)

usethis::use_data(credit_portfolio, overwrite = TRUE)
