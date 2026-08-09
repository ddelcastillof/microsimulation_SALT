#--------------------------------
# Trial-based mortality module
#--------------------------------

print("Loading mortality functions")

# mortality_rates: estimate pooled per-cycle death probabilities from a
# person-cycle table with columns:
#   state     - one of "Healthy", "CVD", "PostCVD"
#   dead      - 1 if the person died during that person-cycle, else 0
#   cvd_death - 1 if that death was attributed to CVD (from c_muerte), else 0
# Returns the three per-cycle probabilities plus a `counts` table carrying the
# events and denominators behind them. Counts are pooled because 5-year trial
# death counts are too sparse for fine age/sex stratification.
#
# The counts exist so the PSA can draw each probability as its exact conjugate
# posterior, Beta(events, n - events), rather than needing an invented standard
# error (see psa_specs() in R/psa.R).
mortality_rates <- function(dt) {
  require(data.table)
  stopifnot(is.data.table(dt))

  healthy_pt  <- dt[state == "Healthy", .N]
  noncvd_dead <- dt[state == "Healthy" & dead == 1 & cvd_death == 0, .N]
  p_background <- if (healthy_pt > 0) noncvd_dead / healthy_pt else 0

  cvd_events <- dt[state == "CVD", .N]
  cvd_fatal  <- dt[state == "CVD" & dead == 1, .N]
  p_cvd_fatal <- if (cvd_events > 0) cvd_fatal / cvd_events else 0

  post_pt   <- dt[state == "PostCVD", .N]
  post_dead <- dt[state == "PostCVD" & dead == 1, .N]
  p_postcvd <- if (post_pt > 0) post_dead / post_pt else 0

  list(p_background = p_background,
       p_cvd_fatal  = p_cvd_fatal,
       p_postcvd    = p_postcvd,
       counts       = data.table(
         param  = c("p_background", "p_cvd_fatal", "p_postcvd"),
         events = c(noncvd_dead, cvd_fatal, post_dead),
         n      = c(healthy_pt, cvd_events, post_pt)
       ))
}
