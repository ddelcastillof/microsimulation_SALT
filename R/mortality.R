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

# ---------------------------------------------------------------------------
# GBD-derived lifetime mortality
# ---------------------------------------------------------------------------
#
# mortality_rates() above stays the trial-based estimator, but at a lifetime
# horizon it can no longer be the model input. It pools 11 deaths into one
# constant per-cycle probability; held fixed for ~197 cycles that leaves most
# of the cohort alive at age 100, and it gives the intervention no age gradient
# to act against. It is retained as a VALIDATION check on the GBD level.
#
# The replacement is age x sex specific and comes from GBD 2023 Peru. See
# code/gbd_reference_data.R for provenance and for why acute case fatality is
# taken from literature rather than from GBD deaths / incidence.

.mort_cache <- new.env(parent = emptyenv())

# load_gbd_mortality: read the Peru mortality extract and derive the two rates
# the model consumes.
#
# noncvd_rate is all-cause NET of IHD + stroke. The subtraction is essential:
# probs() routes Healthy -> Dead as NON-CVD death only, because CVD deaths
# reach Dead through the CVD state. Leaving IHD and stroke in the background
# rate would kill the same people twice and, worse, would do it in the arm the
# intervention cannot protect.
load_gbd_mortality <- function(path = here::here("data", "gbd_mortality_peru.csv")) {
  require(data.table)
  if (!is.null(.mort_cache[[path]])) return(.mort_cache[[path]])

  g <- fread(path)
  g[, noncvd_rate := (allcause_death_rate - ihd_stroke_death_rate) / 1e5]
  g[, sex_num := as.integer(sex == "male")]
  setorder(g, sex, age_lower)

  stopifnot(all(g$noncvd_rate > 0), all(g$acute_cf > 0 & g$acute_cf < 1))
  .mort_cache[[path]] <- g[]
  g[]
}

# gbd_band_value: look up a column of the mortality table for each (age, sex).
# Same banding convention as gbd_age_multiplier() in globorisk_risk.R --
# findInterval on the band lower bounds, with ages beyond the last band taking
# the last band's value.
gbd_band_value <- function(age, sex_num, col, rates = load_gbd_mortality()) {
  require(data.table)
  breaks  <- sort(unique(rates$age_lower))
  idx     <- findInterval(age, breaks)
  idx[!is.na(idx) & idx < 1L] <- 1L          # below the first band -> first band
  band_lo <- breaks[idx]
  rates[[col]][match(paste(sex_num, band_lo),
                     paste(rates$sex_num, rates$age_lower))]
}

# mortality_matrices: per-cycle, per-person death probabilities over the whole
# horizon. Returns n_i x n_t matrices, because at a lifetime horizon these are
# no longer constants -- a 40-year-old at cycle 1 is an 80-year-old by cycle
# 96, and a scalar would flatten exactly the gradient the model needs.
#
# Rates are annual; the per-cycle probability is 1 - exp(-rate * cycle_length),
# the constant-hazard conversion used throughout this model.
#
# The returned values are UNCALIBRATED. lifetime_mortality() applies the
# sampled level multipliers, for the same reason arm_risk() leaves risk_calib
# out: it keeps this matrix independent of every PSA parameter, so it can be
# built once per run rather than once per draw.
mortality_matrices <- function(base, params,
                               rates = load_gbd_mortality()) {
  require(data.table)
  n_i <- nrow(base)
  n_t <- params$n_t
  cl  <- params$cycle_length

  age <- rep(base$age, times = n_t) +
           rep((seq_len(n_t) - 1L) * cl, each = n_i)
  sex <- rep(base$sex_num, times = n_t)

  rate_bg <- gbd_band_value(age, sex, "noncvd_rate", rates)
  cf      <- gbd_band_value(age, sex, "acute_cf",    rates)

  list(
    p_background = matrix(1 - exp(-rate_bg * cl), n_i, n_t),
    p_cvd_fatal  = matrix(cf, n_i, n_t)
  )
}

# lifetime_mortality: apply the sampled level multipliers and derive post-CVD
# mortality. Called from evaluate_model() on every draw.
#
# p_postcvd is NOT an independent input: it is the person's own background
# mortality raised by params$hr_postcvd, on the cumulative-hazard scale. That
# makes post-CVD mortality age-varying for free, keeps it above background by
# construction, and means the PSA samples one interpretable hazard ratio
# instead of a probability with no denominator behind it. Any p_postcvd
# supplied in `mortality` is therefore ignored by design.
#
# mort_calib composes into p_postcvd as well, so a draw that moves the GBD
# mortality level moves it consistently for both living states.
lifetime_mortality <- function(mortality, params) {
  mc <- if (is.null(params$mort_calib)) 1 else params$mort_calib
  cf <- if (is.null(params$cf_calib))   1 else params$cf_calib
  hr <- if (is.null(params$hr_postcvd)) 1 else params$hr_postcvd

  list(
    p_background = apply_calib(mortality$p_background, mc),
    p_cvd_fatal  = apply_calib(mortality$p_cvd_fatal,  cf),
    p_postcvd    = apply_calib(mortality$p_background, mc * hr)
  )
}

# reference_sle: standard life expectancy at an exact age, for YLL.
#
# Linear interpolation between the tabulated ages, flat beyond the last one and
# floored at zero so an age past the end of the table cannot produce negative
# lost years. See code/gbd_reference_data.R for which table this is and why.
.sle_cache <- new.env(parent = emptyenv())

reference_sle <- function(age,
                          path = here::here("data", "gbd_reference_life_table.csv")) {
  require(data.table)
  if (is.null(.sle_cache[[path]])) .sle_cache[[path]] <- fread(path)
  tab <- .sle_cache[[path]]
  pmax(stats::approx(tab$age, tab$sle, xout = age, rule = 2)$y, 0)
}
