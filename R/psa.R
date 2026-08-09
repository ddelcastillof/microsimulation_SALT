#--------------------------------
# Probabilistic sensitivity analysis module
#--------------------------------
#
# Multivariate PSA following the ISPOR-SMDM Modeling Good Research Practices
# Task Force report on parameter estimation and uncertainty (Briggs et al.,
# Value in Health 2012;15:835-42):
#
#   * cost parameters              -> gamma      (positive, right-skewed)
#   * utilities, disability weights,
#     transition probabilities     -> beta       (bounded 0-1)
#   * continuous treatment effects -> normal
#   * multiplicative calibration   -> lognormal  (positive, median 1)
#
# Discount rates are deliberately absent: ISPOR treats them as policy choices
# fixed by the reference case, to be varied in one-way or scenario analysis
# (see R/owsa.R), not sampled as if uncertain.
#
# Dispersion values marked PLACEHOLDER in psa_specs() are the only thing that
# needs replacing once real uncertainty estimates are available. Nothing else
# in this file has to change.

print("Loading PSA functions")

# assign_path: return `x` with the element at `path` replaced by `value`.
# Works for nested lists and for named atomic vectors (params$dw, params$utility
# are named numerics, params$costs is a list).
assign_path <- function(x, path, value) {
  if (length(path) == 1L) {
    x[[path]] <- value
    return(x)
  }
  x[[path[1L]]] <- assign_path(x[[path[1L]]], path[-1L], value)
  x
}

# draw_dist: n draws for one parameter spec.
#
# gamma and beta are parameterised by method of moments from the mean and
# standard error, which is how CE model inputs are normally reported.
# beta_counts instead uses the trial counts directly: Beta(events + 1/2,
# n - events + 1/2), the Jeffreys posterior. That is exact rather than a
# moment approximation, and unlike Beta(events, n - events) it stays a proper
# distribution when zero events were observed.
draw_dist <- function(spec, n) {
  switch(spec$dist,
    gamma = {
      shape <- (spec$mean / spec$se)^2
      scale <- spec$se^2 / spec$mean
      rgamma(n, shape = shape, scale = scale)
    },
    beta = {
      v <- spec$se^2
      if (v >= spec$mean * (1 - spec$mean)) {
        stop("beta spec '", spec$name, "': se too large for mean ", spec$mean)
      }
      k <- spec$mean * (1 - spec$mean) / v - 1
      rbeta(n, spec$mean * k, (1 - spec$mean) * k)
    },
    beta_counts = {
      if (spec$n == 0) return(rep(0, n))     # no person-time, no uncertainty
      rbeta(n, spec$events + 0.5, spec$n - spec$events + 0.5)
    },
    normal    = rnorm(n, spec$mean, spec$se),
    lognormal = rlnorm(n, meanlog = 0, sdlog = spec$sdlog),
    stop("unknown distribution: ", spec$dist)
  )
}

# psa_specs: the parameters sampled by the PSA, one entry each.
#
#   name   - column name in the draws table, and tornado/label key
#   label  - human-readable name for plots and tables
#   target - "params" or "mortality"; which structure the value is written into
#   path   - element path within that structure (see assign_path)
#   dist   - distribution name understood by draw_dist
#   source - provenance; "PLACEHOLDER" flags a dispersion still to be sourced
#
# `mortality` is the mortality_rates() list. Supplying it adds the three
# trial-derived transition probabilities, drawn from their own event counts so
# no dispersion has to be invented. Omitting it leaves them fixed at the point
# estimate rather than sampling them from a made-up standard error.
psa_specs <- function(mortality = NULL, base = get_params()) {
  # formals are dot-prefixed on purpose: R partially matches argument names
  # against formals declared before `...`, so a plain `name` formal would
  # swallow the `n = ` distribution argument below and silently shift every
  # positional argument by one.
  spec <- function(.name, .label, .target, .path, .dist, .source, ...) {
    c(list(name = .name, label = .label, target = .target, path = .path,
           dist = .dist, source = .source), list(...))
  }

  # costs: se placeholders at 20% of the mean. params$costs$healthy is 0 and a
  # gamma is undefined there, so it is left fixed.
  cost_spec <- function(name, label, key) {
    m <- base$costs[[key]]
    spec(name, label, "params", c("costs", key), "gamma",
         "PLACEHOLDER: se = 20% of mean, pending data/costs/costing_cleaned.xlsx",
         mean = m, se = 0.20 * m)
  }

  out <- list(
    cost_hta          = cost_spec("cost_hta", "HTA management cost", "hta"),
    cvd_event         = cost_spec("cvd_event", "Acute CVD event cost", "cvd_event"),
    cost_postcvd      = cost_spec("cost_postcvd", "Post-CVD annual cost", "postcvd"),
    cost_intervention = cost_spec("cost_intervention", "Salt substitute cost",
                                  "intervention"),

    u_healthy = spec("u_healthy", "Utility, Healthy", "params",
                     c("utility", "Healthy"), "beta",
                     "PLACEHOLDER: EQ-5D se pending trial eqindex analysis",
                     mean = base$utility[["Healthy"]], se = 0.05),
    u_cvd     = spec("u_cvd", "Utility, CVD", "params",
                     c("utility", "CVD"), "beta",
                     "PLACEHOLDER: EQ-5D se pending trial eqindex analysis",
                     mean = base$utility[["CVD"]], se = 0.05),
    u_postcvd = spec("u_postcvd", "Utility, Post-CVD", "params",
                     c("utility", "PostCVD"), "beta",
                     "PLACEHOLDER: EQ-5D se pending trial eqindex analysis",
                     mean = base$utility[["PostCVD"]], se = 0.05),

    dw_cvd     = spec("dw_cvd", "Disability weight, CVD", "params",
                      c("dw", "CVD"), "beta",
                      "PLACEHOLDER: fit to GBD uncertainty interval",
                      mean = base$dw[["CVD"]], se = 0.05),
    dw_postcvd = spec("dw_postcvd", "Disability weight, Post-CVD", "params",
                      c("dw", "PostCVD"), "beta",
                      "PLACEHOLDER: fit to GBD uncertainty interval",
                      mean = base$dw[["PostCVD"]], se = 0.02),

    delta_sbp = spec("delta_sbp", "Treatment effect on SBP (mmHg)", "params",
                     "delta_sbp", "normal",
                     "PLACEHOLDER: se pending swCRT treatment-effect model",
                     mean = base$delta_sbp, se = 1.5),

    # Carries the unresolved disagreement between the Globorisk absolute level
    # and GBD Peru IHD+stroke incidence into the credible interval. sdlog 0.2
    # gives a 95% interval of roughly 0.68-1.48 on the hazard.
    risk_calib = spec("risk_calib", "Globorisk calibration multiplier",
                      "params", "risk_calib", "lognormal",
                      "PLACEHOLDER: sdlog pending the CVD event definition decision",
                      sdlog = 0.2)
  )

  if (!is.null(mortality)) {
    cnt <- mortality$counts
    mort_spec <- function(name, label) {
      r <- cnt[param == name]
      spec(name, label, "mortality", name, "beta_counts",
           "Trial event counts (Beta-Jeffreys posterior)",
           events = as.integer(r$events), n = as.integer(r$n))
    }
    out <- c(out, list(
      p_background = mort_spec("p_background", "Non-CVD death probability"),
      p_cvd_fatal  = mort_spec("p_cvd_fatal",  "CVD case fatality"),
      p_postcvd    = mort_spec("p_postcvd",    "Post-CVD death probability")
    ))
  }
  out
}

# sample_params: n_sim independent draws for every spec.
# Returns a data.table with one row per draw and one column per parameter.
# Parameters are drawn independently: the specs declare no correlation
# structure. If correlated inputs are ever needed (e.g. jointly estimated
# regression coefficients), draw them on the multivariate normal scale and
# transform, rather than widening these marginals.
sample_params <- function(specs, n_sim = 1000L, seed = 42L) {
  require(data.table)
  set.seed(seed)
  as.data.table(lapply(specs, draw_dist, n = n_sim))
}

# apply_draw: write one row of the draws table into params and mortality.
# `draw` is a single-row data.table. Returns list(params, mortality).
apply_draw <- function(params, mortality, specs, draw) {
  cur <- list(params = params, mortality = mortality)
  for (s in specs) {
    cur <- apply_value(cur$params, cur$mortality, s, draw[[s$name]][1L])
  }
  cur
}

# run_psa: draw n_sim parameter sets and evaluate the model at each.
#   specs     - psa_specs() list
#   cohort    - build_cohort() output, fixed across draws
#   mortality - mortality_rates() list; sampled entries are overwritten per draw
#   params    - base get_params() list; unsampled entries stay at these values
#
# Single-loop design: one microsimulation per arm per draw, with both arms run
# from the same RNG seed so individual-level (first-order) Monte Carlo noise is
# common to the arms and largely cancels in the incremental estimate. Because
# the cohort is a fixed set of real trial participants rather than a resampled
# synthetic population, that is sufficient -- see psa_convergence() to confirm
# n_sim is large enough for the running mean to settle.
#
# Returns one row per draw: the CEA outputs cbind'ed to the sampled parameter
# values, so draws can be regressed on inputs later (EVPPI, metamodelling)
# without re-running anything.
run_psa <- function(specs, cohort, mortality, params = get_params(),
                    n_sim = 1000L, seed = 42L) {
  require(data.table)
  draws <- sample_params(specs, n_sim = n_sim, seed = seed)

  # the Intervention arm's SBP depends on no sampled parameter, so its
  # uncalibrated risk matrix is built once instead of n_sim times
  risk_i <- arm_risk(cohort$sbp_intervention, cohort$base, params)

  out <- vector("list", n_sim)
  for (s in seq_len(n_sim)) {
    d   <- apply_draw(params, mortality, specs, draws[s])
    cea <- evaluate_model(d$params, cohort, d$mortality,
                          risk_intervention = risk_i)
    out[[s]] <- data.table(
      sim          = s,
      d_cost       = cea$d_cost,
      daly_averted = cea$daly_averted,
      qaly_gained  = cea$qaly_gained,
      icer_daly    = cea$icer_daly,
      icer_qaly    = cea$icer_qaly
    )
  }
  cbind(rbindlist(out), draws)
}

# ceac: cost-effectiveness acceptability curve. For each willingness-to-pay
# threshold, the share of draws with positive net monetary benefit
# (lambda * effect - incremental cost).
#
# The default grid spans 0 to roughly 3x Peru's GDP per capita, the WHO-CHOICE
# convention. PLACEHOLDER: replace with the threshold your reference case adopts.
ceac <- function(draws, wtp = seq(0, 24000, by = 500),
                 outcome = c("daly", "qaly")) {
  require(data.table)
  outcome <- match.arg(outcome)
  eff <- if (outcome == "daly") draws$daly_averted else draws$qaly_gained
  data.table(
    wtp  = wtp,
    p_ce = vapply(wtp,
                  function(l) mean(l * eff - draws$d_cost > 0, na.rm = TRUE),
                  numeric(1))
  )
}

# psa_summary: mean, median and 95% credible interval for each CEA outcome.
# Note that the mean of the per-draw ICERs is not the ICER of the mean cost and
# effect: report the credible interval around incremental cost and effect, and
# treat the ICER interval as descriptive only (it is undefined when the effect
# denominator crosses zero).
psa_summary <- function(draws, probs = c(0.025, 0.975)) {
  require(data.table)
  cols <- intersect(c("d_cost", "daly_averted", "qaly_gained",
                      "icer_daly", "icer_qaly"), names(draws))
  rbindlist(lapply(cols, function(cn) {
    x <- draws[[cn]]
    q <- stats::quantile(x, probs, na.rm = TRUE)
    data.table(outcome = cn,
               mean   = mean(x, na.rm = TRUE),
               median = stats::median(x, na.rm = TRUE),
               lower  = q[[1]],
               upper  = q[[2]])
  }))
}

# psa_convergence: running mean of an outcome across draws, to demonstrate that
# n_sim is large enough. plot = FALSE returns the trace instead of the figure.
psa_convergence <- function(draws, outcome = "icer_daly", plot = TRUE) {
  require(data.table)
  x  <- draws[[outcome]]
  tr <- data.table(sim = seq_along(x),
                   running_mean = cumsum(x) / seq_along(x))
  if (!plot) return(tr)
  require(ggplot2)
  ggplot(tr, aes(sim, running_mean)) +
    geom_hline(yintercept = mean(x, na.rm = TRUE),
               linetype = "dashed", colour = "grey60") +
    geom_line(colour = "#1A5276") +
    labs(x = "PSA draw", y = paste("Running mean,", outcome),
         title = "PSA convergence") +
    theme_minimal(base_size = 11)
}

# ce_plane_psa: scatter of the PSA cloud on the cost-effectiveness plane.
ce_plane_psa <- function(draws, wtp = NULL) {
  require(ggplot2)
  p <- ggplot(draws, aes(daly_averted, d_cost)) +
    geom_hline(yintercept = 0, colour = "grey70") +
    geom_vline(xintercept = 0, colour = "grey70") +
    geom_point(alpha = 0.25, size = 1.2, colour = "#1A5276") +
    labs(x = "DALYs averted (Intervention vs Control)",
         y = "Incremental cost",
         title = "Cost-effectiveness plane (PSA)") +
    theme_minimal(base_size = 11)
  if (!is.null(wtp)) {
    p <- p + geom_abline(slope = wtp, intercept = 0,
                         linetype = "dashed", colour = "#E74C3C")
  }
  p
}

# ceac_plot: plot a ceac() table.
ceac_plot <- function(ce) {
  require(ggplot2)
  ggplot(ce, aes(wtp, p_ce)) +
    geom_line(colour = "#1A5276", linewidth = 0.7) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Willingness to pay per DALY averted",
         y = "Probability cost-effective",
         title = "Cost-effectiveness acceptability curve") +
    theme_minimal(base_size = 11)
}

# apply_value: substitute a single value into params or mortality according to
# a spec's `target` and `path`. Shared by apply_draw() and run_owsa().
apply_value <- function(params, mortality, spec, value) {
  if (spec$target == "params") {
    params <- assign_path(params, spec$path, value)
  } else {
    mortality <- assign_path(mortality, spec$path, value)
  }
  list(params = params, mortality = mortality)
}
