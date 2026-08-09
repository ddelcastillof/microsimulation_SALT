#--------------------------------
# One-way (deterministic) sensitivity analysis module
#--------------------------------
#
# Deterministic sensitivity analysis per the ISPOR-SMDM Modeling Good Research
# Practices Task Force: vary one parameter at a time across a plausible range,
# holding all others at their base-case value, and display the result as a
# tornado diagram ordered by influence.
#
# Unlike the PSA this DOES include the discount rates. ISPOR treats them as
# methodological choices rather than uncertain quantities, so they belong in
# one-way and scenario analysis, not in the probabilistic sampling.
#
# Requires R/psa.R to be sourced first: assign_path() and apply_value()
# are shared with the PSA so both analyses substitute parameters identically.

print("Loading one-way sensitivity analysis functions")

# owsa_ranges: parameters to vary one at a time.
#   name/label/target/path - as in psa_specs()
#   low/high               - the range endpoints
#   source                 - provenance; "PLACEHOLDER" flags a range to source
#
# Placeholder ranges are +/-25% of the base case, except where a natural range
# exists (discount rates at the conventional 0-5%, risk_calib spanning the
# Globorisk-vs-GBD level disagreement).
owsa_ranges <- function(base = get_params()) {
  rng <- function(.name, .label, .target, .path, .low, .high, .source) {
    list(name = .name, label = .label, target = .target, path = .path,
         low = .low, high = .high, source = .source)
  }
  pct <- function(x, f = 0.25) c(x * (1 - f), x * (1 + f))

  cost_rng <- function(name, label, key) {
    b <- pct(base$costs[[key]])
    rng(name, label, "params", c("costs", key), b[1], b[2],
        "PLACEHOLDER: +/-25%, pending data/costs/costing_cleaned.xlsx")
  }

  list(
    delta_sbp = rng("delta_sbp", "Treatment effect on SBP (mmHg)",
                    "params", "delta_sbp",
                    base$delta_sbp - 3, base$delta_sbp + 3,
                    "PLACEHOLDER: +/-3 mmHg, pending swCRT treatment-effect model"),

    cost_hta          = cost_rng("cost_hta", "HTA management cost", "hta"),
    cvd_event         = cost_rng("cvd_event", "Acute CVD event cost", "cvd_event"),
    cost_postcvd      = cost_rng("cost_postcvd", "Post-CVD annual cost", "postcvd"),
    cost_intervention = cost_rng("cost_intervention", "Salt substitute cost",
                                 "intervention"),

    u_cvd     = rng("u_cvd", "Utility, CVD", "params", c("utility", "CVD"),
                    base$utility[["CVD"]] - 0.10, base$utility[["CVD"]] + 0.10,
                    "PLACEHOLDER: +/-0.10, pending trial eqindex analysis"),
    u_postcvd = rng("u_postcvd", "Utility, Post-CVD", "params",
                    c("utility", "PostCVD"),
                    base$utility[["PostCVD"]] - 0.10,
                    base$utility[["PostCVD"]] + 0.10,
                    "PLACEHOLDER: +/-0.10, pending trial eqindex analysis"),

    dw_cvd     = rng("dw_cvd", "Disability weight, CVD", "params",
                     c("dw", "CVD"),
                     base$dw[["CVD"]] - 0.10, base$dw[["CVD"]] + 0.10,
                     "PLACEHOLDER: replace with the GBD uncertainty interval"),
    dw_postcvd = rng("dw_postcvd", "Disability weight, Post-CVD", "params",
                     c("dw", "PostCVD"),
                     max(base$dw[["PostCVD"]] - 0.05, 0),
                     base$dw[["PostCVD"]] + 0.05,
                     "PLACEHOLDER: replace with the GBD uncertainty interval"),

    # spans the unresolved Globorisk-vs-GBD absolute level disagreement
    risk_calib = rng("risk_calib", "Globorisk calibration multiplier",
                     "params", "risk_calib", 0.5, 1.5,
                     "PLACEHOLDER: pending the CVD event definition decision"),

    # methodological choices: conventional 0-5% bracket around the 3% base case
    d_c = rng("d_c", "Cost discount rate", "params", "d_c", 0, 0.05,
              "Conventional 0-5% reference-case bracket"),
    d_e = rng("d_e", "Effect discount rate", "params", "d_e", 0, 0.05,
              "Conventional 0-5% reference-case bracket")
  )
}

# run_owsa: evaluate the model at each range endpoint, one parameter at a time.
#   ranges  - owsa_ranges() list, or a subset of it
#   outcome - name of the run_cea() element to track (default the DALY ICER)
# Returns one row per parameter, ordered by descending spread, plus the
# unperturbed base-case value in `base` for the tornado reference line.
#
# The Intervention arm's uncalibrated risk matrix is built once and reused:
# no parameter in owsa_ranges() affects it, since delta_sbp shifts only the
# Control arm and risk_calib is applied downstream of arm_risk(). Adding a
# range over a params$globorisk setting would invalidate that and require
# dropping the cache.
run_owsa <- function(ranges, cohort, mortality, params = get_params(),
                     outcome = "icer_daly") {
  require(data.table)
  risk_i <- arm_risk(cohort$sbp_intervention, cohort$base, params)

  at <- function(spec, value) {
    d <- apply_value(params, mortality, spec, value)
    evaluate_model(d$params, cohort, d$mortality,
                   risk_intervention = risk_i)[[outcome]]
  }

  out <- rbindlist(lapply(ranges, function(r) {
    data.table(param = r$name, label = r$label,
               low = r$low, high = r$high,
               out_low = at(r, r$low), out_high = at(r, r$high))
  }))
  out[, base := evaluate_model(params, cohort, mortality,
                               risk_intervention = risk_i)[[outcome]]]
  out[, spread := abs(out_high - out_low)]
  setorder(out, -spread)
  out[]
}

# tornado_plot: horizontal bars spanning each parameter's low-to-high outcome,
# most influential at the top, with the base case as a vertical reference.
tornado_plot <- function(owsa, xlab = "ICER per DALY averted") {
  require(ggplot2)
  require(data.table)
  d <- copy(owsa)
  d[, label := factor(label, levels = rev(label))]
  d[, `:=`(lo = pmin(out_low, out_high), hi = pmax(out_low, out_high))]

  ggplot(d, aes(y = label)) +
    geom_vline(xintercept = d$base[1], linetype = "dashed", colour = "grey50") +
    geom_segment(aes(x = lo, xend = hi, yend = label),
                 linewidth = 6, colour = "#1A5276", alpha = 0.85) +
    labs(x = xlab, y = NULL,
         title = "One-way sensitivity analysis",
         subtitle = "Dashed line: base case") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          plot.subtitle = element_text(colour = "grey50", size = 9))
}
