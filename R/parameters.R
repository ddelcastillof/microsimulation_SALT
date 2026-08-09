#----------------------------
# Parameters for the microsimulation
#----------------------------

print("Loading microsimulation parameters")

# get_params: returns a single config list for the microsimulation.
# delta_sbp is the swCRT-estimated treatment effect on SBP (mmHg);
# a configurable placeholder until the treatment-effect workstream lands.
# seed sets the RNG seed for reproducible microsimulation runs.
# Cost and disability-weight values are placeholders to be replaced from
# data/costs/costing_cleaned.xlsx and GBD reference values.
get_params <- function(delta_sbp = 0, seed = 42) {
  list(
    n_t          = 6L,                                   # number of cycles
    # One cycle = one inter-visit interval. The trial measured blood pressure
    # every 5 months after baseline (Apr 2014 - Mar 2017), which the data
    # confirm: median gap between consecutive visits 0.419 yr (IQR 0.402-0.449),
    # median baseline-to-last span 2.74 yr. Horizon is therefore 6 x 5/12 = 2.5 yr.
    cycle_length = 5 / 12,                               # years per cycle
    state_names  = c("Healthy", "CVD", "PostCVD", "Dead"),
    d_c          = 0.03,                                 # cost discount rate
    d_e          = 0.03,                                 # effect discount rate
    arms         = c("Intervention", "Control"),
    delta_sbp    = delta_sbp,                            # mmHg, swCRT placeholder
    # risk_calib: multiplier on the Globorisk cumulative hazard. 1 = take the
    # equation at face value. Exists so the PSA can carry the unresolved
    # Globorisk-vs-GBD absolute-level disagreement into the credible interval.
    risk_calib   = 1,
    # HTA cost thresholds, mmHg: the ACC/AHA 2017 stage-1 pair, 130/80.
    # htn_flag() bills management cost when EITHER arm is met, so isolated
    # diastolic hypertension is counted. DBP never enters the Globorisk office
    # equation; it only gates this cost. Note methods-discussions.md still
    # states the rule as 140/90 -- update it, or move these to 140/90, so the
    # costed definition and the documented one agree.
    htn_sbp      = 130,
    htn_dbp      = 80,
    dw           = c(CVD = 0.43, PostCVD = 0.07),        # GBD disability weights
    utility      = c(Healthy = 0.85, CVD = 0.55,
                     PostCVD = 0.75, Dead = 0),          # EQ-5D placeholders
    # All costs are per individual, PER CYCLE, in 2024 US dollars. costs.R
    # applies them as-is; none is rescaled by cycle_length, so an annual figure
    # entered here would be understated by a factor of cycle_length.
    # cvd_event lands once because CVD is a one-cycle tunnel state.
    # Dead accrues nothing; that is structural in costs.R, not a parameter.
    costs        = list(
      healthy      = 0,
      hta          = 588.64,                 # hypertension management, annual
      # acute CVD event: mean of the stroke and AMI event costs, since the
      # model has a single undifferentiated CVD state
      cvd_event    = (6060.10 + 5876.15) / 2,
      postcvd      = 1200,                    # PLACEHOLDER: annual post-CVD chronic cost
      intervention = 3.54 + 2.63             # salt substitute + social component
    ),
    # extrapolate: correct the 40/74 age clamp using GBD Peru IHD+stroke
    # incidence gradients (see gbd_age_multiplier). Set FALSE for the
    # plain-clamp sensitivity scenario.
    globorisk    = list(version = "office", iso = "PER", year = 2017L,
                        extrapolate = TRUE),
    seed         = as.integer(seed)
  )
}
