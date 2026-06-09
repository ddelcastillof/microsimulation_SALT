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
get_params <- function(delta_sbp = -5, seed = 123L) {
  list(
    n_t          = 6L,                                   # number of cycles
    cycle_length = 5 / 6,                                # years per cycle (5-yr horizon / 6 cycles)
    state_names  = c("Healthy", "CVD", "PostCVD", "Dead"),
    d_c          = 0.03,                                 # cost discount rate
    d_e          = 0.03,                                 # effect discount rate
    arms         = c("Intervention", "Control"),
    delta_sbp    = delta_sbp,                            # mmHg, swCRT placeholder
    dw           = c(CVD = 0.43, PostCVD = 0.07),        # GBD disability weights
    utility      = c(Healthy = 0.85, CVD = 0.55,
                     PostCVD = 0.75, Dead = 0),          # EQ-5D placeholders
    costs        = list(
      healthy      = 0,
      hta          = 120,    # annual HTA management cost
      cvd_event    = 3000,   # one-off acute CVD event cost
      postcvd      = 600,    # annual post-CVD chronic cost
      intervention = 30      # salt substitute cost per treated person-cycle
    ),
    globorisk    = list(version = "office", iso = "PER", year = 2017L),
    seed         = as.integer(seed)
  )
}
