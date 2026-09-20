#-----------------------
# Microsimulation orchestrator
#-----------------------
#
# Runs the base case, the one-way deterministic sensitivity analysis, and
# optionally the probabilistic sensitivity analysis, then caches everything to
# output/cea_results.rds for salt_results.qmd to render.
#
# Configured through the SALT_PSA_N, SALT_OWSA, SALT_SEED and SALT_HORIZON
# environment variables, read below. See README.md for usage.
#
# The base case runs a LIFETIME horizon: the trial's own 6 cycles of 5/12 years
# followed by projection to age 100 for the youngest participant (~197 cycles
# in total). Set SALT_HORIZON=trial to run only the 6 observed cycles, which is
# what --quick uses for a fast smoke test.

here::i_am("salt_results.qmd")

# --- load modules ---
source(here::here("R", "cleaning.R"))
source(here::here("R", "parameters.R"))
source(here::here("R", "globorisk_risk.R"))
source(here::here("R", "mortality.R"))
source(here::here("R", "cohort.R"))
source(here::here("R", "transition.R"))
source(here::here("R", "costs.R"))
source(here::here("R", "effects.R"))
source(here::here("R", "microsim.R"))
source(here::here("R", "cea.R"))
source(here::here("R", "model_run.R"))
source(here::here("R", "psa.R"))     # must precede owsa.R (shared helpers)
source(here::here("R", "owsa.R"))

require(data.table)
require(yaml)

# --- run configuration ---
psa_n     <- as.integer(Sys.getenv("SALT_PSA_N", "1000"))
do_owsa   <- toupper(Sys.getenv("SALT_OWSA", "TRUE")) != "FALSE"
psa_seed  <- as.integer(Sys.getenv("SALT_SEED", "42"))
horizon   <- tolower(Sys.getenv("SALT_HORIZON", "lifetime"))
stopifnot(horizon %in% c("lifetime", "trial"))

# --- parameters ---
params <- get_params(delta_sbp = -1.29, seed = psa_seed)

# --- data ---
long <- clean_long()
prep_long(long)

# --- horizon ---
# n_t cannot be set in get_params(): it depends on the cohort's youngest member,
# since all individuals share one rectangular matrix of cycles and the model
# runs until the youngest reaches params$max_age. Older participants simply
# spend the tail of the horizon Dead, which accrues nothing.
if (horizon == "lifetime") params <- set_horizon(params, long)
message(sprintf("Horizon: %s -- %d cycles of %.3f yr = %.1f yr",
                horizon, params$n_t, params$cycle_length,
                params$n_t * params$cycle_length))

# village crossover order: named list village -> crossover wave
vorder_raw <- yaml::read_yaml(here::here("data", "village_order.yaml"))$villages
village_order <- setNames(
  lapply(vorder_raw, function(x) as.integer(x[[2]])),
  vapply(vorder_raw, function(x) as.character(x[[1]]), character(1))
)

# --- cohort ---
cohort <- build_cohort(long, village_order, params)

# --- mortality ---
# The model input is GBD Peru age x sex mortality, net of IHD + stroke so the
# Healthy -> Dead arm carries non-CVD death only (CVD deaths route through the
# CVD state). It has to be age-varying: a single pooled trial probability held
# constant for ~197 cycles would leave most of the cohort alive at 100.
mortality <- mortality_matrices(cohort$base, params)

# The trial's own deaths are now a VALIDATION CHECK on that level rather than
# the model input. The person-cycle state is left as "Healthy" deliberately:
# the CVD and PostCVD denominators were always empty, and the quantities they
# were meant to estimate now come from GBD and literature instead.
death_dt <- long[wave >= 1 & at_risk,
                 .(codigo, wave, state = "Healthy", dead, cvd_death)]
trial_mort <- mortality_rates(death_dt)
gbd_trial_window <- mean(mortality$p_background[, seq_len(params$n_trial)])
message(sprintf(
  "Mortality check over the trial window -- trial %.5f vs GBD %.5f per cycle (ratio %.2f)",
  trial_mort$p_background, gbd_trial_window,
  trial_mort$p_background / gbd_trial_window))

# --- base case: exposure continues after the trial ---
cea <- evaluate_model(params, cohort, mortality)
message("Base case complete (post-trial exposure: continue).")

# --- scenario: exposure stops when the trial ends ---
# Structural assumption, not a parameter range, so it is reported as its own
# scenario rather than as a tornado bar (see the note in R/owsa.R).
params_stop <- modifyList(params, list(post_trial = "stop"))
cea_stop <- evaluate_model(params_stop, cohort, mortality)
message("Scenario complete (post-trial exposure: stop).")

# --- one-way deterministic sensitivity analysis ---
owsa <- NULL
if (do_owsa) {
  owsa <- run_owsa(owsa_ranges(base = params), cohort, mortality, params)
  message("One-way sensitivity analysis complete (",
          nrow(owsa), " parameters).")
}

# --- probabilistic sensitivity analysis ---
psa <- NULL
if (psa_n > 0L) {
  specs <- psa_specs(base = params)
  psa <- run_psa(specs, cohort, mortality, params,
                 n_sim = psa_n, seed = psa_seed)
  message("PSA complete (", psa_n, " draws, seed ", psa_seed, ").")
} else {
  message("PSA skipped. Set SALT_PSA_N to enable.")
}

# --- cache outputs ---
if (!dir.exists(here::here("output"))) dir.create(here::here("output"))
saveRDS(list(params = params, cea = cea, cea_stop = cea_stop,
             owsa = owsa, psa = psa,
             horizon = list(type = horizon, n_t = params$n_t,
                            years = params$n_t * params$cycle_length),
             trial_mortality = trial_mort),
        here::here("output", "cea_results.rds"))

message("Results written to output/cea_results.rds")
print(cea$summary)
