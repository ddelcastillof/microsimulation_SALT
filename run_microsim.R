#-----------------------
# Microsimulation orchestrator
#-----------------------
#
# Runs the base case, the one-way deterministic sensitivity analysis, and
# optionally the probabilistic sensitivity analysis, then caches everything to
# output/cea_results.rds for salt_results.qmd to render.
#
# Configured through the SALT_PSA_N, SALT_OWSA and SALT_SEED environment
# variables, read below. The PSA costs roughly 1.5 s per draw on the full
# cohort, so the default of 1000 draws takes ~25 min. See README.md for usage.

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

# --- parameters ---
params <- get_params(delta_sbp = -1.29, seed = psa_seed)

# --- data ---
long <- clean_long()
prep_long(long)

# village crossover order: named list village -> crossover wave
vorder_raw <- yaml::read_yaml(here::here("data", "village_order.yaml"))$villages
village_order <- setNames(
  lapply(vorder_raw, function(x) as.integer(x[[2]])),
  vapply(vorder_raw, function(x) as.character(x[[1]]), character(1))
)

# --- cohort ---
cohort <- build_cohort(long, village_order, params)

# --- mortality from trial data ---
# person-cycle table: state per wave, death flag, CVD-cause flag.
# at_risk drops the cycles after death, so a person who dies at wave 2 no
# longer contributes waves 3-6 to the denominator. dead, at_risk and cvd_death
# are all built in clean_long() from f_muerte and c_muerte.
death_dt <- long[wave >= 1 & at_risk,
                 .(codigo, wave,
                   state = "Healthy",                     # refined below
                   dead, cvd_death)]
mortality <- mortality_rates(death_dt)

# --- base case ---
cea <- evaluate_model(params, cohort, mortality)
message("Base case complete.")

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
  specs <- psa_specs(mortality = mortality, base = params)
  psa <- run_psa(specs, cohort, mortality, params,
                 n_sim = psa_n, seed = psa_seed)
  message("PSA complete (", psa_n, " draws, seed ", psa_seed, ").")
} else {
  message("PSA skipped. Set SALT_PSA_N to enable.")
}

# --- cache outputs ---
if (!dir.exists(here::here("output"))) dir.create(here::here("output"))
saveRDS(list(params = params, cea = cea, owsa = owsa, psa = psa),
        here::here("output", "cea_results.rds"))

message("Results written to output/cea_results.rds")
print(cea$summary)
