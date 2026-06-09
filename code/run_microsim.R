#-----------------------
# Microsimulation orchestrator
#-----------------------

here::i_am("salt_results.qmd")

# --- load modules ---
source(here::here("functs", "cleaning.R"))
source(here::here("functs", "parameters.R"))
source(here::here("functs", "globorisk_risk.R"))
source(here::here("functs", "mortality.R"))
source(here::here("functs", "cohort.R"))
source(here::here("functs", "transition.R"))
source(here::here("functs", "costs.R"))
source(here::here("functs", "effects.R"))
source(here::here("functs", "microsim.R"))
source(here::here("functs", "cea.R"))

require(data.table)
require(yaml)

# --- parameters ---
params <- get_params()

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

# --- per-arm CVD risk matrices (Healthy individuals, per cycle) ---
cvd_risk_arm <- function(sbp_matrix) {
  n_i <- nrow(sbp_matrix); n_t <- ncol(sbp_matrix)
  out <- matrix(0, n_i, n_t)
  base <- cohort$risk[wave == 0][match(cohort$ids, codigo)]
  for (t in seq_len(n_t)) {
    dt_t <- data.table(
      sex_num  = base$sex_num,
      age      = base$age + t,          # age advances by ~1 yr/cycle
      sbp      = sbp_matrix[, t],
      smoking  = base$smoking,
      bmi      = base$bmi,
      diabetes = base$diabetes
    )
    out[, t] <- cvd_risk(dt_t, params)
  }
  out
}
cvd_risk_mat <- list(
  Intervention = cvd_risk_arm(cohort$sbp_intervention),
  Control      = cvd_risk_arm(cohort$sbp_control)
)

# --- mortality from trial data ---
# person-cycle table: state per wave, death flag, CVD-cause flag
death_dt <- long[wave >= 1, .(
  state     = "Healthy",                                  # refined below
  dead      = as.integer(!is.na(f_muerte) &
                           year(f_muerte) > 0),
  cvd_death = as.integer(grepl("cardio|infarto|derrame",
                               tolower(as.character(c_muerte))))
), by = .(codigo, wave)]
mortality <- mortality_rates(death_dt)

# --- assemble sim_input ---
sim_input <- list(
  ids       = cohort$ids,
  state_0   = cohort$state_0,
  cvd_risk  = cvd_risk_mat,
  mortality = mortality,
  treated   = cohort$treated,
  htn       = cohort$htn
)

# --- run both arms ---
res <- lapply(params$arms, function(a)
  MicroSim(sim_input, params, a, probs, costs, effects))
names(res) <- params$arms

# --- cost-effectiveness ---
cea <- run_cea(res)

# --- cache outputs ---
if (!dir.exists(here::here("output"))) dir.create(here::here("output"))
saveRDS(list(params = params, res = res, cea = cea),
        here::here("output", "cea_results.rds"))

message("Microsimulation complete. Results written to output/cea_results.rds")
print(cea$summary)
