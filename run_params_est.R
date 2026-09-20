#-----------------------
# Parameter estimation orchestrator
#-----------------------
#
# Estimates the trial's own intervention effects -- blood pressure,
# hypertension incidence, mortality, CVD -- and caches everything to
# output/params_est.rds for params_est.qmd to render.
#
# The blood-pressure and hypertension models replicate Bernabe-Ortiz et al.
# (Nature Medicine 2020) and act as a validation gate on clean_long(). The
# death and CVD models are new; the paper never analysed them.
#
# Runs in well under a minute, so there are no environment-variable knobs.

here::i_am("salt_results.qmd")

source(here::here("R", "cleaning.R"))
source(here::here("R", "params_est.R"))

require(data.table)

res <- run_params_est()

message("\n--- cohort ---")
message(res$long_n$rows, " rows, ", res$long_n$ids, " people, ",
        res$long_n$households, " households")

message("\n--- wide vs long event reconciliation ---")
print(res$wide_events$recon)

message("\n--- blood pressure (paper Table 2) ---")
print(res$bp[, .(outcome, adjust, estimate = round(estimate, 3),
                 low = round(low, 2), high = round(high, 2))])

message("\n--- incidence, gap-time axis, gamma village frailty ---")
print(res$incidence[, .(label, adjust, events, py = round(py),
                        hr = round(hr, 3), low = round(low, 3),
                        high = round(high, 3),
                        converged = is.na(warning))])

message("\n--- time-axis sensitivity (why gap time, not calendar) ---")
print(res$axis_check[, .(axis, hr = round(hr, 3), low = round(low, 3),
                         high = round(high, 3))])

message("\n--- crude rates ---")
print(res$rates[, .(endpoint, arm, events, py = round(py, 1),
                    rate_100py = round(rate_100py, 2))])

message("\n--- replication against published values ---")
print(res$replication[, .(quantity,
                          paper = sprintf("%.2f (%.2f, %.2f)", published,
                                          published_low, published_high),
                          ours  = sprintf("%.3f (%.2f, %.2f)", estimate,
                                          low, high),
                          diff  = round(diff, 3), verdict)])

message("\nResults written to output/params_est.rds")
