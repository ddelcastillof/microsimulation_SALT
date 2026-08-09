#--------------------------------
# Costs module
#--------------------------------

print("Loading cost functions")

# costs: per-individual cost accrued in cycle t given the state entered.
# Dead accrues nothing (including no intervention cost). The intervention
# cost applies to treated person-time in the Intervention arm only.
#
# Every value in params$costs is already expressed per cycle, so nothing here
# is rescaled by cycle_length. Supplying an annual figure would understate the
# cost by a factor of cycle_length.
costs <- function(v_state, si, t, arm, params) {
  cst <- params$costs
  n_i <- length(v_state)
  c_vec <- numeric(n_i)

  c_vec[v_state == "Healthy"] <- cst$healthy
  c_vec[v_state == "CVD"]     <- cst$cvd_event
  c_vec[v_state == "PostCVD"] <- cst$postcvd
  c_vec[v_state == "Dead"]    <- 0

  # HTA management add-on for hypertensive Healthy individuals
  htn <- si$htn[[arm]][, t]
  hta_idx <- v_state == "Healthy" & htn
  c_vec[hta_idx] <- c_vec[hta_idx] + cst$hta

  # intervention cost on treated, living person-time
  if (arm == "Intervention") {
    treated <- si$treated[, t] & v_state != "Dead"
    c_vec[treated] <- c_vec[treated] + cst$intervention
  }
  c_vec
}
