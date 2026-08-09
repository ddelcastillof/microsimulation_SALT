#--------------------------------
# Transition probability module
#--------------------------------

print("Loading transition functions")

# probs: build the per-individual transition probability matrix for cycle t.
#   v_state - character vector of current states (length n_i)
#   si      - sim_input list (see plan contract)
#   t       - cycle index (1..n_t)
#   arm     - "Intervention" or "Control"
#   params  - get_params() list
# CVD is a one-cycle tunnel state. Healthy faces competing risks of an
# incident CVD event and non-CVD death, combined on the rate scale so the
# resulting probabilities are always valid.
probs <- function(v_state, si, t, arm, params) {
  v_n <- params$state_names
  n_i <- length(v_state)
  P <- matrix(0, n_i, length(v_n), dimnames = list(NULL, v_n))
  mort <- si$mortality

  is_H <- v_state == "Healthy"
  if (any(is_H)) {
    p_cvd   <- si$cvd_risk[[arm]][is_H, t]
    r_cvd   <- -log(1 - p_cvd)
    r_death <- -log(1 - mort$p_background)
    r_tot   <- r_cvd + r_death
    p_exit  <- 1 - exp(-r_tot)
    share   <- ifelse(r_tot > 0, r_cvd / r_tot, 0)
    P[is_H, "CVD"]     <- p_exit * share
    P[is_H, "Dead"]    <- p_exit * (1 - share)
    P[is_H, "Healthy"] <- 1 - p_exit
  }

  is_C <- v_state == "CVD"
  if (any(is_C)) {
    P[is_C, "Dead"]    <- mort$p_cvd_fatal
    P[is_C, "PostCVD"] <- 1 - mort$p_cvd_fatal
  }

  is_P <- v_state == "PostCVD"
  if (any(is_P)) {
    P[is_P, "Dead"]    <- mort$p_postcvd
    P[is_P, "PostCVD"] <- 1 - mort$p_postcvd
  }

  is_D <- v_state == "Dead"
  if (any(is_D)) P[is_D, "Dead"] <- 1

  P
}
