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
#
# The three mortality inputs may be scalars or n_i x n_t matrices. They are
# matrices whenever mortality varies with age, which a lifetime horizon
# requires; they are scalars in fixtures and in the trial-horizon scenario.
# at_cycle() accepts both so neither caller has to know which it holds.
probs <- function(v_state, si, t, arm, params) {
  v_n <- params$state_names
  n_i <- length(v_state)
  P <- matrix(0, n_i, length(v_n), dimnames = list(NULL, v_n))
  mort <- si$mortality

  is_H <- v_state == "Healthy"
  if (any(is_H)) {
    p_cvd   <- si$cvd_risk[[arm]][is_H, t]
    r_cvd   <- -log(1 - p_cvd)
    r_death <- -log(1 - at_cycle(mort$p_background, is_H, t))
    r_tot   <- r_cvd + r_death
    p_exit  <- 1 - exp(-r_tot)
    share   <- ifelse(r_tot > 0, r_cvd / r_tot, 0)
    P[is_H, "CVD"]     <- p_exit * share
    P[is_H, "Dead"]    <- p_exit * (1 - share)
    P[is_H, "Healthy"] <- 1 - p_exit
  }

  is_C <- v_state == "CVD"
  if (any(is_C)) {
    p_fatal <- at_cycle(mort$p_cvd_fatal, is_C, t)
    P[is_C, "Dead"]    <- p_fatal
    P[is_C, "PostCVD"] <- 1 - p_fatal
  }

  is_P <- v_state == "PostCVD"
  if (any(is_P)) {
    p_pdeath <- at_cycle(mort$p_postcvd, is_P, t)
    P[is_P, "Dead"]    <- p_pdeath
    P[is_P, "PostCVD"] <- 1 - p_pdeath
  }

  is_D <- v_state == "Dead"
  if (any(is_D)) P[is_D, "Dead"] <- 1

  P
}

# at_cycle: read a mortality input for the individuals in `idx` at cycle `t`.
#
# A scalar is returned as-is and recycles across those individuals, which is
# what the trial-horizon scenario and every test fixture rely on. A matrix is
# subset to the selected rows and this cycle's column, which is what an
# age-varying lifetime mortality table needs. Keeping both behind one accessor
# is what let the lifetime mortality change land without rewriting fixtures.
at_cycle <- function(x, idx, t) {
  if (is.matrix(x)) x[idx, t] else x
}
