#--------------------------------
# Model evaluation module
#--------------------------------

print("Loading model evaluation functions")

# arm_risk: uncalibrated per-cycle CVD probability matrix for one arm.
# `sbp_matrix` is n_i x n_t; `base` is cohort$base (baseline risk factors, one
# row per individual in id order). Age advances one year per cycle.
#
# The risk_calib multiplier is deliberately NOT applied here. Keeping it out
# means this matrix depends only on quantities the PSA never samples, so
# run_psa() can build the Intervention arm's matrix once and reuse it across
# every draw -- which matters, because this loop is the dominant cost of a PSA.
arm_risk <- function(sbp_matrix, base, params) {
  require(data.table)
  n_i <- nrow(sbp_matrix)
  n_t <- ncol(sbp_matrix)
  out <- matrix(0, n_i, n_t)
  for (t in seq_len(n_t)) {
    dt_t <- data.table(
      sex_num  = base$sex_num,
      age      = base$age + t,
      sbp      = sbp_matrix[, t],
      smoking  = base$smoking,
      bmi      = base$bmi,
      diabetes = base$diabetes
    )
    out[, t] <- cvd_risk(dt_t, params)
  }
  out
}

# apply_calib: rescale a per-cycle probability by a cumulative-hazard
# multiplier. calib = 1 is the identity. Applied outside cvd_risk() so that
# function stays a pure statement of the risk equation; it composes with the
# GBD age multiplier because both act on the same hazard scale.
apply_calib <- function(p, calib) {
  if (calib == 1) return(p)
  1 - (1 - p)^calib
}

# evaluate_model: the single parameters -> ICER path.
#   params    - get_params() list, possibly perturbed by OWSA or PSA
#   cohort    - build_cohort() output (fixed across draws: observed SBP,
#               crossover schedule, baseline states and risk factors)
#   mortality - list of p_background, p_cvd_fatal, p_postcvd. Passed in rather
#               than read from `cohort` so the PSA can substitute sampled
#               values for the trial point estimates.
#   risk_intervention - optional uncalibrated Intervention-arm risk matrix from
#               a previous arm_risk() call. Supplying it skips the rebuild;
#               it is safe because that matrix never depends on delta_sbp or
#               risk_calib. Omit it and the matrix is built fresh.
# Returns run_cea() output.
#
# The Control arm's SBP, hypertension flag and risk matrix are all rebuilt from
# params$delta_sbp on every call. Reusing cohort$sbp_control would silently pin
# the treatment effect at its base-case value and make every sensitivity
# analysis a no-op for the one parameter that matters most.
#
# Both arms are run from the same RNG seed (MicroSim seeds per call), so
# individual-level Monte Carlo noise is common to the arms and largely cancels
# in the incremental estimate.
evaluate_model <- function(params, cohort, mortality, risk_intervention = NULL) {
  require(data.table)
  require(purrr)
  calib <- if (is.null(params$risk_calib)) 1 else params$risk_calib

  sbp <- list(
    Intervention = cohort$sbp_intervention,
    Control      = cohort$sbp_intervention +
                     cohort$treated * abs(params$delta_sbp)
  )

  raw_i <- if (is.null(risk_intervention)) {
    arm_risk(sbp$Intervention, cohort$base, params)
  } else {
    risk_intervention
  }
  raw_c <- arm_risk(sbp$Control, cohort$base, params)

  sim_input <- list(
    ids       = cohort$ids,
    state_0   = cohort$state_0,
    cvd_risk  = list(Intervention = apply_calib(raw_i, calib),
                     Control      = apply_calib(raw_c, calib)),
    mortality = mortality,
    treated   = cohort$treated,
    # cohort$dbp is arm-invariant: only SBP carries the treatment effect
    htn       = map(sbp, \(m) htn_flag(m, params, cohort$dbp))
  )

  res <- map(set_names(params$arms), \(a)
    MicroSim(sim_input, params, a, probs, costs, effects))

  run_cea(res)
}
