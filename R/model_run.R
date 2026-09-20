#--------------------------------
# Model evaluation module
#--------------------------------

print("Loading model evaluation functions")

# arm_risk: uncalibrated per-cycle CVD probability matrix for one arm.
# `sbp_matrix` is n_i x n_t; `base` is cohort$base (baseline risk factors, one
# row per individual in id order).
#
# Time axis: cycle t scores each individual at age_0 + (t - 1) * cycle_length,
# i.e. their age at the START of the cycle. The index t is NOT a year -- a
# cycle is cycle_length years -- so advancing age by t would run the cohort's
# ageing 1 / cycle_length times too fast (2.4x at the trial's 5/12 cycle, and
# 197 years in 82 over a lifetime horizon).
#
# Every cycle is scored in ONE globorisk() call rather than one call per cycle.
# globorisk() is vectorised, and at a lifetime horizon the per-cycle loop meant
# ~197 calls per arm per draw. as.vector() on a matrix is column-major, which
# is exactly cycle-major order, so the result reshapes back without a permute.
#
# The risk_calib multiplier is deliberately NOT applied here. Keeping it out
# means this matrix depends only on quantities the PSA never samples, so
# run_psa() can build the Intervention arm's matrix once and reuse it across
# every draw. That reuse is load-bearing at 197 cycles: it is valid only while
# nothing varied reaches this function, which is why the post-trial SBP
# projection carries the last observed value forward rather than introducing a
# sampled drift parameter.
arm_risk <- function(sbp_matrix, base, params) {
  require(data.table)
  n_i <- nrow(sbp_matrix)
  n_t <- ncol(sbp_matrix)
  cl  <- params$cycle_length

  dt_all <- data.table(
    sex_num  = rep(base$sex_num,  times = n_t),
    age      = rep(base$age,      times = n_t) +
                 rep((seq_len(n_t) - 1L) * cl, each = n_i),
    sbp      = as.vector(sbp_matrix),
    smoking  = rep(base$smoking,  times = n_t),
    bmi      = rep(base$bmi,      times = n_t),
    diabetes = rep(base$diabetes, times = n_t)
  )
  matrix(cvd_risk(dt_all, params), n_i, n_t)
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

  # The observed crossover schedule, extended across the projected cycles
  # according to params$post_trial. This one matrix carries BOTH halves of the
  # exposure scenario: the Control arm's SBP gap below, and the intervention
  # cost in costs(). Building it here rather than in build_cohort() is what
  # lets a scenario be switched without rebuilding the cohort, and what makes
  # the switch visible to every caller on this path.
  treated <- post_trial_treated(cohort$treated, params$n_t, params$post_trial)

  # The cohort is built at params$n_t, so a params list whose horizon has since
  # changed would otherwise fail deep inside a matrix sum with "non-conformable
  # arrays". Most likely cause: set_horizon() was called after build_cohort(),
  # or a fixture raised n_t without rebuilding its matrices.
  if (ncol(cohort$sbp_intervention) != params$n_t) {
    stop("cohort was built for ", ncol(cohort$sbp_intervention),
         " cycles but params$n_t is ", params$n_t,
         "; call set_horizon() before build_cohort().")
  }

  sbp <- list(
    Intervention = cohort$sbp_intervention,
    Control      = cohort$sbp_intervention + treated * abs(params$delta_sbp)
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
    # Level multipliers and the post-CVD excess hazard are applied here, not
    # baked into `mortality`, for the same reason risk_calib is applied outside
    # arm_risk(): it keeps the GBD matrices independent of every sampled
    # parameter, so run_psa() builds them once per run instead of once per draw.
    mortality = lifetime_mortality(mortality, params),
    treated   = treated,
    # age at baseline, carried so MicroSim() can turn a death cycle into an
    # age at death and look up the reference life expectancy for YLL
    age_0     = cohort$base$age,
    # cohort$dbp is arm-invariant: only SBP carries the treatment effect
    htn       = map(sbp, \(m) htn_flag(m, params, cohort$dbp))
  )

  res <- map(set_names(params$arms), \(a)
    MicroSim(sim_input, params, a, probs, costs, effects))

  run_cea(res)
}
