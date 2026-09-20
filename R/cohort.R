#--------------------------------
# Cohort construction module
#--------------------------------

print("Loading cohort construction functions")

# build_cohort: turn cleaned long data into the cohort structure used by the
# microsimulation. `long` is one row per individual x wave. `village_order`
# is a named list mapping village code -> crossover wave (1..6). `params`
# is the get_params() list.
#
# Steps: flag prevalent CVD at wave 0, seed baseline state, LOCF-impute
# missing risk factors, assign treatment status per cycle from the crossover
# schedule, and build the intervention/control SBP counterfactuals.
build_cohort <- function(long, village_order, params) {
  require(data.table)
  require(purrr)
  long <- copy(long)
  setorder(long, codigo, wave)
  # n_trial is the observed width (one column per follow-up wave); n_t is the
  # full modelled horizon, which for a lifetime run is much wider. Measurement
  # matrices are built at n_trial and then projected out to n_t.
  n_trial <- params$n_trial
  n_t     <- params$n_t
  ids <- sort(unique(long$codigo))

  # --- numeric risk-factor coding ---
  # levels come from english_labels() in cleaning.R: sexo Female/Male,
  # smoking1 Never/Smoker, db2 No/Yes. A label change upstream would silently
  # code these to a constant, so assert that each one actually varies.
  assert_level <- function(x, label, nm) {
    if (is.factor(x) && !label %in% levels(x)) {
      stop(nm, ' has no level "', label, '" (levels: ',
           paste(levels(x), collapse = ", "),
           '); check english_labels() in cleaning.R.')
    }
  }
  assert_level(long$sexo,     "Male",   "sexo")
  assert_level(long$smoking1, "Smoker", "smoking1")
  assert_level(long$db2,      "Yes",    "db2")

  long[, sex_num  := as.integer(sexo == "Male")]
  long[, smoking  := as.integer(smoking1 == "Smoker")]
  long[, diabetes := as.integer(db2 == "Yes")]

  # --- LOCF imputation of risk factors within person ---
  # dbp is imputed alongside the rest even though Globorisk office never reads
  # it: 71 participants have no follow-up blood pressure at all, and without
  # this the dbp matrix would carry NaN rows into the hypertension cost flag.
  rf_cols <- intersect(c("age", "sbp", "dbp", "bmi", "sex_num", "smoking",
                         "diabetes"),
                       names(long))
  long[, (rf_cols) := map(.SD, nafill, type = "locf"),
       .SDcols = rf_cols, by = codigo]
  long[, (rf_cols) := map(.SD, nafill, type = "nocb"),
       .SDcols = rf_cols, by = codigo]
  # cohort-median fallback for individuals with zero observations of a risk
  # factor across all waves (LOCF/NOCB can't fill those) — prevents NAs from
  # propagating into Globorisk and the transition matrix
  walk(rf_cols, \(v) {
    med <- stats::median(long[[v]], na.rm = TRUE)
    long[is.na(get(v)), (v) := med]
  })

  # --- prevalent CVD at wave 0 -> baseline state ---
  cvd_cols <- c("infarto", "derrame", "insuficiencia", "otracor")
  base <- long[wave == 0]
  base[, prevalent := reduce(map(.SD, \(x) x == "Yes"), `|`),
       .SDcols = cvd_cols]
  base[, state_0 := fifelse(prevalent %in% TRUE, "PostCVD", "Healthy")]
  state_0 <- base[match(ids, codigo), state_0]

  # --- treatment status per cycle (cycles 1..n_trial) ---
  # Deliberately only as wide as the trial. The crossover schedule is observed
  # data; whether exposure persists after the trial is a scenario choice, and
  # scenario choices belong on the single params -> ICER path in
  # evaluate_model() so that OWSA and PSA see them. Widening `treated` here
  # would pin the post-trial assumption at cohort-build time.
  cross <- map_int(long[match(ids, codigo), codigovilla],
                   \(v) village_order[[as.character(v)]])
  treated <- outer(cross, seq_len(n_trial), function(cw, t) t >= cw)

  # --- measurement matrices: rows = individuals, cols = cycles 1..n_trial ---
  # fun.aggregate collapses the rare codigo x wave duplicate (one such
  # case in the real data) by taking the first non-NA value
  wave_matrix <- function(col) {
    wide <- dcast(long[wave >= 1], codigo ~ wave, value.var = col,
                  fun.aggregate = function(x) {
                    x <- x[!is.na(x)]; if (length(x)) x[1] else NA_real_
                  })
    if (ncol(wide) - 1L != n_trial) {
      stop("Expected ", n_trial, " follow-up waves but found ", ncol(wide) - 1L,
           "; ", col, " matrix would be mis-sized.")
    }
    m <- unname(as.matrix(wide[match(ids, codigo), -1]))
    # row-mean fill: residual NAs occur when a participant is entirely missing
    # a wave row in the long table (rare; one such case in the trial data)
    na_idx <- which(is.na(m), arr.ind = TRUE)
    if (nrow(na_idx)) {
      row_means <- rowMeans(m, na.rm = TRUE)
      m[na_idx] <- row_means[na_idx[, 1L]]
    }
    m
  }

  sbp_intervention <- carry_forward(wave_matrix("sbp"), n_t)
  # DBP is observed, not counterfactual: the trial estimated delta on SBP only,
  # so the same matrix serves both arms. NULL when the column is absent (test
  # fixtures), which degrades htn_flag() to its systolic arm.
  dbp <- if ("dbp" %in% names(long)) carry_forward(wave_matrix("dbp"), n_t) else NULL

  # sbp_control and htn here are the BASE-CASE views only, kept for inspection
  # and for callers that want the cohort's own counterfactual. evaluate_model()
  # rebuilds both from params$delta_sbp and params$post_trial on every call;
  # reusing these would make every sensitivity analysis a no-op. `treated` is
  # padded to the horizon under the base-case "continue" assumption purely so
  # these two stay conformable with the projected SBP matrix.
  treated_base <- post_trial_treated(treated, n_t, params$post_trial)
  sbp_control <- sbp_intervention + treated_base * abs(params$delta_sbp)

  # --- hypertension cost flag on each arm's blood pressure ---
  htn <- list(Intervention = htn_flag(sbp_intervention, params, dbp),
              Control      = htn_flag(sbp_control,      params, dbp))

  # --- baseline risk factors, one row per id in `ids` order ---
  # evaluate_model() rebuilds the per-cycle risk matrices from this, so it is
  # pre-aligned here rather than left to each caller to re-subset and re-match
  base_rf <- long[wave == 0][match(ids, codigo),
                             .(sex_num, age, sbp, bmi, smoking, diabetes)]

  list(
    ids              = ids,
    state_0          = state_0,
    sbp_intervention = sbp_intervention,
    sbp_control      = sbp_control,
    dbp              = dbp,
    treated          = treated,
    htn              = htn,
    base             = base_rf,
    risk             = long          # full long table for cvd_risk()
  )
}

# htn_flag: hypertension flag driving the HTA management cost. Single
# definition shared by build_cohort() and evaluate_model(), which recomputes it
# per PSA draw because delta_sbp shifts the Control arm across the threshold.
#
# Both arms of the threshold rule are tested, so isolated diastolic
# hypertension is billed. `dbp_matrix` keeps its default so a caller without
# observed DBP still gets the systolic flag rather than an error; params stays
# in second position so existing positional calls keep working.
htn_flag <- function(sbp_matrix, params, dbp_matrix = NULL) {
  flag <- sbp_matrix >= params$htn_sbp
  if (is.null(dbp_matrix)) return(flag)
  stopifnot(identical(dim(dbp_matrix), dim(sbp_matrix)))
  flag | dbp_matrix >= params$htn_dbp
}

# carry_forward: widen a measurement matrix from the observed trial width to
# the full modelled horizon by repeating its last observed column.
#
# This is the post-trial projection rule, and it is deliberately the most
# assumption-light one available: each person holds their final observed value
# for the rest of the horizon. Two consequences are load-bearing.
#
# First, the whole post-trial gap between arms is then exactly delta_sbp, never
# an artefact of a drift model. Second, because no sampled parameter touches
# the Intervention arm's SBP, run_psa() and run_owsa() can keep building its
# uncalibrated risk matrix once and reusing it across every draw -- which at a
# ~197-cycle horizon is the difference between a tractable PSA and an
# intractable one. A sampled SBP-drift parameter would invalidate that cache
# silently, producing plausible numbers from a stale matrix.
#
# Age still varies over the projection: cvd_risk() advances it in arm_risk(),
# so absolute risk climbs even though the measured risk factors are frozen.
carry_forward <- function(m, n_t) {
  if (ncol(m) >= n_t) return(m)
  cbind(m, matrix(m[, ncol(m)], nrow(m), n_t - ncol(m)))
}

# post_trial_treated: extend the observed crossover schedule across the
# projected cycles according to the post-trial exposure scenario.
#
#   "continue" - everyone stays exposed for the rest of the horizon. By the
#                last trial wave every village has crossed over, so this is a
#                matrix of TRUE. Both the SBP gap and the intervention cost
#                persist.
#   "stop"     - exposure ends with the trial. The SBP gap closes at the first
#                projected cycle and no further intervention cost accrues.
#
# One matrix expresses both, because `treated` is the only object feeding both
# the effect (the Control arm's SBP in evaluate_model) and the cost (costs()).
# Keeping them on a single switch is what stops the two halves of a scenario
# drifting out of step. Events already averted during the trial stay averted
# under "stop"; only the forward-looking gap closes.
post_trial_treated <- function(treated, n_t, post_trial = "continue") {
  n_post <- n_t - ncol(treated)
  if (n_post <= 0L) return(treated)
  cbind(treated,
        matrix(identical(post_trial, "continue"), nrow(treated), n_post))
}
