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
  n_t <- params$n_t
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

  # --- treatment status per cycle (cycles 1..n_t) ---
  cross <- map_int(long[match(ids, codigo), codigovilla],
                   \(v) village_order[[as.character(v)]])
  treated <- outer(cross, seq_len(n_t), function(cw, t) t >= cw)

  # --- measurement matrices: rows = individuals, cols = cycles 1..n_t ---
  # fun.aggregate collapses the rare codigo x wave duplicate (one such
  # case in the real data) by taking the first non-NA value
  wave_matrix <- function(col) {
    wide <- dcast(long[wave >= 1], codigo ~ wave, value.var = col,
                  fun.aggregate = function(x) {
                    x <- x[!is.na(x)]; if (length(x)) x[1] else NA_real_
                  })
    if (ncol(wide) - 1L != n_t) {
      stop("Expected ", n_t, " follow-up waves but found ", ncol(wide) - 1L,
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

  sbp_intervention <- wave_matrix("sbp")
  sbp_control <- sbp_intervention + treated * abs(params$delta_sbp)
  # DBP is observed, not counterfactual: the trial estimated delta on SBP only,
  # so the same matrix serves both arms. NULL when the column is absent (test
  # fixtures), which degrades htn_flag() to its systolic arm.
  dbp <- if ("dbp" %in% names(long)) wave_matrix("dbp") else NULL

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
