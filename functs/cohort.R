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
  long <- copy(long)
  setorder(long, codigo, wave)
  n_t <- params$n_t
  ids <- sort(unique(long$codigo))

  # --- numeric risk-factor coding ---
  long[, sex_num  := as.integer(sexo == "Masculino")]
  long[, smoking  := as.integer(smoking1 == "Si")]
  long[, diabetes := as.integer(db2 == "Si")]

  # --- LOCF imputation of risk factors within person ---
  rf_cols <- c("age", "sbp", "bmi", "sex_num", "smoking", "diabetes")
  long[, (rf_cols) := lapply(.SD, nafill, type = "locf"),
       .SDcols = rf_cols, by = codigo]
  long[, (rf_cols) := lapply(.SD, nafill, type = "nocb"),
       .SDcols = rf_cols, by = codigo]
  # cohort-median fallback for individuals with zero observations of a risk
  # factor across all waves (LOCF/NOCB can't fill those) — prevents NAs from
  # propagating into Globorisk and the transition matrix
  for (col in rf_cols) {
    med <- stats::median(long[[col]], na.rm = TRUE)
    long[is.na(get(col)), (col) := med]
  }

  # --- prevalent CVD at wave 0 -> baseline state ---
  cvd_cols <- c("infarto", "derrame", "insuficiencia", "otracor")
  base <- long[wave == 0]
  base[, prevalent := Reduce(`|`, lapply(.SD, function(x) x == "Si")),
       .SDcols = cvd_cols]
  base[, state_0 := fifelse(prevalent %in% TRUE, "PostCVD", "Healthy")]
  state_0 <- base[match(ids, codigo), state_0]

  # --- treatment status per cycle (cycles 1..n_t) ---
  cross <- vapply(long[match(ids, codigo), codigovilla],
                  function(v) village_order[[as.character(v)]], integer(1))
  treated <- outer(cross, seq_len(n_t), function(cw, t) t >= cw)

  # --- SBP matrices: rows = individuals, cols = cycles 1..n_t ---
  # fun.aggregate collapses the rare codigo x wave duplicate (one such
  # case in the real data) by taking the first non-NA SBP value
  sbp_wide <- dcast(long[wave >= 1], codigo ~ wave, value.var = "sbp",
                    fun.aggregate = function(x) {
                      x <- x[!is.na(x)]; if (length(x)) x[1] else NA_real_
                    })
  if (ncol(sbp_wide) - 1L != n_t) {
    stop("Expected ", n_t, " follow-up waves but found ", ncol(sbp_wide) - 1L,
         "; SBP matrix would be mis-sized.")
  }
  sbp_intervention <- unname(as.matrix(sbp_wide[match(ids, codigo), -1]))
  # row-mean fill: residual NAs occur when a participant is entirely missing
  # a wave row in the long table (rare; one such case in the trial data)
  na_idx <- which(is.na(sbp_intervention), arr.ind = TRUE)
  if (nrow(na_idx)) {
    row_means <- rowMeans(sbp_intervention, na.rm = TRUE)
    sbp_intervention[na_idx] <- row_means[na_idx[, 1L]]
  }
  sbp_control <- sbp_intervention + treated * abs(params$delta_sbp)

  # --- hypertension cost flag (>=140) on each arm's SBP ---
  htn <- list(Intervention = sbp_intervention >= 140,
              Control      = sbp_control      >= 140)

  list(
    ids              = ids,
    state_0          = state_0,
    sbp_intervention = sbp_intervention,
    sbp_control      = sbp_control,
    treated          = treated,
    htn              = htn,
    risk             = long          # full long table for cvd_risk()
  )
}
