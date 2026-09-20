#------------------
# Microsimulation module tests
#------------------
pacman::p_load(testthat, data.table)

source(here::here("R", "parameters.R"))

test_that("get_params returns a complete config list", {
  p <- get_params()
  expect_type(p, "list")
  expect_identical(p$n_t, 6L)
  expect_identical(p$state_names, c("Healthy", "CVD", "PostCVD", "Dead"))
  # one cycle = one inter-visit interval; the trial measured every 5 months
  expect_equal(p$cycle_length, 5 / 12)
  expect_true(all(c("d_c", "d_e", "arms", "delta_sbp", "dw",
                    "utility", "costs", "globorisk", "seed") %in% names(p)))
  expect_equal(p$globorisk$version, "office")
})

test_that("get_params delta_sbp is configurable", {
  expect_equal(get_params(delta_sbp = -8)$delta_sbp, -8)
})

# --- horizon: trial rhythm first, then projection to the lifetime cap -------

test_that("get_params defaults to the trial horizon and continued exposure", {
  p <- get_params()
  # n_t defaults to the 6 observed trial cycles so every fixture keeps working;
  # the lifetime horizon is opted into through set_horizon()
  expect_identical(p$n_t, 6L)
  expect_identical(p$n_trial, 6L)
  expect_identical(p$post_trial, "continue")
  expect_equal(p$max_age, 100)
  expect_gt(p$hr_postcvd, 1)
  expect_equal(p$mort_calib, 1)
  expect_equal(p$cf_calib, 1)
})

test_that("get_params rejects an unknown post-trial scenario", {
  expect_error(get_params(post_trial = "sometimes"), "arg")
})

test_that("set_horizon sizes n_t from the youngest participant and the age cap", {
  p <- get_params()
  # youngest is 30, cap 100 -> 70 years -> 70 / (5/12) = 168 cycles
  long <- data.table(codigo = c("a", "b", "c"), wave = 0L, age = c(30, 55, 80))
  out <- set_horizon(p, long)
  expect_identical(out$n_t, 168L)
  expect_equal(out$horizon_years, 168 * 5 / 12)
  # the trial phase is untouched, and the projection is everything after it
  expect_identical(out$n_trial, p$n_trial)
  expect_gt(out$n_t, out$n_trial)
})

test_that("set_horizon honours an explicit max_age and never shortens the trial", {
  p <- get_params()
  long <- data.table(codigo = c("a", "b"), wave = 0L, age = c(70, 90))
  expect_identical(set_horizon(p, long, max_age = 80)$n_t, 24L)   # 10 yr / (5/12)
  # a cap at or below the youngest age must still leave the observed trial
  # cycles intact rather than producing a zero-width model
  expect_identical(set_horizon(p, long, max_age = 60)$n_t, p$n_trial)
})

source(here::here("R", "globorisk_risk.R"))

test_that("risk10_to_cycle converts a 10-year risk to a per-cycle probability", {
  # 10-year risk of 0 -> 0; risk in (0,1) stays in (0,1); monotonic
  expect_equal(risk10_to_cycle(0, 5 / 6), 0)
  p_lo <- risk10_to_cycle(0.10, 5 / 6)
  p_hi <- risk10_to_cycle(0.40, 5 / 6)
  expect_true(p_lo > 0 && p_lo < 1)
  expect_true(p_hi > p_lo)
})

test_that("risk10_to_cycle for a full cycle-decade matches the 10-year risk", {
  # ten years of cycle_length = 10 should recover the original risk
  expect_equal(risk10_to_cycle(0.25, 10), 0.25, tolerance = 1e-8)
})

# --- GBD age extrapolation outside the Globorisk 40-74 window --------------
pacman::p_load(globorisk)

test_that("get_params enables GBD age extrapolation by default", {
  expect_true(get_params()$globorisk$extrapolate)
})

test_that("gbd_age_multiplier is exactly 1 across the validated 40-74 range", {
  ages <- c(40, 45, 55, 65, 74)
  expect_equal(gbd_age_multiplier(ages, rep(1L, length(ages))),
               rep(1, length(ages)))
  expect_equal(gbd_age_multiplier(ages, rep(0L, length(ages))),
               rep(1, length(ages)))
})

test_that("gbd_age_multiplier scales under-40 down and over-74 up", {
  expect_true(all(gbd_age_multiplier(c(25, 30, 35), rep(1L, 3)) < 1))
  expect_true(all(gbd_age_multiplier(c(75, 80), rep(1L, 2)) > 1))
})

test_that("gbd_age_multiplier rises monotonically with age", {
  m <- gbd_age_multiplier(c(22, 27, 32, 37, 42, 77, 82), rep(1L, 7))
  expect_true(all(diff(m) > 0))
})

test_that("gbd_age_multiplier is sex-specific above the range", {
  expect_false(isTRUE(all.equal(gbd_age_multiplier(80, 1L),
                                gbd_age_multiplier(80, 0L))))
})

test_that("gbd_age_multiplier collapses the sparse 85+ bands into one", {
  # 90-94 and 95+ hold ~50 trial person-waves between them; collapsing avoids
  # applying the largest multipliers to the smallest cells
  m <- gbd_age_multiplier(c(85, 90, 95, 100), rep(1L, 4))
  expect_equal(m, rep(m[1], 4))
})

test_that("gbd_age_multiplier propagates NA age", {
  expect_true(is.na(gbd_age_multiplier(NA_real_, 1L)))
})

make_risk_dt <- function(age, sex_num = 1L) {
  data.table(sex_num = sex_num, age = age, sbp = 130,
             smoking = 0L, bmi = 27, diabetes = 0L)
}

globorisk_at <- function(age, sex_num = 1L, p = get_params()) {
  globorisk(sex = sex_num, age = age, sbp = 130, tc = NA_real_, dm = 0L,
            smk = 0L, bmi = 27, iso = p$globorisk$iso,
            year = p$globorisk$year, version = p$globorisk$version,
            type = "risk")
}

test_that("cvd_risk is unchanged inside the validated age range", {
  p <- get_params()
  expect_equal(cvd_risk(make_risk_dt(60), p),
               risk10_to_cycle(globorisk_at(60), p$cycle_length))
})

test_that("cvd_risk applies the multiplier on the cumulative-hazard scale", {
  p <- get_params()
  m <- gbd_age_multiplier(30, 1L)
  expected <- risk10_to_cycle(1 - (1 - globorisk_at(40))^m, p$cycle_length)
  expect_equal(cvd_risk(make_risk_dt(30), p), expected)
})

test_that("cvd_risk scales an over-74 individual above the plain clamp", {
  p <- get_params()
  expect_gt(cvd_risk(make_risk_dt(85), p),
            risk10_to_cycle(globorisk_at(74), p$cycle_length))
})

test_that("cvd_risk with extrapolate disabled reproduces the plain age clamp", {
  p <- get_params()
  p$globorisk$extrapolate <- FALSE
  expect_equal(cvd_risk(make_risk_dt(30), p),
               risk10_to_cycle(globorisk_at(40), p$cycle_length))
  expect_warning(over74 <- cvd_risk(make_risk_dt(85), p),
                 "above Globorisk age range")
  expect_equal(over74, risk10_to_cycle(globorisk_at(74), p$cycle_length))
})

test_that("cvd_risk keeps the SBP gradient intact outside the age range", {
  p <- get_params()
  lo <- copy(make_risk_dt(30)); lo[, sbp := 120]
  hi <- copy(make_risk_dt(30)); hi[, sbp := 150]
  expect_gt(cvd_risk(hi, p), cvd_risk(lo, p))
})

source(here::here("R", "mortality.R"))

test_that("mortality_rates estimates pooled per-cycle death probabilities", {
  # synthetic person-cycle table: state, dead (0/1), cvd_death (0/1)
  dt <- data.table(
    state     = c(rep("Healthy", 100), rep("CVD", 10), rep("PostCVD", 20)),
    dead      = c(rep(0, 96), rep(1, 4),   rep(c(1, 0), c(3, 7)), rep(0, 18), 1, 1),
    cvd_death = c(rep(0, 96), 1, 1, 0, 0,  rep(c(1, 0), c(3, 7)), rep(0, 20))
  )
  m <- mortality_rates(dt)
  expect_equal(m$p_background, 2 / 100)   # 4 healthy deaths, 2 non-CVD
  expect_equal(m$p_cvd_fatal, 3 / 10)     # 3 of 10 CVD events fatal
  expect_equal(m$p_postcvd, 2 / 20)       # 2 of 20 post-CVD person-cycles
})

test_that("mortality_rates returns 0 when a state has no person-time", {
  dt <- data.table(state = rep("Healthy", 10), dead = 0, cvd_death = 0)
  m <- mortality_rates(dt)
  expect_equal(m$p_cvd_fatal, 0)
  expect_equal(m$p_postcvd, 0)
})

make_death_fixture <- function() {
  data.table(
    state     = c(rep("Healthy", 100), rep("CVD", 10), rep("PostCVD", 20)),
    dead      = c(rep(0, 96), rep(1, 4),   rep(c(1, 0), c(3, 7)), rep(0, 18), 1, 1),
    cvd_death = c(rep(0, 96), 1, 1, 0, 0,  rep(c(1, 0), c(3, 7)), rep(0, 20))
  )
}

test_that("mortality_rates reports the event counts and denominators", {
  # PSA draws these as Beta(events, n - events); the counts must travel with
  # the probabilities so no dispersion parameter has to be invented
  m <- mortality_rates(make_death_fixture())
  expect_s3_class(m$counts, "data.table")
  expect_equal(m$counts$param, c("p_background", "p_cvd_fatal", "p_postcvd"))
  expect_equal(m$counts$events, c(2L, 3L, 2L))
  expect_equal(m$counts$n, c(100L, 10L, 20L))
})

test_that("mortality_rates counts reproduce the reported probabilities", {
  m <- mortality_rates(make_death_fixture())
  expect_equal(m$counts$events / m$counts$n,
               c(m$p_background, m$p_cvd_fatal, m$p_postcvd))
})

test_that("mortality_rates reports zero denominators for absent states", {
  m <- mortality_rates(data.table(state = rep("Healthy", 10),
                                  dead = 0, cvd_death = 0))
  expect_equal(m$counts$n, c(10L, 0L, 0L))
  expect_equal(m$counts$events, c(0L, 0L, 0L))
})

source(here::here("R", "cohort.R"))

make_long_fixture <- function() {
  # 2 people x 7 waves. Person A: CVD-free. Person B: prior infarto at wave 0.
  CJ_ids <- CJ(codigo = c("A", "B"), wave = 0:6)
  dt <- as.data.table(CJ_ids)
  dt[, codigovilla := "020"]
  dt[, sexo := ifelse(codigo == "A", "Female", "Male")]
  dt[, age := 55L + wave]
  dt[, sbp := ifelse(codigo == "A", 150, 135)]
  # A is systolic-hypertensive only, B is diastolic-hypertensive only, so
  # htn_flag() has to test both arms to flag them
  dt[, dbp := ifelse(codigo == "A", 75, 95)]
  dt[, bmi := 27]
  dt[, smoking1 := "Never"]
  dt[, db2 := "No"]
  dt[, c("infarto", "derrame", "insuficiencia", "otracor") := "No"]
  dt[codigo == "B" & wave == 0, infarto := "Yes"]
  dt[]
}

test_that("build_cohort seeds prevalent CVD to PostCVD and others to Healthy", {
  vorder <- list(`020` = 1L)   # village 020 crosses over at wave 1
  ch <- build_cohort(make_long_fixture(), vorder, get_params())
  expect_equal(ch$state_0[ch$ids == "A"], "Healthy")
  expect_equal(ch$state_0[ch$ids == "B"], "PostCVD")
})

test_that("build_cohort builds the control SBP counterfactual", {
  vorder <- list(`020` = 1L)
  p <- get_params(delta_sbp = -6)
  ch <- build_cohort(make_long_fixture(), vorder, p)
  # treated person-time: village 020 crosses at wave 1, so cycles 1..6 treated
  # control SBP = observed SBP + |delta_sbp| on treated person-time
  expect_equal(ch$sbp_intervention[1, ], rep(150, 6))   # person A, 6 cycles
  expect_equal(ch$sbp_control[1, ], rep(150 + 6, 6))
  # person B (row 2) confirms matrix-row alignment, not just the arithmetic
  expect_equal(ch$sbp_intervention[2, ], rep(135, 6))
  expect_equal(ch$sbp_control[2, ], rep(135 + 6, 6))
})

test_that("build_cohort returns baseline risk factors aligned to ids", {
  vorder <- list(`020` = 1L)
  ch <- build_cohort(make_long_fixture(), vorder, get_params())
  # evaluate_model() consumes cohort$base directly, so it must be one row per
  # id in ids order -- not a full long table the caller has to re-subset
  expect_s3_class(ch$base, "data.table")
  expect_equal(nrow(ch$base), length(ch$ids))
  expect_true(all(c("sex_num", "age", "smoking", "bmi", "diabetes") %in%
                    names(ch$base)))
  expect_equal(ch$base$age, c(55L, 55L))   # both people are 55 at wave 0
})

test_that("htn_flag tests both arms of the blood-pressure rule", {
  p <- get_params()                    # 130 / 80
  sbp <- matrix(c(150, 120, 120), nrow = 3)
  dbp <- matrix(c( 75,  95,  75), nrow = 3)
  # systolic-only, diastolic-only, neither
  expect_equal(as.vector(htn_flag(sbp, p, dbp)), c(TRUE, TRUE, FALSE))
  # without a DBP matrix the flag degrades to the systolic arm rather than
  # erroring, so fixtures and cohorts lacking dbp still run
  expect_equal(as.vector(htn_flag(sbp, p)), c(TRUE, FALSE, FALSE))
  expect_error(htn_flag(sbp, p, matrix(75, nrow = 2)))
})

# --- lifetime projection: matrices extend, treatment schedule does not ------

test_that("build_cohort carries the last observed wave forward to the horizon", {
  vorder <- list(`020` = 1L)
  p <- get_params(); p$n_t <- 20L          # 6 observed cycles + 14 projected
  ch <- build_cohort(make_long_fixture(), vorder, p)

  expect_equal(ncol(ch$sbp_intervention), 20L)
  expect_equal(ncol(ch$dbp), 20L)
  # every projected column repeats the last OBSERVED column, so the post-trial
  # arm gap is purely delta_sbp and not an artefact of a projection rule
  last_obs <- ch$sbp_intervention[, p$n_trial]
  for (t in (p$n_trial + 1L):20L) {
    expect_equal(ch$sbp_intervention[, t], last_obs)
  }
  expect_equal(ch$dbp[, 20L], ch$dbp[, p$n_trial])
})

test_that("build_cohort leaves treated at the observed trial width", {
  # the crossover schedule is data; what happens after the trial is a scenario
  # choice, so it is applied in evaluate_model(), not baked into the cohort
  vorder <- list(`020` = 1L)
  p <- get_params(); p$n_t <- 20L
  ch <- build_cohort(make_long_fixture(), vorder, p)
  expect_equal(ncol(ch$treated), p$n_trial)
})

test_that("build_cohort still checks the observed waves against n_trial", {
  # a mis-sized measurement matrix must fail loudly rather than be padded out
  vorder <- list(`020` = 1L)
  p <- get_params(); p$n_t <- 20L; p$n_trial <- 4L
  expect_error(build_cohort(make_long_fixture(), vorder, p), "follow-up waves")
})

test_that("build_cohort flags isolated diastolic hypertension", {
  vorder <- list(`020` = 1L)
  ch <- build_cohort(make_long_fixture(), vorder, get_params())
  # A is 150/75 and B is 135/95: both hypertensive, B only via the diastolic
  # arm, which the systolic-only flag used to miss
  expect_equal(dim(ch$dbp), dim(ch$sbp_intervention))
  expect_true(all(ch$htn$Intervention))
})

source(here::here("R", "transition.R"))

make_sim_input <- function(n = 3, n_t = 6) {
  list(
    ids      = as.character(seq_len(n)),
    state_0  = rep("Healthy", n),
    cvd_risk = list(
      Intervention = matrix(0.05, n, n_t),
      Control      = matrix(0.08, n, n_t)),
    mortality = list(p_background = 0.02, p_cvd_fatal = 0.30, p_postcvd = 0.10),
    treated   = matrix(TRUE, n, n_t),
    htn       = list(Intervention = matrix(FALSE, n, n_t),
                     Control      = matrix(FALSE, n, n_t))
  )
}

test_that("probs returns rows that sum to 1 with no negative entries", {
  si <- make_sim_input()
  P <- probs(rep("Healthy", 3), si, t = 1, arm = "Control",
             params = get_params())
  expect_equal(unname(rowSums(P)), rep(1, 3))
  expect_true(all(P >= 0))
})

test_that("probs makes Dead absorbing and CVD a one-cycle tunnel", {
  si <- make_sim_input()
  p <- get_params()
  P_dead <- probs(c("Dead"), si, t = 1, arm = "Control", params = p)
  expect_equal(unname(P_dead[1, ]), c(0, 0, 0, 1))
  P_cvd <- probs(c("CVD"), si, t = 1, arm = "Control", params = p)
  expect_equal(unname(P_cvd[1, "Healthy"]), 0)
  expect_equal(unname(P_cvd[1, "Dead"]), si$mortality$p_cvd_fatal)
  expect_equal(unname(P_cvd[1, "PostCVD"]), 1 - si$mortality$p_cvd_fatal)
  P_post <- probs(c("PostCVD"), si, t = 1, arm = "Control", params = p)
  expect_equal(unname(P_post[1, "Dead"]), si$mortality$p_postcvd)
  expect_equal(unname(P_post[1, "PostCVD"]), 1 - si$mortality$p_postcvd)
})

test_that("probs gives the Control arm higher CVD risk than Intervention", {
  si <- make_sim_input()
  p <- get_params()
  P_i <- probs(rep("Healthy", 3), si, 1, "Intervention", p)
  P_c <- probs(rep("Healthy", 3), si, 1, "Control", p)
  expect_true(all(P_c[, "CVD"] > P_i[, "CVD"]))
})

source(here::here("R", "costs.R"))

test_that("costs assigns state costs and the intervention cost", {
  si <- make_sim_input(n = 4)
  p  <- get_params()
  p$costs$healthy <- 50   # non-zero so the Healthy-state assignment is genuinely tested
  v  <- c("Healthy", "CVD", "PostCVD", "Dead")
  cst <- costs(v, si, t = 1, arm = "Intervention", params = p)
  # params$costs values are already per cycle, so none is rescaled by
  # cycle_length -- see the timing conventions block in R/parameters.R
  expect_equal(cst[1], p$costs$healthy + p$costs$intervention)        # treated Healthy
  expect_equal(cst[2], p$costs$cvd_event + p$costs$intervention)
  expect_equal(cst[3], p$costs$postcvd + p$costs$intervention)
  expect_equal(cst[4], 0)                                            # Dead, no intervention
})

test_that("costs omits the intervention add-on in the Control arm", {
  si <- make_sim_input(n = 4)
  p  <- get_params()
  p$costs$healthy <- 50
  v  <- c("Healthy", "CVD", "PostCVD", "Dead")
  cst <- costs(v, si, t = 1, arm = "Control", params = p)
  expect_equal(cst[1], p$costs$healthy)             # no intervention cost
  expect_equal(cst[2], p$costs$cvd_event)
  expect_equal(cst[3], p$costs$postcvd)
  expect_equal(cst[4], 0)
})

test_that("costs adds the HTA management cost when hypertensive", {
  si <- make_sim_input(n = 1)
  si$htn$Control[1, 1] <- TRUE
  p  <- get_params()
  p$costs$healthy <- 50   # non-zero so the base cost is distinguishable from the HTA add-on
  cst <- costs("Healthy", si, t = 1, arm = "Control", params = p)
  expect_equal(cst, p$costs$healthy + p$costs$hta)
})

source(here::here("R", "effects.R"))

test_that("effects returns per-cycle YLD and QALY by state", {
  p  <- get_params()
  cl <- p$cycle_length
  v  <- c("Healthy", "CVD", "PostCVD", "Dead")
  e  <- effects(v, params = p)
  expect_equal(e$yld, unname(c(0, p$dw["CVD"] * cl, p$dw["PostCVD"] * cl, 0)))
  expect_equal(e$qaly, unname(c(p$utility["Healthy"] * cl,
                                p$utility["CVD"] * cl,
                                p$utility["PostCVD"] * cl, 0)))
})

source(here::here("R", "transition.R"))
source(here::here("R", "costs.R"))
source(here::here("R", "effects.R"))
source(here::here("R", "microsim.R"))

test_that("samplev returns valid states from a probability matrix", {
  set.seed(1)
  P <- matrix(c(1, 0, 0, 0,
                0, 0, 0, 1), nrow = 2, byrow = TRUE)
  expect_equal(samplev(P, c("Healthy", "CVD", "PostCVD", "Dead")),
               c("Healthy", "Dead"))
})

test_that("MicroSim is reproducible and Dead is absorbing", {
  si <- make_sim_input(n = 50)
  p  <- get_params()
  r1 <- MicroSim(si, p, arm = "Control", probs, costs, effects)
  r2 <- MicroSim(si, p, arm = "Control", probs, costs, effects)
  expect_identical(r1$m_M, r2$m_M)
  # once Dead, always Dead across remaining cycles
  for (i in seq_len(nrow(r1$m_M))) {
    d <- which(r1$m_M[i, ] == "Dead")
    if (length(d)) expect_true(all(r1$m_M[i, min(d):ncol(r1$m_M)] == "Dead"))
  }
})

test_that("MicroSim returns mean discounted cost, DALYs and QALYs", {
  si <- make_sim_input(n = 50)
  r  <- MicroSim(si, get_params(), "Control", probs, costs, effects)
  expect_true(is.finite(r$mean_cost))
  expect_true(is.finite(r$mean_daly))
  expect_true(is.finite(r$mean_qaly))
  expect_true(r$mean_daly >= 0)
})

source(here::here("R", "cea.R"))

test_that("run_cea computes incremental outcomes and the ICER", {
  res <- list(
    Intervention = list(mean_cost = 500, mean_daly = 0.80, mean_qaly = 3.50),
    Control      = list(mean_cost = 200, mean_daly = 1.00, mean_qaly = 3.20)
  )
  cea <- run_cea(res)
  expect_equal(cea$d_cost, 300)
  expect_equal(cea$daly_averted, 0.20)   # control DALYs - intervention DALYs
  expect_equal(cea$qaly_gained, 0.30)
  expect_equal(cea$icer_daly, 300 / 0.20)
  expect_equal(cea$icer_qaly, 300 / 0.30)
})

test_that("run_cea reports a dominant intervention", {
  res <- list(
    Intervention = list(mean_cost = 100, mean_daly = 0.80, mean_qaly = 3.50),
    Control      = list(mean_cost = 200, mean_daly = 1.00, mean_qaly = 3.20)
  )
  cea <- run_cea(res)
  expect_true(cea$dominant)
})

# --- evaluate_model: the single params -> ICER path -------------------------
source(here::here("R", "model_run.R"))

make_eval_cohort <- function(n = 300, n_t = 6) {
  set.seed(7)
  list(
    ids              = as.character(seq_len(n)),
    state_0          = rep("Healthy", n),
    sbp_intervention = matrix(rnorm(n * n_t, 130, 12), n, n_t),
    treated          = matrix(TRUE, n, n_t),
    base             = data.table(
      sex_num  = rep(0:1, length.out = n),
      age      = round(seq(45, 70, length.out = n)),
      smoking  = 0L,
      bmi      = 27,
      diabetes = 0L
    )
  )
}

make_eval_mortality <- function() {
  list(p_background = 0.004, p_cvd_fatal = 0.30, p_postcvd = 0.10)
}

test_that("get_params defaults the risk calibration multiplier to 1", {
  expect_equal(get_params()$risk_calib, 1)
})

test_that("evaluate_model returns the run_cea result shape", {
  out <- evaluate_model(get_params(), make_eval_cohort(), make_eval_mortality())
  expect_true(all(c("d_cost", "daly_averted", "qaly_gained", "icer_daly",
                    "summary") %in% names(out)))
  expect_equal(nrow(out$summary), 2L)
})

test_that("evaluate_model is reproducible for identical inputs", {
  ch <- make_eval_cohort(); mort <- make_eval_mortality()
  expect_equal(evaluate_model(get_params(), ch, mort)$icer_daly,
               evaluate_model(get_params(), ch, mort)$icer_daly)
})

test_that("evaluate_model with no treatment effect leaves the arms identical", {
  # delta_sbp = 0 makes Control SBP equal Intervention SBP, so with common
  # random numbers the two state traces coincide exactly and the only
  # incremental cost is the intervention itself
  out <- evaluate_model(get_params(delta_sbp = 0), make_eval_cohort(),
                        make_eval_mortality())
  expect_equal(out$daly_averted, 0)
  expect_equal(out$qaly_gained, 0)
  expect_gt(out$d_cost, 0)
})

test_that("evaluate_model averts more DALYs as the treatment effect grows", {
  # 2000 individuals, not the default 300. Both arms share their uniform draws
  # (common random numbers), so an individual only changes state when a draw
  # lands inside the narrow probability gap the treatment effect opens. At
  # -20 mmHg that gap is ~0.0015 per person-cycle, so 300 x 6 = 1800
  # person-cycles expect fewer than three flips and routinely observe zero --
  # the estimator is quantised, not absent. Enough person-time and the
  # ordering is stable.
  ch <- make_eval_cohort(n = 2000); mort <- make_eval_mortality()
  small <- evaluate_model(get_params(delta_sbp = -2), ch, mort)
  large <- evaluate_model(get_params(delta_sbp = -20), ch, mort)
  expect_gt(large$daly_averted, small$daly_averted)
})

test_that("evaluate_model rebuilds the Control SBP from delta_sbp", {
  # a large delta pushes more Control person-time over the 140 HTA threshold,
  # which must show up as higher Control cost -- proving the htn flag is
  # recomputed per call and not carried over from build_cohort
  ch <- make_eval_cohort(); mort <- make_eval_mortality()
  small <- evaluate_model(get_params(delta_sbp = -1), ch, mort)
  large <- evaluate_model(get_params(delta_sbp = -30), ch, mort)
  expect_gt(large$summary[arm == "Control", cost],
            small$summary[arm == "Control", cost])
})

test_that("evaluate_model raises CVD burden when risk_calib exceeds 1", {
  ch <- make_eval_cohort(); mort <- make_eval_mortality()
  p_lo <- get_params(); p_lo$risk_calib <- 1
  p_hi <- get_params(); p_hi$risk_calib <- 2
  expect_gt(evaluate_model(p_hi, ch, mort)$summary[arm == "Control", daly],
            evaluate_model(p_lo, ch, mort)$summary[arm == "Control", daly])
})

test_that("evaluate_model honours sampled mortality probabilities", {
  ch <- make_eval_cohort()
  lo <- evaluate_model(get_params(), ch,
                       list(p_background = 0.004, p_cvd_fatal = 0.10,
                            p_postcvd = 0.10))
  hi <- evaluate_model(get_params(), ch,
                       list(p_background = 0.004, p_cvd_fatal = 0.90,
                            p_postcvd = 0.10))
  expect_gt(hi$summary[arm == "Control", daly],
            lo$summary[arm == "Control", daly])
})

# --- arm_risk: the time axis and the vectorised rebuild ---------------------
# These guard the two properties the lifetime horizon depends on. The age one
# is a behaviour fix: age must advance with calendar time, not with the cycle
# index, or a 197-cycle horizon ages the cohort 197 years in 82. The
# equivalence one pins the vectorised implementation to the per-cycle loop it
# replaced, so a performance rewrite can never quietly change a number.

test_that("arm_risk advances age by cycle_length, not by one year per cycle", {
  p <- get_params()
  ch <- make_eval_cohort(n = 20, n_t = 6)
  # hold SBP flat so the only thing varying across columns is age
  flat <- matrix(140, 20, 6)
  risk <- arm_risk(flat, ch$base, p)

  # cycle t scores the cohort at age_0 + (t - 1) * cycle_length, so a matrix
  # built from ages shifted by exactly one cycle must reproduce column t + 1
  shifted <- copy(ch$base)
  shifted[, age := age + p$cycle_length]
  risk_shifted <- arm_risk(flat, shifted, p)
  expect_equal(risk[, 2], risk_shifted[, 1])

  # and the first column must be the untouched baseline age
  base_only <- cvd_risk(data.table(
    sex_num = ch$base$sex_num, age = ch$base$age, sbp = flat[, 1],
    smoking = ch$base$smoking, bmi = ch$base$bmi, diabetes = ch$base$diabetes
  ), p)
  expect_equal(risk[, 1], base_only)
})

test_that("arm_risk matches a per-cycle reference loop exactly", {
  # reference implementation: the pre-vectorisation loop, with the corrected
  # age axis. Any divergence means the rewrite changed the model, not just
  # its speed.
  arm_risk_loop <- function(sbp_matrix, base, params) {
    n_i <- nrow(sbp_matrix); n_t <- ncol(sbp_matrix)
    out <- matrix(0, n_i, n_t)
    for (t in seq_len(n_t)) {
      out[, t] <- cvd_risk(data.table(
        sex_num  = base$sex_num,
        age      = base$age + (t - 1) * params$cycle_length,
        sbp      = sbp_matrix[, t],
        smoking  = base$smoking,
        bmi      = base$bmi,
        diabetes = base$diabetes
      ), params)
    }
    out
  }
  p  <- get_params()
  ch <- make_eval_cohort(n = 50, n_t = 6)
  expect_identical(arm_risk(ch$sbp_intervention, ch$base, p),
                   arm_risk_loop(ch$sbp_intervention, ch$base, p))
})

test_that("samplev matches an apply-based reference for the same seed", {
  # samplev is rewritten for speed (four column additions instead of a
  # row-wise apply). It runs 2 x n_t times per draw, so at 197 cycles the
  # apply overhead is no longer negligible -- but the sampled states must be
  # identical, draw for draw.
  samplev_ref <- function(P, states) {
    u   <- runif(nrow(P))
    cp  <- t(apply(P, 1L, cumsum))
    idx <- rowSums(u > cp) + 1L
    states[idx]
  }
  set.seed(99)
  P <- matrix(runif(400 * 4), 400, 4)
  P <- P / rowSums(P)
  v_n <- c("Healthy", "CVD", "PostCVD", "Dead")

  set.seed(11); a <- samplev(P, v_n)
  set.seed(11); b <- samplev_ref(P, v_n)
  expect_identical(a, b)

  # degenerate rows (a certain transition) must still land on the right state
  P0 <- matrix(0, 3, 4); P0[1, 1] <- 1; P0[2, 4] <- 1; P0[3, 3] <- 1
  set.seed(5); expect_identical(samplev(P0, v_n), c("Healthy", "Dead", "PostCVD"))
})

# --- post-trial exposure scenarios -----------------------------------------
# One `treated` matrix drives both the effect and the cost, so these tests
# check that a single switch moves both and that neither can drift.

make_lifetime_cohort <- function(n = 800, n_trial = 6L, n_t = 40L) {
  set.seed(3)
  sbp <- matrix(rnorm(n * n_trial, 150, 12), n, n_trial)
  list(
    ids              = as.character(seq_len(n)),
    state_0          = rep("Healthy", n),
    sbp_intervention = carry_forward(sbp, n_t),
    treated          = matrix(TRUE, n, n_trial),
    base             = data.table(
      sex_num  = rep(0:1, length.out = n),
      age      = round(seq(45, 70, length.out = n)),
      smoking  = 0L, bmi = 27, diabetes = 0L
    )
  )
}

lifetime_params <- function(post_trial = "continue", delta_sbp = -20) {
  p <- get_params(delta_sbp = delta_sbp, post_trial = post_trial)
  p$n_t <- 40L
  p
}

test_that("post_trial_treated extends exposure only under 'continue'", {
  tr <- matrix(TRUE, 5, 6)
  cont <- post_trial_treated(tr, 10L, "continue")
  stop <- post_trial_treated(tr, 10L, "stop")

  expect_equal(dim(cont), c(5L, 10L))
  expect_equal(dim(stop), c(5L, 10L))
  # the observed trial columns are identical under both scenarios: events
  # already averted during the trial stay averted
  expect_equal(cont[, 1:6], tr)
  expect_equal(stop[, 1:6], tr)
  expect_true(all(cont[, 7:10]))
  expect_false(any(stop[, 7:10]))
  # already at or beyond the horizon -> returned untouched
  expect_equal(post_trial_treated(tr, 6L, "stop"), tr)
})

test_that("stopping exposure closes the SBP gap at the first projected cycle", {
  ch <- make_lifetime_cohort()
  gap <- function(pt) {
    p <- lifetime_params(pt)
    tr <- post_trial_treated(ch$treated, p$n_t, p$post_trial)
    ctl <- ch$sbp_intervention + tr * abs(p$delta_sbp)
    colMeans(ctl - ch$sbp_intervention)
  }
  g_cont <- gap("continue")
  g_stop <- gap("stop")
  expect_equal(g_cont[1:6], g_stop[1:6])            # trial phase unchanged
  expect_true(all(g_cont[7:40] == 20))              # gap persists
  expect_true(all(g_stop[7:40] == 0))               # gap closes
})

test_that("continuing exposure charges the intervention for the whole horizon", {
  # delta_sbp = 0 isolates the cost switch: with no SBP gap the two arms have
  # identical hypertension flags and identical state traces, so the ONLY
  # incremental cost is the salt substitute itself. Under "continue" it is
  # charged for 40 cycles, under "stop" for 6.
  ch <- make_lifetime_cohort(); mort <- make_eval_mortality()
  cont <- evaluate_model(lifetime_params("continue", delta_sbp = 0), ch, mort)
  stop <- evaluate_model(lifetime_params("stop",     delta_sbp = 0), ch, mort)

  expect_gt(cont$d_cost, stop$d_cost)
  expect_gt(stop$d_cost, 0)
  expect_equal(cont$daly_averted, 0)
  expect_equal(stop$daly_averted, 0)
})

test_that("stopping exposure averts fewer DALYs", {
  # the effect switch, tested on its own. Note that d_cost is NOT asserted
  # here: at a large delta_sbp the intervention can be cost-SAVING, because
  # avoided hypertension management (params$costs$hta, charged per cycle) is
  # two orders of magnitude larger than the salt substitute. Continuing
  # exposure then lowers incremental cost rather than raising it, which is a
  # property of the cost inputs, not of the scenario switch.
  ch <- make_lifetime_cohort(); mort <- make_eval_mortality()
  cont <- evaluate_model(lifetime_params("continue"), ch, mort)
  stop <- evaluate_model(lifetime_params("stop"), ch, mort)

  expect_gt(cont$daly_averted, stop$daly_averted)
  # the trial-phase benefit is not thrown away by stopping afterwards
  expect_gte(stop$daly_averted, 0)
})

test_that("the exposure scenario reaches the model through params alone", {
  # the same cohort object must produce different answers under the two
  # scenarios -- this is what lets OWSA and PSA see the switch
  ch <- make_lifetime_cohort(); mort <- make_eval_mortality()
  expect_false(isTRUE(all.equal(
    evaluate_model(lifetime_params("continue"), ch, mort)$d_cost,
    evaluate_model(lifetime_params("stop"),     ch, mort)$d_cost)))
})

# --- GBD-derived lifetime mortality ----------------------------------------
# Over 197 cycles a single pooled trial death rate would leave most of the
# cohort alive at 100. These tests pin the age-varying replacement.

test_that("load_gbd_mortality derives non-CVD background from all-cause", {
  g <- load_gbd_mortality()
  expect_true(all(c("age_lower", "sex", "noncvd_rate", "acute_cf") %in% names(g)))
  # background must be all-cause NET of IHD+stroke: the Healthy -> Dead arm
  # carries non-CVD death only, since CVD deaths route through the CVD state
  expect_equal(g$noncvd_rate,
               (g$allcause_death_rate - g$ihd_stroke_death_rate) / 1e5)
  expect_true(all(g$noncvd_rate > 0))
  expect_true(all(g$acute_cf > 0 & g$acute_cf < 1))
})

test_that("mortality_matrices returns per-cycle probabilities that rise with age", {
  p <- get_params(); p$n_t <- 60L
  base <- data.table(sex_num = c(0L, 1L), age = c(40, 40))
  m <- mortality_matrices(base, p)

  expect_equal(dim(m$p_background), c(2L, 60L))
  expect_equal(dim(m$p_cvd_fatal), c(2L, 60L))
  expect_true(all(m$p_background > 0 & m$p_background < 1))
  # a 40-year-old is at much higher risk 25 years later
  expect_gt(m$p_background[1, 60], m$p_background[1, 1])
  # men die faster than women at the same age in the Peru life table
  expect_gt(m$p_background[2, 1], m$p_background[1, 1])
})

test_that("mortality_matrices tracks each person's own age, not the cohort's", {
  p <- get_params(); p$n_t <- 12L
  base <- data.table(sex_num = c(1L, 1L), age = c(40, 80))
  m <- mortality_matrices(base, p)
  expect_gt(m$p_background[2, 1], m$p_background[1, 1] * 10)
})

test_that("lifetime_mortality derives post-CVD mortality from the background", {
  p <- get_params()
  raw <- list(p_background = matrix(0.01, 2, 3),
              p_cvd_fatal  = matrix(0.20, 2, 3))
  out <- lifetime_mortality(raw, p)

  expect_equal(out$p_background, raw$p_background)       # calib 1 = identity
  expect_equal(out$p_cvd_fatal, raw$p_cvd_fatal)
  # post-CVD is the background hazard scaled by the excess hazard ratio,
  # applied on the cumulative-hazard scale like every other multiplier here
  expect_equal(out$p_postcvd, 1 - (1 - raw$p_background)^p$hr_postcvd)
  expect_true(all(out$p_postcvd > out$p_background))
})

test_that("lifetime_mortality applies the PSA level multipliers", {
  raw <- list(p_background = matrix(0.01, 2, 3),
              p_cvd_fatal  = matrix(0.20, 2, 3))
  p <- get_params(); p$mort_calib <- 1.5; p$cf_calib <- 0.5
  out <- lifetime_mortality(raw, p)
  expect_equal(out$p_background, matrix(1 - (1 - 0.01)^1.5, 2, 3))
  expect_equal(out$p_cvd_fatal, matrix(1 - (1 - 0.20)^0.5, 2, 3))
  # the background multiplier composes with the excess hazard, so a sampled
  # mortality level moves post-CVD mortality too
  expect_equal(out$p_postcvd,
               matrix(1 - (1 - 0.01)^(1.5 * p$hr_postcvd), 2, 3))
})

test_that("reference_sle returns declining life expectancy and never negative", {
  expect_gt(reference_sle(40), reference_sle(70))
  expect_gt(reference_sle(0), 80)
  expect_gte(reference_sle(100), 0)
  expect_gte(reference_sle(130), 0)
  # interpolates between the tabulated ages rather than stepping
  expect_true(reference_sle(42) < reference_sle(40) &&
              reference_sle(42) > reference_sle(45))
})

# --- YLL: reference life expectancy and discounting from the death cycle ----

test_that("MicroSim charges YLL from the reference life table when age is known", {
  si <- make_sim_input(n = 3)
  p  <- get_params()
  # everyone dies entering cycle 1
  si$cvd_risk$Intervention[] <- 0
  si$mortality <- list(p_background = 1, p_cvd_fatal = 1, p_postcvd = 1)
  si$age_0 <- c(40, 60, 80)

  out <- MicroSim(si, p, "Intervention", probs, costs, effects)
  expect_true(all(out$m_M[, 2L] == "Dead"))
  # death at cycle 1 -> age_0 + 1 * cycle_length; older deaths lose fewer years
  expect_true(out$daly[1] > out$daly[2] && out$daly[2] > out$daly[3])
  # and the magnitude is the reference life expectancy, discounted, not the
  # remaining horizon (which would be the same 2.08 yr for all three)
  expect_gt(out$daly[1], 20)
})

test_that("MicroSim falls back to horizon-based YLL without a baseline age", {
  # older callers that build sim_input by hand keep the previous definition
  si <- make_sim_input(n = 2)
  p  <- get_params()
  si$mortality <- list(p_background = 1, p_cvd_fatal = 1, p_postcvd = 1)
  expect_null(si$age_0)
  out <- MicroSim(si, p, "Intervention", probs, costs, effects)
  # all deaths at cycle 1, so YLL is (n_t - 1) * cl for everyone
  expect_equal(unname(out$daly[1]), unname(out$daly[2]))
  expect_lt(out$daly[1], (p$n_t - 1) * p$cycle_length + 1)
})

test_that("YLL is discounted from the cycle of death, not the end of horizon", {
  # two identical individuals, one dying early and one late. Under the old
  # horizon-end weighting both YLL streams were discounted by the same factor;
  # the early death must now carry the heavier (less discounted) YLL.
  p <- get_params(); p$n_t <- 24L
  mk <- function(death_t) {
    si <- make_sim_input(n = 1, n_t = 24)
    si$age_0 <- 60
    si$cvd_risk$Intervention[] <- 0
    si$mortality <- list(p_background = 0, p_cvd_fatal = 0, p_postcvd = 0)
    # force death exactly at death_t by making background certain there
    bg <- matrix(0, 1, 24); bg[1, death_t] <- 1
    si$mortality$p_background <- bg
    MicroSim(si, p, "Intervention", probs, costs, effects)
  }
  early <- mk(2); late <- mk(20)
  expect_gt(early$daly, late$daly)
})

test_that("evaluate_model rejects a cohort built for a different horizon", {
  ch <- make_lifetime_cohort()          # matrices are 40 cycles wide
  p  <- get_params()                    # but n_t defaults to 6
  expect_error(evaluate_model(p, ch, make_eval_mortality()),
               "set_horizon")
})
