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
  ch <- make_eval_cohort(); mort <- make_eval_mortality()
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
