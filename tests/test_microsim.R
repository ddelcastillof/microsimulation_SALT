#------------------
# Microsimulation module tests
#------------------
pacman::p_load(testthat, data.table)

source(here::here("functs", "parameters.R"))

test_that("get_params returns a complete config list", {
  p <- get_params()
  expect_type(p, "list")
  expect_identical(p$n_t, 6L)
  expect_identical(p$state_names, c("Healthy", "CVD", "PostCVD", "Dead"))
  expect_equal(p$cycle_length, 5 / 6)
  expect_true(all(c("d_c", "d_e", "arms", "delta_sbp", "dw",
                    "utility", "costs", "globorisk", "seed") %in% names(p)))
  expect_equal(p$globorisk$version, "office")
})

test_that("get_params delta_sbp is configurable", {
  expect_equal(get_params(delta_sbp = -8)$delta_sbp, -8)
})

source(here::here("functs", "globorisk_risk.R"))

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

source(here::here("functs", "mortality.R"))

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

source(here::here("functs", "cohort.R"))

make_long_fixture <- function() {
  # 2 people x 7 waves. Person A: CVD-free. Person B: prior infarto at wave 0.
  CJ_ids <- CJ(codigo = c("A", "B"), wave = 0:6)
  dt <- as.data.table(CJ_ids)
  dt[, codigovilla := "020"]
  dt[, sexo := ifelse(codigo == "A", "Femenino", "Masculino")]
  dt[, age := 55L + wave]
  dt[, sbp := ifelse(codigo == "A", 150, 135)]
  dt[, bmi := 27]
  dt[, smoking1 := "No"]
  dt[, db2 := "No"]
  dt[, c("infarto", "derrame", "insuficiencia", "otracor") := "No"]
  dt[codigo == "B" & wave == 0, infarto := "Si"]
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

source(here::here("functs", "transition.R"))

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

source(here::here("functs", "costs.R"))

test_that("costs assigns state costs and the intervention cost", {
  si <- make_sim_input(n = 4)
  p  <- get_params()
  p$costs$healthy <- 50   # non-zero so the Healthy-state assignment is genuinely tested
  v  <- c("Healthy", "CVD", "PostCVD", "Dead")
  cst <- costs(v, si, t = 1, arm = "Intervention", params = p)
  cl <- p$cycle_length
  expect_equal(cst[1], p$costs$healthy + p$costs$intervention)        # treated Healthy
  expect_equal(cst[2], p$costs$cvd_event + p$costs$intervention)
  expect_equal(cst[3], p$costs$postcvd * cl + p$costs$intervention)
  expect_equal(cst[4], 0)                                            # Dead, no intervention
})

test_that("costs omits the intervention add-on in the Control arm", {
  si <- make_sim_input(n = 4)
  p  <- get_params()
  p$costs$healthy <- 50
  v  <- c("Healthy", "CVD", "PostCVD", "Dead")
  cst <- costs(v, si, t = 1, arm = "Control", params = p)
  cl <- p$cycle_length
  expect_equal(cst[1], p$costs$healthy)             # no intervention cost
  expect_equal(cst[2], p$costs$cvd_event)
  expect_equal(cst[3], p$costs$postcvd * cl)
  expect_equal(cst[4], 0)
})

test_that("costs adds the HTA management cost when hypertensive", {
  si <- make_sim_input(n = 1)
  si$htn$Control[1, 1] <- TRUE
  p  <- get_params()
  p$costs$healthy <- 50   # non-zero so the base cost is distinguishable from the HTA add-on
  cst <- costs("Healthy", si, t = 1, arm = "Control", params = p)
  expect_equal(cst, p$costs$healthy + p$costs$hta * p$cycle_length)
})

source(here::here("functs", "effects.R"))

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

source(here::here("functs", "transition.R"))
source(here::here("functs", "costs.R"))
source(here::here("functs", "effects.R"))
source(here::here("functs", "microsim.R"))

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

source(here::here("functs", "cea.R"))

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
