#------------------
# Sensitivity analysis module tests (OWSA + PSA)
#------------------
pacman::p_load(testthat, data.table)

source(here::here("R", "parameters.R"))
source(here::here("R", "globorisk_risk.R"))
source(here::here("R", "mortality.R"))
source(here::here("R", "cohort.R"))
source(here::here("R", "transition.R"))
source(here::here("R", "costs.R"))
source(here::here("R", "effects.R"))
source(here::here("R", "microsim.R"))
source(here::here("R", "cea.R"))
source(here::here("R", "model_run.R"))
source(here::here("R", "psa.R"))

# --- assign_path: writing a sampled value back into a nested structure ------

test_that("assign_path writes into a nested list", {
  p <- list(costs = list(hta = 120, cvd_event = 3000))
  expect_equal(assign_path(p, c("costs", "hta"), 200)$costs$hta, 200)
  expect_equal(assign_path(p, c("costs", "hta"), 200)$costs$cvd_event, 3000)
})

test_that("assign_path writes into a named atomic vector", {
  # dw and utility are named numeric vectors, not lists, so the same helper
  # has to work for both shapes
  p <- list(dw = c(CVD = 0.43, PostCVD = 0.07))
  out <- assign_path(p, c("dw", "CVD"), 0.50)
  expect_equal(out$dw[["CVD"]], 0.50)
  expect_equal(out$dw[["PostCVD"]], 0.07)
})

test_that("assign_path writes a top-level scalar", {
  expect_equal(assign_path(list(delta_sbp = -5), "delta_sbp", -8)$delta_sbp, -8)
})

# --- draw_dist: one sampler per ISPOR-recommended distribution -------------

test_that("draw_dist gamma draws are positive and recover mean and sd", {
  set.seed(42)
  x <- draw_dist(list(dist = "gamma", mean = 3000, se = 600), 20000)
  expect_true(all(x > 0))
  expect_equal(mean(x), 3000, tolerance = 0.02)
  expect_equal(sd(x), 600, tolerance = 0.05)
})

test_that("draw_dist beta draws stay in the unit interval and recover mean", {
  set.seed(42)
  x <- draw_dist(list(dist = "beta", mean = 0.43, se = 0.05), 20000)
  expect_true(all(x > 0 & x < 1))
  expect_equal(mean(x), 0.43, tolerance = 0.02)
  expect_equal(sd(x), 0.05, tolerance = 0.05)
})

test_that("draw_dist beta_counts recovers the observed event fraction", {
  set.seed(42)
  x <- draw_dist(list(dist = "beta_counts", events = 30, n = 100), 20000)
  expect_true(all(x > 0 & x < 1))
  expect_equal(mean(x), 0.30, tolerance = 0.03)
})

test_that("draw_dist beta_counts stays valid when no events were observed", {
  # p_cvd_fatal can legitimately be 0/n in a small trial; Beta(0, n) is not a
  # distribution, so the sampler must not blow up or return all zeros
  set.seed(42)
  x <- draw_dist(list(dist = "beta_counts", events = 0, n = 50), 5000)
  expect_true(all(x >= 0 & x < 1))
  expect_true(mean(x) > 0)
  expect_true(mean(x) < 0.05)
})

test_that("draw_dist beta_counts returns the point value with no person-time", {
  x <- draw_dist(list(dist = "beta_counts", events = 0, n = 0), 100)
  expect_equal(x, rep(0, 100))
})

test_that("draw_dist normal recovers mean and sd", {
  set.seed(42)
  x <- draw_dist(list(dist = "normal", mean = -5, se = 1.5), 20000)
  expect_equal(mean(x), -5, tolerance = 0.02)
  expect_equal(sd(x), 1.5, tolerance = 0.05)
})

test_that("draw_dist lognormal is positive with median 1", {
  set.seed(42)
  x <- draw_dist(list(dist = "lognormal", sdlog = 0.2), 20000)
  expect_true(all(x > 0))
  expect_equal(median(x), 1, tolerance = 0.02)
})

test_that("draw_dist rejects an unknown distribution", {
  expect_error(draw_dist(list(dist = "weibull", mean = 1, se = 1), 10),
               "unknown distribution")
})

# --- psa_specs and sample_params -------------------------------------------

test_that("psa_specs entries all carry the fields sample_params needs", {
  specs <- psa_specs()
  expect_gt(length(specs), 0L)
  for (s in specs) {
    expect_true(all(c("name", "target", "path", "dist", "source") %in% names(s)))
    expect_true(s$target %in% c("params", "mortality"))
  }
})

test_that("psa_specs names are unique", {
  nm <- vapply(psa_specs(), `[[`, character(1), "name")
  expect_equal(anyDuplicated(nm), 0L)
})

test_that("psa_specs excludes the discount rates", {
  # ISPOR treats discount rates as policy choices for scenario/one-way
  # analysis, not as uncertain parameters to sample
  paths <- vapply(psa_specs(), function(s) paste(s$path, collapse = "$"),
                  character(1))
  expect_false(any(c("d_c", "d_e") %in% paths))
})

test_that("psa_specs draws mortality from trial counts when supplied", {
  mort <- mortality_rates(data.table(
    state     = c(rep("Healthy", 100), rep("CVD", 10), rep("PostCVD", 20)),
    dead      = c(rep(0, 96), rep(1, 4), rep(c(1, 0), c(3, 7)), rep(0, 18), 1, 1),
    cvd_death = c(rep(0, 96), 1, 1, 0, 0, rep(c(1, 0), c(3, 7)), rep(0, 20))
  ))
  specs <- psa_specs(mortality = mort)
  fatal <- Filter(function(s) s$name == "p_cvd_fatal", specs)[[1]]
  expect_equal(fatal$dist, "beta_counts")
  expect_equal(fatal$events, 3L)
  expect_equal(fatal$n, 10L)
  expect_false(fatal$source == "PLACEHOLDER")   # counts are real trial data
})

test_that("sample_params returns one row per draw and one column per spec", {
  specs <- psa_specs()
  d <- sample_params(specs, n_sim = 50)
  expect_s3_class(d, "data.table")
  expect_equal(nrow(d), 50L)
  expect_equal(names(d), unname(vapply(specs, `[[`, character(1), "name")))
})

test_that("sample_params is reproducible for a fixed seed", {
  specs <- psa_specs()
  expect_equal(sample_params(specs, 20, seed = 42),
               sample_params(specs, 20, seed = 42))
})

test_that("sample_params gives different draws for different seeds", {
  specs <- psa_specs()
  expect_false(isTRUE(all.equal(sample_params(specs, 20, seed = 42),
                                sample_params(specs, 20, seed = 43))))
})

test_that("sample_params draws are jointly independent across parameters", {
  # the specs declare no correlation structure, so a strong pairwise
  # correlation would mean the sampler is reusing a stream
  d <- sample_params(psa_specs(), n_sim = 4000, seed = 42)
  cm <- abs(cor(as.matrix(d)))
  diag(cm) <- 0
  expect_lt(max(cm), 0.10)
})

# --- apply_draw -------------------------------------------------------------

test_that("apply_draw routes a draw into params and mortality separately", {
  specs <- psa_specs()
  draw  <- sample_params(specs, 1, seed = 42)
  out   <- apply_draw(get_params(), list(p_background = 0.004,
                                         p_cvd_fatal = 0.3,
                                         p_postcvd = 0.1),
                      specs, draw[1])
  expect_true(all(c("params", "mortality") %in% names(out)))
  expect_equal(out$params$delta_sbp, draw$delta_sbp[1])
  expect_equal(out$params$costs$cvd_event, draw$cvd_event[1])
  expect_equal(out$params$dw[["CVD"]], draw$dw_cvd[1])
})

test_that("apply_draw leaves untouched parameters at their base value", {
  specs <- psa_specs()
  draw  <- sample_params(specs, 1, seed = 42)
  base  <- get_params()
  out   <- apply_draw(base, list(p_background = 0.004, p_cvd_fatal = 0.3,
                                 p_postcvd = 0.1), specs, draw[1])
  expect_equal(out$params$d_c, base$d_c)
  expect_equal(out$params$n_t, base$n_t)
  expect_equal(out$params$cycle_length, base$cycle_length)
})

# --- run_psa and its outputs -----------------------------------------------

# A deliberately high-risk fixture: older and hypertensive enough that the two
# arms actually diverge over six cycles. At SBP 132 / ages 45-72 no transition
# differs between arms, daly_averted is exactly 0 and every ICER is Inf, which
# makes the sensitivity outputs untestable rather than merely small.
make_psa_cohort <- function(n = 400, n_t = 6) {
  set.seed(11)
  list(
    ids              = as.character(seq_len(n)),
    state_0          = rep("Healthy", n),
    sbp_intervention = matrix(rnorm(n * n_t, 160, 12), n, n_t),
    treated          = matrix(TRUE, n, n_t),
    base             = data.table(
      sex_num  = rep(0:1, length.out = n),
      age      = round(seq(60, 85, length.out = n)),
      smoking  = 0L,
      bmi      = 27,
      diabetes = 0L
    )
  )
}

make_psa_mortality <- function() {
  list(p_background = 0.004, p_cvd_fatal = 0.30, p_postcvd = 0.10)
}

test_that("evaluate_model gives the same answer with a cached Intervention risk", {
  # run_psa reuses the Intervention risk matrix across draws because its SBP
  # never depends on delta_sbp; the cached path must be exactly equivalent
  ch <- make_psa_cohort(); mort <- make_psa_mortality(); p <- get_params()
  cached <- arm_risk(ch$sbp_intervention, ch$base, p)
  expect_equal(evaluate_model(p, ch, mort, risk_intervention = cached)$icer_daly,
               evaluate_model(p, ch, mort)$icer_daly)
})

test_that("run_psa returns one row per draw with the CEA outputs", {
  draws <- run_psa(psa_specs(), make_psa_cohort(), make_psa_mortality(),
                   n_sim = 6, seed = 42)
  expect_s3_class(draws, "data.table")
  expect_equal(nrow(draws), 6L)
  expect_true(all(c("sim", "d_cost", "daly_averted", "qaly_gained",
                    "icer_daly", "icer_qaly") %in% names(draws)))
})

test_that("run_psa keeps the sampled parameter values alongside the results", {
  # needed for EVPPI / metamodelling later, and to debug implausible draws
  draws <- run_psa(psa_specs(), make_psa_cohort(), make_psa_mortality(),
                   n_sim = 6, seed = 42)
  expect_true(all(c("delta_sbp", "risk_calib", "cvd_event") %in% names(draws)))
  expect_equal(draws$delta_sbp, sample_params(psa_specs(), 6, seed = 42)$delta_sbp)
})

test_that("run_psa is reproducible for a fixed seed", {
  a <- run_psa(psa_specs(), make_psa_cohort(), make_psa_mortality(),
               n_sim = 5, seed = 42)
  b <- run_psa(psa_specs(), make_psa_cohort(), make_psa_mortality(),
               n_sim = 5, seed = 42)
  expect_equal(a, b)
})

test_that("run_psa propagates parameter uncertainty into the results", {
  draws <- run_psa(psa_specs(), make_psa_cohort(), make_psa_mortality(),
                   n_sim = 8, seed = 42)
  expect_gt(sd(draws$d_cost), 0)
  expect_gt(sd(draws$daly_averted), 0)
})

test_that("ceac reports the share of draws that are cost-effective", {
  draws <- data.table(d_cost = c(10, 20, 30), daly_averted = c(1, 1, 1))
  out <- ceac(draws, wtp = c(0, 15, 25, 35))
  expect_equal(out$wtp, c(0, 15, 25, 35))
  expect_equal(out$p_ce, c(0, 1 / 3, 2 / 3, 1))
})

test_that("ceac at zero willingness to pay is the probability of cost saving", {
  draws <- data.table(d_cost = c(-5, 10, -2, 40), daly_averted = c(1, 1, 1, 1))
  expect_equal(ceac(draws, wtp = 0)$p_ce, 0.5)
})

test_that("ceac stays within 0 and 1", {
  draws <- data.table(d_cost = rnorm(200, 100, 50),
                      daly_averted = rnorm(200, 0.01, 0.005))
  out <- ceac(draws, wtp = seq(0, 30000, 5000))
  expect_true(all(out$p_ce >= 0 & out$p_ce <= 1))
})

test_that("ceac can be computed on QALYs instead of DALYs", {
  draws <- data.table(d_cost = c(10, 30), daly_averted = c(0, 0),
                      qaly_gained = c(1, 1))
  expect_equal(ceac(draws, wtp = 20, outcome = "qaly")$p_ce, 0.5)
})

test_that("psa_summary reports a median and 95% credible interval", {
  draws <- data.table(sim = 1:1000, d_cost = rnorm(1000, 100, 10),
                      daly_averted = rnorm(1000, 0.02, 0.002),
                      qaly_gained = rnorm(1000, 0.03, 0.003),
                      icer_daly = rnorm(1000, 5000, 500),
                      icer_qaly = rnorm(1000, 3000, 300))
  s <- psa_summary(draws)
  expect_s3_class(s, "data.table")
  expect_true(all(c("outcome", "mean", "median", "lower", "upper") %in% names(s)))
  expect_true(all(s$lower <= s$median & s$median <= s$upper))
})

test_that("psa_convergence traces the running mean over draws", {
  draws <- data.table(sim = 1:50, icer_daly = rnorm(50, 5000, 500))
  tr <- psa_convergence(draws, outcome = "icer_daly", plot = FALSE)
  expect_equal(nrow(tr), 50L)
  expect_equal(tr$running_mean[50], mean(draws$icer_daly))
  expect_equal(tr$running_mean[1], draws$icer_daly[1])
})

test_that("psa_convergence and ce_plane_psa return ggplot objects", {
  draws <- data.table(sim = 1:40, d_cost = rnorm(40, 100, 10),
                      daly_averted = rnorm(40, 0.02, 0.002),
                      icer_daly = rnorm(40, 5000, 500))
  expect_s3_class(psa_convergence(draws, outcome = "icer_daly"), "ggplot")
  expect_s3_class(ce_plane_psa(draws), "ggplot")
  expect_s3_class(ceac_plot(ceac(draws, wtp = seq(0, 20000, 2000))), "ggplot")
})

# --- one-way deterministic sensitivity analysis ----------------------------
source(here::here("R", "owsa.R"))

test_that("owsa_ranges entries carry the fields run_owsa needs", {
  for (r in owsa_ranges()) {
    expect_true(all(c("name", "label", "target", "path", "low", "high",
                      "source") %in% names(r)))
    expect_true(r$target %in% c("params", "mortality"))
    expect_lt(r$low, r$high)
  }
})

test_that("owsa_ranges includes the discount rates", {
  # the mirror of the PSA test: discount rates belong in one-way analysis,
  # which is where reviewers expect to see them varied
  nm <- vapply(owsa_ranges(), `[[`, character(1), "name")
  expect_true(all(c("d_c", "d_e") %in% nm))
})

test_that("owsa_ranges brackets the base-case value", {
  base <- get_params()
  for (r in owsa_ranges(base = base)) {
    if (r$target != "params") next
    v <- Reduce(function(x, k) x[[k]], r$path, base)
    expect_gte(v, r$low)
    expect_lte(v, r$high)
  }
})

test_that("apply_value substitutes into params or mortality by target", {
  r_p <- list(target = "params", path = "delta_sbp")
  r_m <- list(target = "mortality", path = "p_postcvd")
  out_p <- apply_value(get_params(), list(p_postcvd = 0.1), r_p, -9)
  out_m <- apply_value(get_params(), list(p_postcvd = 0.1), r_m, 0.4)
  expect_equal(out_p$params$delta_sbp, -9)
  expect_equal(out_p$mortality$p_postcvd, 0.1)
  expect_equal(out_m$mortality$p_postcvd, 0.4)
  expect_equal(out_m$params$delta_sbp, get_params()$delta_sbp)
})

test_that("run_owsa returns one row per varied parameter", {
  rg <- owsa_ranges()[c("delta_sbp", "cvd_event")]
  out <- run_owsa(rg, make_psa_cohort(), make_psa_mortality())
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 2L)
  expect_true(all(c("param", "label", "low", "high", "out_low", "out_high",
                    "base", "spread") %in% names(out)))
})

test_that("run_owsa orders rows by descending spread", {
  out <- run_owsa(owsa_ranges()[c("delta_sbp", "cvd_event", "d_c")],
                  make_psa_cohort(), make_psa_mortality())
  expect_equal(out$spread, sort(out$spread, decreasing = TRUE))
})

test_that("run_owsa reports zero spread for a degenerate range", {
  rg <- list(cvd_event = list(name = "cvd_event", label = "CVD event cost",
                              target = "params", path = c("costs", "cvd_event"),
                              low = 3000, high = 3000, source = "test"))
  out <- run_owsa(rg, make_psa_cohort(), make_psa_mortality())
  expect_equal(out$spread, 0)
  expect_equal(out$out_low, out$out_high)
})

test_that("run_owsa averts more DALYs when the treatment effect is larger", {
  # delta_sbp low = -8 mmHg is a bigger effect than high = -2, so more DALYs
  # are averted. Asserted on daly_averted rather than the ICER on purpose: with
  # the 2024 cost inputs the intervention is cost-saving, the ICER is negative,
  # and a bigger effect makes it LESS negative -- ICER ordering only tracks
  # effectiveness in the more-costly/more-effective quadrant.
  rg <- owsa_ranges()["delta_sbp"]
  out <- run_owsa(rg, make_psa_cohort(), make_psa_mortality(),
                  outcome = "daly_averted")
  expect_gt(out$out_low, out$out_high)
})

test_that("run_owsa base column matches an unperturbed evaluation", {
  ch <- make_psa_cohort(); mort <- make_psa_mortality()
  out <- run_owsa(owsa_ranges()["cvd_event"], ch, mort)
  expect_equal(out$base[1], evaluate_model(get_params(), ch, mort)$icer_daly)
})

test_that("tornado_plot returns a ggplot", {
  out <- run_owsa(owsa_ranges()[c("delta_sbp", "cvd_event")],
                  make_psa_cohort(), make_psa_mortality())
  expect_s3_class(tornado_plot(out), "ggplot")
})
