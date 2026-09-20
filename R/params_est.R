#--------------------------------
# Trial-based parameter estimation
#--------------------------------
#
# Estimates the intervention effect on blood pressure, hypertension incidence,
# mortality and CVD directly from the SALT trial, using the design published in
# Bernabe-Ortiz et al., Nature Medicine 2020;26:374-8 (.claude/references/).
#
# The blood-pressure and hypertension models exist to REPLICATE that paper. They
# are a validation gate on clean_long(): if the published numbers come back, the
# cleaning is trustworthy and the same machinery can be pointed at the endpoints
# the paper never analysed (death, CVD).
#
# Published Stata specification (supplementary material):
#   mixed sbp i.intervencion i.time || codvilla: || codhogar: || codigo:,
#         cov(uns) vce(cluster codvilla)
#   keep if ht50 == 0
#   stset dtime, id(codigo) failure(ht5)
#   xi: stcox i.intervencion, share(codvilla) hr

print("Loading parameter estimation functions")

# TIME AXIS. The published methods describe "Cox's proportional hazard modeling
# on a calendar time axis". Empirically that is not what produced the published
# numbers: a calendar axis gives HR 0.643, whereas time-since-baseline gives
# 0.478 against a published 0.49. Stata's `dtime` was therefore time on study,
# not calendar date. build_survival_set() returns BOTH (`g0`/`g1` for gap time,
# `tstart`/`cal` for calendar) so the choice stays visible and testable rather
# than baked into one column.

# build_survival_set: long table -> counting-process intervals, one row per
# person-visit, truncated at the first event.
#   long        - clean_long() output; needs `cal`, `wave`, `codigo`
#   event_expr  - quoted expression evaluated per row giving the event indicator
#   at_risk     - optional quoted expression selecting the people at risk
# Person-time definition lives here alone, so calibrating it against the paper's
# 4,673.4 person-years fixes every endpoint at once.
build_survival_set <- function(long, event_expr, at_risk = NULL) {
  require(data.table)
  d <- copy(long)
  if (!is.null(at_risk)) d <- d[eval(at_risk)]
  d <- d[!is.na(cal)]
  setorder(d, codigo, cal)
  # one person contributes two rows at the same wave (018-214-02); a duplicated
  # visit date would create a zero-length interval and a duplicated event
  d <- unique(d, by = c("codigo", "cal"))

  d[, `:=`(tstart = shift(cal),
           event  = as.integer(eval(event_expr))), by = codigo]
  d <- d[!is.na(tstart) & tstart < cal]
  # truncate at the first event: keep every interval up to and including it
  d[, .ec := cumsum(fifelse(is.na(event), 0L, event)), by = codigo]
  d <- d[.ec <= 1]
  d[, event := fifelse(is.na(event), 0L, event)]
  d[, .ec := NULL]
  # gap time: years since this person's own baseline visit
  d[, `:=`(g0 = tstart - min(tstart), g1 = cal - min(tstart)), by = codigo]
  d[]
}

# person_time: events, person-years and crude rate per 100 py, by arm.
person_time <- function(surv_set) {
  require(data.table)
  out <- surv_set[, .(events = sum(event), py = sum(cal - tstart)),
                  by = intervencion][order(intervencion)]
  out[, rate_100py := 100 * events / py]
  out[, arm := fifelse(intervencion == 1L, "Intervention", "Control")]
  out[, .(arm, events, py, rate_100py)]
}

# baseline_covars: carry each person's wave-0 value to every one of their rows.
# The published adjustment set is "age, sex, education, wealth index and BMI AT
# BASELINE"; using the time-varying columns instead moves the fully adjusted HR
# from 0.489 to 0.586, so this distinction is not cosmetic.
baseline_covars <- function(long) {
  require(data.table)
  require(purrr)
  vars <- intersect(c("edad1", "sexo", "eduacat", "xassets"), names(long))
  walk(vars, \(v) long[, paste0(v, "0") := get(v)[wave == 0][1], by = codigo])
  invisible(long)
}

# fit_bp_model: the paper's equation (1), a three-level linear mixed model with
# village-clustered inference.
#   outcome - "sbp" or "dbp"
#   adjust  - "time_cluster" reproduces Table 2's first column; "full" adds the
#             baseline covariate set for the second
# Inference uses a village-clustered sandwich variance, not the model-based SE:
# the latter is roughly half the published width (0.25 vs 0.40 for SBP).
#
# CR0 with a normal critical value, because that is what Stata's
# `mixed ..., vce(cluster codvilla)` reports. CR2 is the better small-sample
# choice in principle but is not computable here -- it needs a matrix square
# root per cluster, and these clusters hold ~2,500 observations each, so the
# fit does not terminate. CR1 and CR3 are returned alongside for comparison.
fit_bp_model <- function(long, outcome = c("sbp", "dbp"),
                         adjust = c("time_cluster", "full")) {
  require(data.table)
  require(lme4)
  require(clubSandwich)
  outcome <- match.arg(outcome)
  adjust  <- match.arg(adjust)

  covars <- if (adjust == "full") {
    " + edad10 + sexo0 + eduacat0 + xassets0 + bmi0"
  } else ""
  form <- as.formula(paste0(
    outcome, " ~ intervencion + factor(wave)", covars,
    " + (1 | codigovilla/codhogar/codigo)"
  ))
  fit <- lmer(form, data = long, REML = TRUE,
              control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))

  # cluster taken from the fitted model frame, not the input table: lmer drops
  # rows with a missing outcome or covariate, so the two differ in length
  cl <- model.frame(fit)$codigovilla
  se_of <- function(type) {
    v <- try(vcovCR(fit, cluster = cl, type = type), silent = TRUE)
    if (inherits(v, "try-error")) return(NA_real_)
    sqrt(v["intervencion", "intervencion"])
  }
  est <- fixef(fit)[["intervencion"]]
  se  <- se_of("CR0")
  data.table(outcome = outcome, adjust = adjust,
             estimate = est, se = se,
             low = est - 1.96 * se, high = est + 1.96 * se,
             se_cr1 = se_of("CR1"), se_cr3 = se_of("CR3"),
             se_model = sqrt(vcov(fit)["intervencion", "intervencion"]),
             n_obs = nobs(fit), n_ids = min(ngrps(fit)),
             n_villages = max(ngrps(fit)))
}

# fit_incidence_model: shared gamma frailty Cox on the village, matching Stata's
# share(codvilla). Fitted twice -- frailtyEM::emfrail() uses the marginal
# likelihood Stata uses, survival::coxph() the penalised approximation -- and
# both are returned, because the penalised fit warns about non-convergence on
# the sparse endpoints and that warning is information, not noise.
#   axis   - "gap" (time since baseline, reproduces the paper) or "calendar"
#   adjust - "time_cluster" or "full"
fit_incidence_model <- function(surv_set, axis = c("gap", "calendar"),
                                adjust = c("time_cluster", "full"),
                                label = NA_character_) {
  require(data.table)
  require(survival)
  require(frailtyEM)
  axis   <- match.arg(axis)
  adjust <- match.arg(adjust)

  tv <- if (axis == "gap") c("g0", "g1") else c("tstart", "cal")
  covars <- if (adjust == "full") {
    " + edad10 + sexo0 + eduacat0 + xassets0 + bmi0"
  } else ""
  rhs <- paste0("intervencion", covars)
  surv <- paste0("Surv(", tv[1], ", ", tv[2], ", event)")

  # penalised-likelihood fit; capture the convergence warning rather than let
  # it print, so the report can show it as a column
  warn <- NA_character_
  cox <- withCallingHandlers(
    coxph(as.formula(paste(surv, "~", rhs, "+ frailty.gamma(codigovilla)")),
          data = surv_set),
    warning = function(w) {
      warn <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  b  <- coef(cox)["intervencion"]
  se <- sqrt(diag(vcov(cox)))["intervencion"]

  # marginal-likelihood fit
  em <- try(emfrail(
    as.formula(paste(surv, "~", rhs, "+ cluster(codigovilla)")),
    data = surv_set, distribution = emfrail_dist(dist = "gamma")
  ), silent = TRUE)
  em_hr <- if (inherits(em, "try-error")) NA_real_ else {
    exp(summary(em)$coefmat["intervencion", "coef"])
  }
  em_theta <- if (inherits(em, "try-error")) NA_real_ else summary(em)$theta[1]

  data.table(
    label     = label,
    axis      = axis,
    adjust    = adjust,
    events    = sum(surv_set$event),
    n_ids     = uniqueN(surv_set$codigo),
    py        = sum(surv_set$cal - surv_set$tstart),
    hr        = exp(b),
    low       = exp(b - 1.96 * se),
    high      = exp(b + 1.96 * se),
    hr_emfrail = em_hr,
    theta     = em_theta,
    warning   = warn
  )
}

# schoenfeld_check: the proportional-hazards test the paper reported (P = 0.40),
# fitted without the frailty term as the paper specified.
schoenfeld_check <- function(surv_set, axis = c("gap", "calendar")) {
  require(survival)
  axis <- match.arg(axis)
  tv <- if (axis == "gap") c("g0", "g1") else c("tstart", "cal")
  fit <- coxph(as.formula(paste0("Surv(", tv[1], ", ", tv[2],
                                 ", event) ~ intervencion")),
               data = surv_set)
  z <- cox.zph(fit)
  data.table(chisq = z$table["intervencion", "chisq"],
             p     = z$table["intervencion", "p"])
}

# published_targets: what the paper reported, for the replication comparison.
# Table 2 for the blood-pressure rows, Supplementary Table 3 for the incidence
# rows. `key` joins onto the reproduced estimates in replication_table().
published_targets <- function() {
  data.table(
    # NOT `key`: data.table() has a `key` formal that would swallow the column
    # and try to set it as the table key (same trap as psa_specs()' dot-prefixed
    # formals, see CLAUDE.md)
    model_id = c("sbp|time_cluster", "sbp|full", "dbp|time_cluster", "dbp|full",
                 "htn_composite|time_cluster", "htn_composite|full",
                 "htn_bponly|full"),
    quantity = c("SBP time+cluster", "SBP fully adjusted",
                 "DBP time+cluster", "DBP fully adjusted",
                 "HTN composite time+cluster", "HTN composite fully adjusted",
                 "HTN BP-only fully adjusted"),
    source = c(rep("Table 2", 4), rep("Suppl. Table 3", 3)),
    published      = c(-1.23, -1.29, -0.72, -0.76, 0.49, 0.45, 0.41),
    published_low  = c(-2.07, -2.17, -1.34, -1.39, 0.34, 0.31, 0.27),
    published_high = c(-0.38, -0.41, -0.10, -0.13, 0.71, 0.66, 0.62),
    scale = c(rep("mmHg", 4), rep("HR", 3))
  )
}

# replication_table: published value beside the reproduced one, with an explicit
# verdict. `tol` is the absolute difference tolerated on the point estimate --
# 0.10 mmHg for the BP coefficients and 0.10 on the HR scale, both roughly a
# tenth of the published effect.
replication_table <- function(bp, incidence, tol = 0.10) {
  require(data.table)
  got <- rbind(
    bp[, .(model_id = paste(outcome, adjust, sep = "|"),
           estimate, low, high)],
    incidence[, .(model_id = paste(label, adjust, sep = "|"),
                  estimate = hr, low, high)]
  )
  out <- merge(published_targets(), got, by = "model_id", all.x = TRUE,
               sort = FALSE)
  out[, diff := estimate - published]
  out[, verdict := fifelse(is.na(estimate), "not fitted",
                    fifelse(abs(diff) <= tol, "match", "differs"))]
  out[, .(quantity, source, scale, published, published_low, published_high,
          estimate, low, high, diff, verdict)]
}

# run_params_est: the single driver. Builds every endpoint, fits every model,
# and caches one list to output/params_est.rds for params_est.qmd to render --
# the same run-then-render split as run_microsim.R / salt_results.qmd.
run_params_est <- function(save = TRUE) {
  require(data.table)
  require(purrr)

  long <- clean_long()
  baseline_covars(long)
  # BP-only hypertension at baseline, for the alternative event definition
  long[, ht_bp := as.integer(sbp >= 140 | dbp >= 90)]

  # --- wide-file event validation ---
  wide_events <- clean_wide_events(long)

  # --- blood pressure: the paper's primary outcome ---
  bp <- rbindlist(map(
    list(list("sbp", "time_cluster"), list("sbp", "full"),
         list("dbp", "time_cluster"), list("dbp", "full")),
    \(a) fit_bp_model(long, outcome = a[[1]], adjust = a[[2]])
  ))

  # --- hypertension incidence: the paper's secondary outcome ---
  htn_composite <- build_survival_set(long, quote(ht5 == "Yes"),
                                      at_risk = quote(ht50 == "No"))
  htn_bponly    <- build_survival_set(long, quote(sbp >= 140 | dbp >= 90),
                                      at_risk = quote(ht50 == "No"))

  # --- new endpoints ---
  death <- build_survival_set(long, quote(dead == 1L))
  # incident CVD from the self-reported flags, prevalent cases excluded
  long[, cvd_inc := as.integer(!is.na(cvd_event_wave) & wave >= cvd_event_wave)]
  cvd_flag <- build_survival_set(long, quote(cvd_inc == 1L),
                                 at_risk = quote(cvd_prev == FALSE))
  # the two strokes the wide file corroborates with an event date
  verified <- wide_events$stroke$incident$codigo
  long[, cvd_ver := as.integer(codigo %in% verified &
                                 !is.na(cvd_event_wave) & wave >= cvd_event_wave)]
  cvd_verified <- build_survival_set(long, quote(cvd_ver == 1L),
                                     at_risk = quote(cvd_prev == FALSE))

  sets <- list(htn_composite = htn_composite, htn_bponly = htn_bponly,
               death = death, cvd_flag = cvd_flag, cvd_verified = cvd_verified)

  incidence <- rbindlist(map(names(sets), \(nm) {
    rbindlist(map(c("time_cluster", "full"), \(adj)
      fit_incidence_model(sets[[nm]], axis = "gap", adjust = adj, label = nm)))
  }), fill = TRUE)

  # calendar-axis sensitivity for the two replication endpoints, showing why
  # the gap-time axis is the one that reproduces the paper
  axis_check <- rbindlist(map(c("gap", "calendar"), \(ax)
    fit_incidence_model(htn_composite, axis = ax, adjust = "time_cluster",
                        label = "htn_composite")), fill = TRUE)

  rates <- rbindlist(map(names(sets), \(nm)
    cbind(endpoint = nm, person_time(sets[[nm]]))))

  out <- list(
    long_n      = list(rows = nrow(long), ids = uniqueN(long$codigo),
                       households = uniqueN(long$codhogar)),
    bp          = bp,
    incidence   = incidence,
    axis_check  = axis_check,
    rates       = rates,
    schoenfeld  = schoenfeld_check(htn_composite),
    wide_events = wide_events,
    replication = replication_table(bp, incidence),
    sets        = sets
  )

  if (save) {
    if (!dir.exists(here::here("output"))) dir.create(here::here("output"))
    saveRDS(out, here::here("output", "params_est.rds"))
  }
  out
}
