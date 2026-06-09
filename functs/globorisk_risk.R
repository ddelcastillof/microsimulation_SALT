#--------------------------------
# Globorisk CVD risk module
#--------------------------------

print("Loading Globorisk risk functions")

# risk10_to_cycle: convert a 10-year cumulative CVD risk to a per-cycle
# probability, assuming a constant hazard. cycle_length is in years.
risk10_to_cycle <- function(risk10, cycle_length) {
  rate <- -log(1 - risk10) / 10
  1 - exp(-rate * cycle_length)
}

# cvd_risk: per-cycle CVD probability for each individual at one wave,
# using the Globorisk office (non-laboratory) equation.
# `dt` must have columns: sex_num (0 = female, 1 = male), age, sbp,
# smoking (0/1), bmi, diabetes (0/1).
#
# Age handling: Globorisk is only validated for ages 40-74.
#   - Under 40: by design, scored as a 40-year-old (their other risk
#     factors are kept). This is an intentional modelling assumption,
#     not a data anomaly, so it is applied silently.
#   - Over 74: clamped down to 74 and warned, since this reflects ages
#     outside the model's intended use.
cvd_risk <- function(dt, params) {
  require(globorisk)
  age <- dt$age
  above_range <- age > 74
  if (any(above_range, na.rm = TRUE)) {
    warning(sum(above_range, na.rm = TRUE),
            " participant-rows above Globorisk age range (>74); clamping to 74.")
  }
  # Floor under-40 ages at 40 (intentional) and cap over-74 ages at 74.
  age_c <- pmin(pmax(age, 40), 74)
  r10 <- globorisk(
    sex     = dt$sex_num,
    age     = age_c,
    sbp     = dt$sbp,
    tc      = NA_real_,                       # office version omits cholesterol
    dm      = dt$diabetes,
    smk     = dt$smoking,
    bmi     = dt$bmi,
    iso     = params$globorisk$iso,
    year    = params$globorisk$year,
    version = params$globorisk$version,
    type    = "risk"                          # 10-yr absolute CVD risk (always a probability)
  )
  risk10_to_cycle(r10, params$cycle_length)
}
