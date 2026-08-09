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

# Cache for the GBD lookup: cvd_risk() is called once per arm per cycle, so
# re-reading and re-deriving the table on every call would be wasteful.
.gbd_cache <- new.env(parent = emptyenv())

# load_gbd_rates: read the GBD Peru 2023 incidence extract and derive the
# age multipliers used to extend Globorisk beyond its validated 40-74 window.
#
# The multiplier is a ratio of two GBD rates, anchored at the band adjacent to
# each clamp boundary (40-44 below, 70-74 above). Because it is a ratio, any
# constant offset between the GBD endpoint (incident IHD + stroke) and the
# Globorisk endpoint (fatal or non-fatal CVD event) cancels: only the relative
# age gradient is imported, never GBD's absolute level. The multiplier is
# exactly 1 across 40-74, so the correction is continuous at both seams.
#
# Bands at 90+ hold only a few dozen trial person-waves but would carry the
# largest multipliers, so 85-89, 90-94 and 95+ are collapsed into a single
# 85+ band taking the 85-89 rate. This is deliberately conservative.
load_gbd_rates <- function(path = here::here("data", "gbd_ihd_stroke_peru.csv")) {
  require(data.table)
  if (!is.null(.gbd_cache[[path]])) return(.gbd_cache[[path]])

  g <- fread(path)
  g[, rate := ihd_rate + stroke_rate]
  g <- g[age_lower <= 85]                       # collapse the sparse 85+ tail
  setorder(g, sex, age_lower)

  # per-sex anchor rates at the two boundary bands
  a40 <- g[age_lower == 40][match(g$sex, sex), rate]
  a70 <- g[age_lower == 70][match(g$sex, sex), rate]
  g[, mult := fifelse(age_lower < 40, rate / a40,
              fifelse(age_lower >= 75, rate / a70, 1))]
  g[, sex_num := as.integer(sex == "male")]

  .gbd_cache[[path]] <- g[]
  g[]
}

# gbd_age_multiplier: cumulative-hazard multiplier for each individual, given
# age and sex (0 = female, 1 = male). Returns 1 inside 40-74, <1 below 40 and
# >1 above 74. NA age propagates to NA.
gbd_age_multiplier <- function(age, sex_num, rates = load_gbd_rates()) {
  require(data.table)
  breaks  <- sort(unique(rates$age_lower))
  idx     <- findInterval(age, breaks)
  idx[!is.na(idx) & idx < 1L] <- NA_integer_
  band_lo <- breaks[idx]
  rates$mult[match(paste(sex_num, band_lo),
                   paste(rates$sex_num, rates$age_lower))]
}

# cvd_risk: per-cycle CVD probability for each individual at one wave,
# using the Globorisk office (non-laboratory) equation.
# `dt` must have columns: sex_num (0 = female, 1 = male), age, sbp,
# smoking (0/1), bmi, diabetes (0/1).
#
# Age handling: Globorisk is only validated for ages 40-74, so age is clamped
# into that window before scoring. When params$globorisk$extrapolate is TRUE
# the resulting risk is then rescaled by gbd_age_multiplier(), which restores
# the age gradient the clamp flattens. The rescaling is applied on the
# cumulative-hazard scale, so the SBP hazard ratio passes through unchanged
# and the treatment effect stays estimable at every age.
#
# With extrapolate = FALSE the plain clamp is used and over-74 rows warn,
# since those ages then sit outside the model's intended use.
cvd_risk <- function(dt, params) {
  require(globorisk)
  age    <- dt$age
  extrap <- isTRUE(params$globorisk$extrapolate)

  if (extrap) {
    mult <- gbd_age_multiplier(age, dt$sex_num)
  } else {
    above_range <- age > 74
    if (any(above_range, na.rm = TRUE)) {
      warning(sum(above_range, na.rm = TRUE),
              " participant-rows above Globorisk age range (>74); clamping to 74.")
    }
    mult <- 1
  }

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
  r10 <- 1 - (1 - r10)^mult
  risk10_to_cycle(r10, params$cycle_length)
}
