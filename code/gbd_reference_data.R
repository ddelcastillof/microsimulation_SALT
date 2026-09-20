#--------------------------------
# GBD reference extracts for the lifetime horizon
#--------------------------------
#
# Writes the two reference tables the lifetime microsimulation needs:
#
#   data/gbd_mortality_peru.csv       age x sex mortality and case fatality
#   data/gbd_reference_life_table.csv standard life expectancy for YLL
#
# Both are checked in, like data/gbd_ihd_stroke_peru.csv, because they are
# small, static reference data rather than derived output. Re-run this script
# only when moving to a new GBD round.
#
# PROVENANCE
# ----------
# Mortality: IHME Global Burden of Disease Study 2023, Peru (location_id 123),
# year 2023, measure "deaths", sex-specific rates per 100,000, on GBD's
# five-year age partition. Pulled cause by cause: "All causes", "Ischemic
# heart disease", "Stroke".
#
# Incidence: read from the existing data/gbd_ihd_stroke_peru.csv, so the
# case-fatality denominator is exactly the incidence the risk module already
# calibrates against (see methods-discussions.md section 6). Keeping one
# incidence source means the two GBD-derived quantities cannot drift apart.
#
# The "<20" band: GBD reports deaths from "15 to 19" downward in separate
# child and neonatal bands, but gbd_ihd_stroke_peru.csv collapses everything
# under 20 into one row. The trial's youngest participant is 18, so the only
# ages that ever occupy this band are 18-19 -- for at most five cycles at the
# very start of the horizon. This script therefore assigns the 15-19 death
# rates to the <20 band rather than an all-under-20 average, which would
# blend in neonatal mortality and be badly wrong for an 18-year-old.

here::i_am("salt_results.qmd")
require(data.table)

# --- GBD 2023 Peru, deaths per 100,000, by five-year band and sex -----------
bands <- c(0, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)

# "All causes". The first entry is the 15-19 band (see the <20 note above).
allcause_m <- c(98.4, 147.4, 180.6, 201.4, 240.4, 288.4, 363.4, 497.5, 730.1,
                1090.6, 1621.9, 2553.9, 4215.4, 7153.2, 15380.5, 23090.5, 28642.3)
allcause_f <- c(59.5, 70.4, 80.4, 93.0, 131.1, 183.7, 267.3, 378.5, 539.6,
                797.7, 1233.2, 1909.0, 3130.3, 5712.7, 12704.4, 20646.0, 26707.5)

# "Ischemic heart disease"
ihd_d_m <- c(2.3, 3.9, 5.3, 8.9, 12.2, 16.7, 24.4, 35.8, 53.9,
             89.8, 129.1, 223.7, 351.4, 629.2, 1570.4, 2547.3, 3030.2)
ihd_d_f <- c(1.1, 1.2, 1.8, 2.3, 3.2, 5.5, 9.1, 14.4, 22.8,
             36.0, 62.1, 122.9, 202.9, 420.7, 1136.9, 2125.3, 2635.2)

# "Stroke"
str_d_m <- c(3.2, 3.8, 6.1, 6.9, 8.1, 12.0, 16.4, 24.3, 36.3,
             58.3, 84.9, 142.5, 246.8, 412.1, 972.6, 1341.2, 1272.0)
str_d_f <- c(2.5, 2.9, 3.6, 3.9, 5.3, 9.4, 14.4, 19.4, 28.7,
             44.7, 72.4, 121.6, 202.7, 358.4, 881.0, 1369.6, 1392.2)

deaths <- rbindlist(list(
  data.table(age_lower = bands, sex = "male",
             allcause_death_rate = allcause_m,
             ihd_stroke_death_rate = ihd_d_m + str_d_m),
  data.table(age_lower = bands, sex = "female",
             allcause_death_rate = allcause_f,
             ihd_stroke_death_rate = ihd_d_f + str_d_f)
))

# --- incidence, reused from the file the risk module already calibrates on --
inc <- fread(here::here("data", "gbd_ihd_stroke_peru.csv"))
inc[, ihd_stroke_inc_rate := ihd_rate + stroke_rate]

out <- merge(deaths, inc[, .(age_lower, sex, age_band, ihd_stroke_inc_rate)],
             by = c("age_lower", "sex"), all.x = TRUE)
setcolorder(out, c("age_band", "age_lower", "sex"))
setorder(out, sex, age_lower)

# Non-CVD (background) mortality is all-cause NET of IHD + stroke. This
# subtraction is not cosmetic: the Healthy -> Dead arm in transition.R must
# carry non-CVD death only, because CVD deaths are routed separately through
# the CVD state. Leaving it in would double-count them.
stopifnot(all(out$allcause_death_rate > out$ihd_stroke_death_rate))

# --- acute case fatality ----------------------------------------------------
# p_cvd_fatal is the probability of dying IN THE CYCLE OF THE EVENT, because
# CVD is a one-cycle tunnel state and every later cycle is PostCVD, whose
# mortality is modelled separately as background x hr_postcvd.
#
# It is therefore 28-day case fatality, and it is NOT GBD deaths / incidence.
# That ratio is a steady-state quantity: at older ages the deaths come from the
# accumulated prevalent pool while the denominator counts only new events, so
# it measures lifetime fatality per case and exceeds 1.0 from age 85 (peaking
# at 1.54). Using it here would both double-count the chronic deaths already
# carried by p_postcvd and produce an invalid probability.
#
# Levels below are published 28-day case fatality for stroke and myocardial
# infarction combined, age-graded. PLACEHOLDER: replace with the specific
# sources the write-up cites; cf_calib carries the level uncertainty in the PSA.
acute_cf <- function(age_lower) {
  fifelse(age_lower < 60, 0.15,
  fifelse(age_lower < 70, 0.20,
  fifelse(age_lower < 80, 0.28, 0.40)))
}
out[, acute_cf := acute_cf(age_lower)]

# The GBD ratio is kept as a column, unused by the model, so the comparison
# stays visible and auditable rather than living only in a commit message.
out[, gbd_lifetime_fatality_ratio := ihd_stroke_death_rate / ihd_stroke_inc_rate]

stopifnot(all(out$acute_cf > 0 & out$acute_cf < 1))

# Diagnostic, not an assertion. One would expect acute fatality to sit below
# the lifetime ratio, since acute deaths are a subset of all deaths from a
# case. It does across the ages that carry the trial's person-time, but not in
# the youngest bands: GBD stroke incidence under 40 is dominated by
# non-atherosclerotic causes with low fatality, so the lifetime ratio there
# dips below a 28-day figure drawn from adult stroke and MI cohorts. The two
# come from different sources measuring different populations, so this is
# reported rather than enforced.
bad <- out[gbd_lifetime_fatality_ratio < 1 & acute_cf >= gbd_lifetime_fatality_ratio]
if (nrow(bad)) {
  message("acute_cf exceeds the GBD lifetime ratio in ", nrow(bad),
          " band(s) (youngest: age ", min(bad$age_lower), "); see comment above")
}

fwrite(out, here::here("data", "gbd_mortality_peru.csv"))
message("wrote data/gbd_mortality_peru.csv (", nrow(out), " rows)")

# --- GBD standard reference life table --------------------------------------
# Used for YLL: a death contributes the standard life expectancy at its age,
# which is the GBD DALY definition and is consistent with the GBD disability
# weights already in params$dw.
#
# CAVEAT: the GBD 2023 server exposes no reference-life-table dataset, so these
# are the published GBD 2019 standard values, which GBD has carried forward
# unchanged since. VERIFY against the GBD appendix before publication.
#
# This is the normative "aspirational" table, deliberately NOT Peru's own
# period life expectancy -- that alternative is available from the GBD tools
# and would give lower YLL per death and hence a higher ICER.
sle <- data.table(
  age = c(0, 1, seq(5, 95, by = 5)),
  sle = c(88.87, 88.00, 84.03, 79.05, 74.07, 69.11, 64.15, 59.20, 54.25,
          49.32, 44.43, 39.63, 34.91, 30.25, 25.68, 21.28, 17.10, 13.20,
          9.70, 6.77, 4.60)
)
stopifnot(!is.unsorted(rev(sle$sle)))     # monotonically declining with age
fwrite(sle, here::here("data", "gbd_reference_life_table.csv"))
message("wrote data/gbd_reference_life_table.csv (", nrow(sle), " rows)")
