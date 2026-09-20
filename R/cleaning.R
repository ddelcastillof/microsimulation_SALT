#-------------------
# Cleaning functions
#-------------------

print("Loading cleaning functions...")

# Function 1: Importing dataset in both long and wide format

import_data <- function(format = c("wide", "long")) {
  require(tidyverse)
  require(data.table)
  require(fst)
  require(haven)

  format <- match.arg(format)
  dta_path <- here::here("data", "raw", paste0(format, ".dta"))
  fst_path <- here::here("data", paste0(format, ".fst"))

  if (!dir.exists(here::here("data"))) {
    dir.create(here::here("data"))
    message("Created 'data' directory.")
  }

  if (!file.exists(dta_path)) {
    stop(paste0("Raw data file '", format,
                ".dta' not found in 'data/raw'."))
  }

  if (!file.exists(fst_path)) {
    message("Reading and cleaning '", format, ".dta'...")
    data_in <- read_stata(dta_path, .name_repair = "minimal") |>
      mutate(across(where(is.labelled), as_factor)) |>
      zap_label() |>
      zap_formats() |>
      zap_widths()

    message("Creating FST file '", format, ".fst'...")
    fst::write_fst(data_in, fst_path)
  } else {
    message("'", format, ".fst' already exists, skipping.")
  }

  read_fst(fst_path) |> as.data.table()
}

# Function 1.1: Recoding Stata value labels into one English vocabulary
#
# The raw labels arrive mixed: some columns are already English (db2 = No/Yes,
# smoking1 = Never/Smoker), others are Spanish (sexo = Mujer/Hombre, the CVD
# history flags = No/Si). Downstream code tests those levels by string, so a
# single convention is what keeps build_cohort() from silently coding a
# constant. Binary columns all end up No/Yes.
#
# Modifies `data` by reference and returns it invisibly. Levels that are not
# present are dropped from the map first, because fct_recode() warns on them.

english_labels <- function(data) {
  require(data.table)
  require(forcats)
  require(purrr)

  maps <- list(
    sexo     = c(Female = "Mujer", Male = "Hombre"),
    ecivil   = c(Single = "Soltero", Married = "Casado",
                 Cohabiting = "Conviviente", Separated = "Separado",
                 Divorced = "Divorciado", Widowed = "Viudo",
                 Unknown = "No sabe"),
    ecivil1  = c(Single = "Soltero", Partnered = "Con Pareja",
                 Unpartnered = "Sin Pareja"),
    niveduca = c(None = "Sin nivel", Preschool = "Inicial",
                 Primary = "Primaria", Secondary = "Secundaria",
                 `Non-university tertiary` = "Superior NO universitaria",
                 University = "Superior universitaria")
  )
  # Si / Sí -> Yes, so every binary column reads No/Yes
  yes_no_cols <- c("derrame", "infarto", "insuficiencia", "otracor",
                   "colesterol", "pvivo")
  maps <- c(maps, map(set_names(yes_no_cols), \(v) c(Yes = "Si", Yes = "Sí")))

  # walk: set() recodes by reference and the return value is discarded
  walk(intersect(names(maps), names(data)), \(v) {
    if (!is.factor(data[[v]])) return(invisible(NULL))
    m <- maps[[v]]
    m <- m[m %in% levels(data[[v]])]
    if (!length(m)) return(invisible(NULL))
    set(data, j = v,
        value = do.call(fct_recode, c(list(data[[v]]), as.list(m))))
  })

  invisible(data)
}

# Function 2: Cleaning data in long format, including assigning wave numbers and filling in missing waves

clean_long <- function() {
  require(data.table)
  require(lubridate)
  require(forcats)
  require(purrr)
  tidied_data <- import_data("long")
  
  cols_to_select <- c(
      "entvilla",
      "codigo",
      "codigogen",
      "codigovilla",
      "codigovivienda",
      "codigofam",
      "codigopersona",
      # codhogar is the household (1,047 units). codigovivienda has 280 and
      # codigofam only 2, so neither can serve as the family random intercept
      # the Bernabe-Ortiz model needs (|| codhogar:)
      "codhogar",
      "fecha",
      "time",
      "intervencion",
      "sexo",
      "fec_nac",
      "edad1",
      "ecivil",
      "ecivil1",
      "dbdx",
      "db2",
      "talla_par",
      "peso",
      "bmi",
      "phq",
      "phq1",
      "phq2",
      "sbp",
      "dbp",
      "preht",
      "hts",
      "htd",
      "htdx",
      "htdxtx",
      "ht5",
      "derrame",
      "infarto",
      "insuficiencia",
      "otracor",
      "colesterol",
      "smoking1",
      "smoking2",
      "pvivo",
      "dni",
      "f_muerte",
      "c_muerte",
      "assets",
      "xassets",
      "niveduca",
      # education as the published analysis coded it: eduacat is the 3-category
      # <7 / 7-11 / >=12 YEARS split, edua the underlying count. niveduca is
      # school LEVEL and cannot reproduce i.eduacat
      "eduacat",
      "edua",
      "eqindex",
      "disaprev1",
      "disaprev2",
      "disaprev3"
    )
  
  tidied_data <- tidied_data[, ..cols_to_select]
  # transforming date variables into date format
  tidied_data[, c("fecha", "fec_nac") := lapply(.SD, dmy),
              .SDcols = c("fecha", "fec_nac")]
  # f_muerte is stored as character in Stata; empty string encodes missing
  tidied_data[f_muerte == "", f_muerte := NA_character_]
  tidied_data[, f_muerte := dmy(f_muerte)]
  # one English vocabulary for every factor level, so downstream string tests
  # match a single convention (see english_labels below)
  english_labels(tidied_data)
  # collapsing superior education categories
  tidied_data[, niveduca2 := fct_collapse(
    niveduca,
    Tertiary = c("Non-university tertiary", "University")
  )]
  # assigning wave numbers, filling missing from 0:6
  # wave will represent visit number, with 0 being baseline
  tidied_data[, wave := as.integer(time) - 1L]
  tidied_data[, wave := {
    missing_waves <- setdiff(0:6, wave[!is.na(wave)])
    wave[is.na(wave)] <- sort(missing_waves)
    wave
  }, by = codigo]

  # rebuilding the treatment indicator from the crossover schedule.
  # the stored intervencion is 0 on every placeholder row (no `time`) and on a
  # handful of stragglers in villages that had already crossed over, so it
  # understates treated person-time. The crossover wave is read back from the
  # data as the first wave where the village is majority-treated, which
  # reproduces data/village_order.yaml without depending on that file.
  print(dcast(tidied_data[!is.na(time), .N,
                          by = .(codigovilla, wave, intervencion)],
              codigovilla + intervencion ~ wave, value.var = "N", fill = 0))
  tidied_data[, crossover := mean(intervencion[!is.na(time)] == 1, na.rm = TRUE),
              by = .(codigovilla, wave)]
  tidied_data[, crossover := {
    # which(), not logical subsetting: a village x wave group made up entirely
    # of placeholder rows has a NaN share, and wave[NaN > 0.5] would inject an
    # NA that propagates through min() and blanks the whole village
    w <- wave[which(crossover > 0.5)]
    if (length(w)) min(w) else NA_integer_
  }, by = codigovilla]
  tidied_data[, intervencion := as.integer(!is.na(crossover) & wave >= crossover)]
  print(tidied_data[, .(crossover_wave = crossover[1],
                        treated_rows   = sum(intervencion)), by = codigovilla][
    order(crossover_wave)])
  # scaffolding only; intervencion carries the schedule from here on
  tidied_data[, crossover := NULL]

  # for those with no time variable with comorbidities as measured, set these to NA
  cat_cols <- c("db2", "smoking1", "smoking2", "infarto", "derrame",
                "insuficiencia", "otracor", "colesterol")
  tidied_data[is.na(time), (cat_cols) := NA]

  # ensuring age is an integer
  tidied_data[, age := as.integer(edad1)]

  # calendar time in years from the first study visit. This is the survival
  # time axis the published incidence analysis used ("Cox's proportional
  # hazard modeling on a calendar time axis, to account for time trends")
  tidied_data[, cal := as.numeric(fecha - min(fecha, na.rm = TRUE)) / 365.25]

  # baseline hypertension, matching Stata's ht50: fixed at wave 0 and repeated
  # on every row of a person. bmi0 is built further down, after the bmi repair
  tidied_data[, ht50 := ht5[wave == 0][1], by = codigo]

  # mortality fixes from the pvivo x f_muerte diagnostics
  # 020-153-02 carries a death date at wave 1 but has full visits at waves 2-3,
  # so the wave 1 date is dropped and the post-death wave 3 visit is blanked
  id_cols <- c("codigo", "codigogen", "codigovilla", "codigovivienda",
               "codigofam", "codigopersona", "entvilla", "wave", "time")
  tidied_data[codigo == "020-153-02" & wave == 1, f_muerte := NA]
  tidied_data[codigo == "020-153-02" & wave == 3,
              (setdiff(names(tidied_data), id_cols)) := NA]

  # checking bmi/height/weight variables: diagnose first, then recode
  # the -1 sentinel (Stata missing that survived zap_*) to NA
  anthro_cols <- c("bmi", "talla_par", "peso")
  ## visual inspection
  tidied_data[, sort(unique(peso))]
  tidied_data[, sort(unique(talla_par))]
  tidied_data[, sort(unique(bmi))]
  ## implausible values of height
  tidied_data[, .(n_neg = sum(talla_par < 0, na.rm = TRUE),
                  n_zero = sum(talla_par == 0, na.rm = TRUE),
                  min_130 = sum(talla_par < 130, na.rm = TRUE),
                  min_120 = sum(talla_par < 120, na.rm = TRUE), # clamping here
                  max = max(talla_par, na.rm = TRUE))]
  ## all values lower than 120 cm changed as NA 
  tidied_data[talla_par < 120, talla_par := NA_real_]
  ## implausible values of weight
  tidied_data[, .(n_neg = sum(peso < 0, na.rm = TRUE),
                  n_zero = sum(peso == 0, na.rm = TRUE),
                  min_30 = sum(peso < 30, na.rm = TRUE), # clamping here
                  max = max(peso, na.rm = TRUE))]
  ## all values lower than 30 are negatives, so they will be NAs
  tidied_data[peso < 30, peso := NA_real_]
  ## BMI that are not the transformation of weight and height as well as other implausible values
  ## tolerance, not exact equality: the stored bmi was computed from height
  ## rounded to 2 dp in metres, which shifts it by up to ~0.2 units on its own
  bmi_tol <- 0.5
  print(tidied_data[, .(n_neg  = sum(bmi < 0, na.rm = TRUE),
                        n_zero = sum(bmi == 0, na.rm = TRUE),
                        min_15 = sum(bmi < 15, na.rm = TRUE),
                        max_45 = sum(bmi > 45, na.rm = TRUE),
                        max    = max(bmi, na.rm = TRUE),
                        n_comparable = sum(!is.na(bmi) & !is.na(peso) &
                                             !is.na(talla_par)),
                        no_correspond_to_trans = sum(
                          abs(bmi - peso / (talla_par / 100)^2) > bmi_tol,
                          na.rm = TRUE))])
  ## repairing negatives first as NA
  tidied_data[bmi < 0, bmi := NA_real_]
  ## mark the rows whose stored bmi is not the transformation of the measured
  ## weight and height, then overwrite them with the recomputed value.
  ## bmi_mismatch stays in the table so the repair is auditable downstream
  tidied_data[, bmi_recalc := peso / (talla_par / 100)^2]
  tidied_data[, bmi_mismatch := !is.na(bmi) & !is.na(bmi_recalc) &
                abs(bmi - bmi_recalc) > bmi_tol]
  tidied_data[bmi_mismatch == TRUE, bmi := bmi_recalc]
  tidied_data[, bmi_recalc := NULL]
  print(tidied_data[, .(n_marked = sum(bmi_mismatch),
                        n_ids_marked = uniqueN(codigo[bmi_mismatch]),
                        max_gap_left = max(abs(bmi - peso / (talla_par / 100)^2),
                                           na.rm = TRUE))])

  print(melt(tidied_data, id.vars = "codigo", measure.vars = anthro_cols,
             variable.name = "var")[, .(
               n_na   = sum(is.na(value)),
               n_neg  = sum(value < 0, na.rm = TRUE),
               min    = min(value, na.rm = TRUE),
               median = median(value, na.rm = TRUE),
               max    = max(value, na.rm = TRUE)
             ), by = var])

  ## baseline BMI carried to every row, matching Stata's bmi0. Built here
  ## rather than with ht50 so it snapshots the REPAIRED bmi, not the raw one
  tidied_data[, bmi0 := bmi[wave == 0][1], by = codigo]

  # cleaning CVD history variables
  cvd_cols <- c("derrame", "infarto", "insuficiencia", "otracor",
                "colesterol", "smoking1", "smoking2")
  ## what levels and frequency
  print(dcast(rbindlist(map(cvd_cols, \(v) {
    tt <- table(as.character(tidied_data[[v]]), useNA = "ifany")
    data.table(var = v, level = names(tt), n = as.integer(tt))
  })), var ~ level, value.var = "n", fill = 0))

  ## how many persons revert cvd occurences
  hist_cols <- c("derrame", "infarto", "insuficiencia", "otracor", "colesterol")
  print(rbindlist(map(hist_cols, \(v) {
    z <- tidied_data[!is.na(get(v)),
                     .(codigo, wave, yes = get(v) == "Yes")][order(codigo, wave)]
    z[, .(ever = any(yes),
          revierte = any(diff(as.integer(yes)) < 0)), by = codigo][
      , .(var = v, ids_con_dato = .N, ids_ever_yes = sum(ever),
          ids_que_revierten = sum(revierte))]
  })))

  ## in which visit appear the first ocurrence. If wave ==0 was in the past (prevalent)
  ## if wave >=1 is incident during the trial. NA if never occurs
  print(rbindlist(map(hist_cols, \(v) {
    tidied_data[get(v) == "Yes", .(first_wave = min(wave)), by = codigo][
      , .(var = v, n_ids = .N, prevalentes = sum(first_wave == 0),
          incidentes = sum(first_wave > 0))]
  })))

  ## coherency between two smoking vars
  print(tidied_data[, .N, by = .(smoking1, smoking2)][order(smoking1, smoking2)])

  ## cleaning CVD
  ## if any occured in a missing visit, is NA
  ## for prevalent and incident visits a cvd absorbent marker will be created
  event_cols <- c("derrame", "infarto", "insuficiencia", "otracor")
  setorder(tidied_data, codigo, wave)
  # cvd_any: was any event in this visit? (NA if no data for this visit)
  tidied_data[, cvd_any := reduce(map(.SD, \(x) x == "Yes"), `|`),
              .SDcols = event_cols]
  # baseline prevalence: was any event ever reported at wave 0?
  tidied_data[, cvd_prev := any(cvd_any[wave == 0] %in% TRUE), by = codigo]
  # first incident wave: the first wave >= 1 with any event, or NA if none
  tidied_data[, cvd_event_wave := {
    w <- wave[cvd_any %in% TRUE & wave >= 1]
    if (length(w)) min(w) else NA_integer_
  }, by = codigo]
  # absorbent state: has any event ever been reported up to and including this wave?
  tidied_data[, cvd_hist := cummax(as.integer(cvd_any %in% TRUE)) > 0,
              by = codigo]

  print(tidied_data[, .(ids                = uniqueN(codigo),
                        ids_prevalentes    = uniqueN(codigo[cvd_prev]),
                        ids_incidentes     = uniqueN(codigo[!is.na(cvd_event_wave)]),
                        ids_cvd_hist       = uniqueN(codigo[cvd_hist]),
                        filas_cvd_hist     = sum(cvd_hist))])

  # cleaning mortality variables: diagnose first, then decide
  ## c_muerte carries two Stata missing sentinels, blank and -2
  print(tidied_data[, .N, by = .(c_muerte_missing = c_muerte %in% c("", "-2"))])
  tidied_data[c_muerte %in% c("", "-2"), c_muerte := NA_character_]
  ## is the death date a person-level fact, or does it conflict across waves?
  print(tidied_data[!is.na(f_muerte),
                    .(n_rows = .N, n_dates = uniqueN(f_muerte)), by = codigo][
    , .(ids = .N, ids_repeated_row = sum(n_rows > 1),
        ids_conflicting_date = sum(n_dates > 1))])
  ## pvivo against the presence of a date
  print(tidied_data[, .N, by = .(pvivo, has_date = !is.na(f_muerte))][
    order(pvivo, has_date)])
  ## one date per person, so carry it to every row of that person
  tidied_data[, death_date := f_muerte[!is.na(f_muerte)][1], by = codigo]
  ## the record is entered at the visit that found the person dead, so its
  ## visit date is later than the death itself
  print(tidied_data[!is.na(death_date) & !is.na(fecha) & fecha > death_date,
                    .(n_rows = .N, ids = uniqueN(codigo),
                      with_sbp = sum(!is.na(sbp)),
                      lag_days_median = median(as.numeric(fecha - death_date)))])
  ## death_wave: the cycle whose interval contains the death, i.e. the first
  ## follow-up dated on or after it. Placeholder rows have no date, so fall
  ## back to the wave where the record was entered
  tidied_data[, death_wave := {
    dated <- wave[wave >= 1 & !is.na(fecha) & !is.na(death_date) &
                    fecha >= death_date]
    entered <- wave[wave >= 1 & !is.na(f_muerte)]
    if (length(dated)) min(dated)
    else if (length(entered)) min(entered)
    else NA_integer_
  }, by = codigo]
  ## dead marks the single cycle of death; at_risk closes person-time after it
  tidied_data[, dead := as.integer(!is.na(death_wave) & wave == death_wave)]
  tidied_data[, at_risk := is.na(death_wave) | wave <= death_wave]
  ## cause of death from the free-text field, for the CVD-fatality probability
  tidied_data[, cvd_death := as.integer(grepl(
    "cardio|infarto|derrame|cardiaco", tolower(c_muerte)))]
  tidied_data[, cvd_death := as.integer(any(cvd_death == 1L)), by = codigo]

  print(tidied_data[, .(ids                    = uniqueN(codigo),
                        ids_muertos            = uniqueN(codigo[!is.na(death_wave)]),
                        ids_muerte_cvd         = uniqueN(codigo[dead == 1L &
                                                                  cvd_death == 1L]),
                        filas_dead             = sum(dead),
                        filas_post_muerte      = sum(!at_risk),
                        post_muerte_con_sbp    = sum(!at_risk & !is.na(sbp)))])

  return(tidied_data)
}

# Function 2.1. Helper function to preprocessing additional variables

prep_long <- function(data) {
  # `age` now comes from clean_long(); this helper only builds pool_cvd

  # creating a variable for pool_cvd risk factor count (0-5, counting ht5, insuficiencia, infarto, derrame, otracor)
  # factor columns coded as No/Si (or No/Yes) -> 0/1 via as.integer() - 1L before summing
  pool_cols <- c("ht5", "insuficiencia", "infarto", "derrame", "otracor")
  data[, pool_cvd := rowSums(
    sapply(.SD, function(x) {
      if (is.factor(x)) as.integer(x) - 1L else as.integer(x)
    }),
    na.rm = TRUE
  ), .SDcols = pool_cols]
}

# Function 2.2: Reconstructing the per-wave event record from the wide file
#
# The wide file stores one column block per visit: baseline columns carry no
# prefix, follow-ups are prefixed f1..f6. It keeps event-DATE fields that the
# long file drops, and those dates are the only way to separate a genuinely
# incident event from a re-report of an old one. `derrame` is the only CVD
# variable with them: `derrame_a` gives years-since-stroke at baseline, and
# `f{i}derramec_m` / `f{i}derramec_a` give the month and 2-digit year of a
# stroke reported at follow-up i.
#
# Returns a list:
#   long   - reshaped person x wave event table, one row per person-wave
#   recon  - wide vs long counts per variable; a mismatch means a cleaning bug
#   stroke - the dated stroke records, prevalent and incident
#   revert - per-variable reversion counts, the evidence that the flag-only
#            variables measure recall rather than incidence
#
# `long_data` is the clean_long() output to reconcile against; pass NULL to
# skip the reconciliation and get the wide side alone.

clean_wide_events <- function(long_data = NULL) {
  require(data.table)
  require(purrr)

  wide <- import_data("wide")
  ev_cols <- c("derrame", "infarto", "insuficiencia", "otracor", "ht5", "pvivo")

  # melt one variable's 7-column block into person x wave. Baseline is the
  # unprefixed column, so waves run 0:6 and line up with clean_long()'s `wave`.
  melt_block <- function(v) {
    cols <- c(v, paste0("f", 1:6, v))
    missing <- setdiff(cols, names(wide))
    if (length(missing)) {
      stop("wide file is missing ", paste(missing, collapse = ", "))
    }
    x <- wide[, c("codigo", ..cols)]
    setnames(x, c("codigo", paste0("w", 0:6)))
    # value.factor: melt returns character by default even when every source
    # column is a factor with identical levels, which would make
    # english_labels() skip the column and leave the Spanish "Si" in place
    m <- melt(x, id.vars = "codigo", variable.name = "wave", value.name = v,
              value.factor = TRUE)
    m[, wave := as.integer(sub("w", "", wave))][]
  }
  wl <- reduce(map(ev_cols, melt_block),
               \(a, b) merge(a, b, by = c("codigo", "wave")))
  # the wide file keeps raw Stata labels; english_labels() only touches the
  # columns it knows, so recode the block copies to the same No/Yes vocabulary
  english_labels(wl)
  setorder(wl, codigo, wave)

  cvd_cols <- c("derrame", "infarto", "insuficiencia", "otracor")

  # --- reversion: how often does a reported "Yes" become "No" again? ---
  revert <- rbindlist(map(cvd_cols, \(v) {
    z <- wl[!is.na(get(v)), .(codigo, wave, yes = get(v) == "Yes")]
    s <- z[, .(ever  = any(yes),
               prev  = any(yes[wave == 0]),
               first = if (any(yes)) min(wave[yes]) else NA_integer_,
               rev   = any(diff(as.integer(yes)) < 0)), by = codigo]
    data.table(var = v, ever = sum(s$ever), prevalent = sum(s$prev),
               first_after_w0 = sum(s$first > 0, na.rm = TRUE),
               reverts = sum(s$rev, na.rm = TRUE))
  }))

  # --- dated strokes: the only corroborated CVD events in the trial ---
  base_stroke <- wide[derrame == "Si" | derrame == "Yes",
                      .(codigo, wave = 0L, years_since = as.character(derrame_a),
                        confirmed = as.character(derramece))]
  fu_stroke <- rbindlist(map(1:6, \(i) {
    idx <- which(wide[[paste0("f", i, "derrame")]] %in% c("Si", "Yes"))
    if (!length(idx)) return(NULL)
    data.table(codigo      = wide$codigo[idx],
               wave        = i,
               event_month = wide[[paste0("f", i, "derramec_m")]][idx],
               event_year  = wide[[paste0("f", i, "derramec_a")]][idx],
               confirmed   = as.character(wide[[paste0("f", i, "derramece")]][idx]))
  }))
  # 2-digit year, all within the 2014-2017 trial window
  if (nrow(fu_stroke)) {
    fu_stroke[, event_date := as.Date(sprintf("%04d-%02d-01",
                                              2000L + event_year, event_month))]
  }
  stroke <- list(prevalent = base_stroke, incident = fu_stroke)

  # --- reconciliation against clean_long() ---
  recon <- NULL
  if (!is.null(long_data)) {
    count_yes <- function(dt, src) {
      rbindlist(map(intersect(c(cvd_cols, "ht5"), names(dt)), \(v)
        data.table(source = src, var = v,
                   n_yes = sum(dt[[v]] == "Yes", na.rm = TRUE))))
    }
    recon <- dcast(rbind(count_yes(wl, "wide"), count_yes(long_data, "long")),
                   var ~ source, value.var = "n_yes")
    recon[, match := wide == long]
  }

  list(long = wl, recon = recon, stroke = stroke, revert = revert)
}

# Function 3: Cleaning data in wide format, including assigning wave numbers and filling in missing waves

clean_wide <- function() { #in development, check dictionary
  require(data.table)
  require(lubridate)
  require(forcats)
  require(purrr)

  cols_to_select <- c(
    "entvilla",
    "codigo",
    "codigogen",
    "codigovilla",
    "codigovivienda",
    "codigofam",
    "codigopersona",
    "fecha",
    "time",
    "intervencion",
    "sexo",
    "fec_nac",
    "edad1",
    "ecivil",
    "ecivil1",
    "dbdx",
    "db2",
    "bmi",
    "phq",
    "phq1",
    "phq2",
    "sbp",
    "dbp",
    "preht",
    "hts",
    "htd",
    "htdx",
    "htdxtx",
    "ht5",
    "derrame",
    "infarto",
    "insuficiencia",
    "otracor",
    "colesterol",
    "smoking1",
    "smoking2",
    "pvivo",
    "dni",
    "f_muerte",
    "c_muerte",
    "assets",
    "xassets",
    "niveduca"
  )

  tidied_data <- import_data("wide")
  tidied_data <- tidied_data[, ..cols_to_select]
  # transforming date variables into date format
  tidied_data[, c("fecha", "fec_nac") := lapply(.SD, dmy),
              .SDcols = c("fecha", "fec_nac")]
  # f_muerte is stored as character in Stata; empty string encodes missing
  tidied_data[f_muerte == "", f_muerte := NA_character_]
  tidied_data[, f_muerte := dmy(f_muerte)]
  # one English vocabulary for every factor level (see english_labels above)
  english_labels(tidied_data)
  # collapsing superior education categories
  tidied_data[, niveduca2 := fct_collapse(
    niveduca,
    Tertiary = c("Non-university tertiary", "University")
  )]
  return(tidied_data)
}
