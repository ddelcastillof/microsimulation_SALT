#-----------------------------------------------------------------
# Cleaning ENDES adults and households before synthetic creation
# Author: Darwin Del Castillo
#-----------------------------------------------------------------

print("Importing functions to clean ENDES before synthetic data generation")

# CSALUD01's selected adult + roster line + household, for all of Peru (Tumbes is filtered
# after synthesis). Returns one row per eligible adults. Writes data/endes-clean/adults-clean.parquet

cleaning_endes <- function(data = here::here("data", "endes-proc", "compiled.parquet") |>
                             arrow::read_parquet()) {
  require(data.table)
  require(purrr)
  require(arrow)

  endes_proc  <- here::here("data", "endes-proc")
  endes_clean <- here::here("data", "endes-clean")

  # --- Specification, from the INEI dictionaries ---

  # Survey process, IDs, line numbers, consents, repeated columns and geography below the
  # district (HV024, prov_key and SHDISTRI keep the district)
  drop_cols <- c(
    # Survey year: it counts the pooled years, then goes, since they are one population
    "ID1",
    # CSALUD01. QS212T repeats the QS212A-QS212O flags; QS407 left the questionnaire.
    # QS212Z, QS708_Y and QS731_Y are "No sabe" options, folded into dk_lists
    "QHCLUSTER", "QHNUMBER", "QHHOME", "QSNUMERO", "QSINTM", "QSINTY", "QSTOTVISIT",
    "QSRESINF", "QS20C", "QSMEF", "QSDIA", "QS22M", "QS22A", "QS212T", "QS407", "QS704T",
    "QFIN", "QS8INICIO", "QS212Z", "QS708_Y", "QS731_Y",
    # Time since the hypertension, diabetes and cataract diagnoses; years smoking daily.
    # The user checked raw CSALUD01_2025.csv (2026-10-23): empty by construction.
    # Re-check when pooling years
    "QS103U", "QS103C", "QS110U", "QS110C", "QS204U", "QS204C", "QS305U", "QS305C",
    # synthpop's collinearity check would copy these from an earlier column (sparse columns
    # meet on a few rows; QS731_F/I/J follow QS731_C), and the microsimulation uses none:
    # glasses and bottles of three drinks, three QS731 places, and the pairs behind age
    # started and cigarettes a day, and time since dental care, IVAA, HPV and PSA tests
    "QS212BB", "QS212CB", "QS212FV", "QS212FB", "QS731_F", "QS731_I", "QS731_J",
    "QS203U", "QS203C", "QS205U", "QS205C", "QS312U", "QS312C", "QS413B", "QS413B_A",
    "QS413E", "QS413E_A", "QS419A", "QS419A_A",
    # RECH0. HV023 repeats HV024 (the user checked: identical). UBIGEO is HV024 + SHPROVIN +
    # SHDISTRI, which building_endes() checks. PSU (HV001) and stratum (HV022) are sampling
    # codes, not traits of a person, so they are not synthesised
    "HV001", "HV022",
    "HV000", "HV002", "HV002A", "HV003", "HV004", "HV007", "HV008", "HV017", "HV020",
    "HV021", "HV023", "HV027", "HV028", "HV030", "HV031", "HV032", "HV033", "HV042",
    "HV043", "HV044", "NCONGLOME1", "CODCCPP", "NOMCCPP", "LONGITUDX", "LATITUDY", "UBIGEO",
    # RECH1. HV104/HV105 repeat QSSEXO/QS23 and HV121 follows HV110; QH25A is free text;
    # QH25CA (year of arrival in Peru) only fed a duration that collinearity would copy
    "HV104", "HV105", "HV112", "HV114", "HV117", "HV118", "HV120", "HV121", "QH25A",
    "QH25CM", "QH25CA",
    # RECH4. SH11Y is the "No sabe" option of SH11*, folded into dk_lists. SH15N/SH15G
    # follow QS25N/QS25G and SH18G follows QH21A (collinear)
    "SH11Y", "SH15N", "SH15G", "SH18G",
    # RECH5. HA1 repeats QS23 and HA68 follows HA66; HA4-HA12B are child growth indices
    "HA1", "HA4", "HA5", "HA6", "HA11", "HA12", "HA12A", "HA12B", "HA32", "HA33", "HA50",
    "HA51", "HA52", "HA58", "HA61", "HA62", "HA63", "HA64", "HA65", "HA68", "HA69",
    # RECH23. Province codes repeat across departments, so SHPROVIN becomes prov_key
    # (department + province), the group that SHDISTRI is synthesised within. SH72, SH224
    # and SH78 are collinear with HV216, HV234 and HV244
    "HV218", "SHVER", "SHSEMES", "QH227A", "SHPROVIN", "SH72", "SH224", "SH78",
    # Programas Sociales x Hogar. QH97D/M/A (JUNTOS affiliation date) repeat QH96A/QH96M;
    # QH100B has no dictionary entry
    "QH96", "QH97D", "QH97M", "QH97A", "QHVIOLEN", "QH100B",
    # Durations collinearity would copy (JUNTOS, BECA 18, Trabaja Perú, Cuna Más, Comedor);
    # PS109_1R/PS109_1S follow QH21A. All go after the membership flags are built from them
    "QH96A", "QH96M", "PS92_1A", "PS92_1M", "PS94_1A", "PS94_1M", "PS100C1_A", "PS100C1_M",
    "PS104_1A", "PS104_1M", "PS109_1R", "PS109_1S"
  )

  # Glasses (V) and bottles (B) drunk, per drink type, less the four in drop_cols
  qs212_cols <- setdiff(paste0("QS212", rep(c(LETTERS[1:7], "X"), each = 2), c("V", "B")),
                        c("QS212BB", "QS212CB", "QS212FV", "QS212FB"))
  # AUDIT-type alcohol items (there is no QS718)
  qs71x_cols <- paste0("QS7", c(13:17, 19:29))

  # "No sabe", "No recuerda", "Rehúsa", "No hay dato" or test not done, by code. Listed per
  # variable because a code's meaning is not global: 8 is Leña in HV226, Jubilado in SH13
  # and "A veces" in QS711
  na_codes <- list(
    `3` = c("QH91", "QH93", "QH95", "QH99", "QH101", "QH103", "QH106"), # No sabe/No recuerda
    `7` = c("SH15Y", "SH18Y", "SH20Y"), # given in grades: SH20G, QS25G (for SH15G), QH21A (SH18G)
    `8` = c(
      # CSALUD01 yes/no items (1:2, 8) and the alcohol items (1:2, 8, 9)
      "QS102", "QS104", "QS105", "QS106", "QS107", "QS109", "QS111", "QS112", "QS113",
      "QS200", "QS201", "QS206", "QS208", "QS209", "QS210", "QS301", "QS304", "QS306",
      "QS307", "QS308", "QS309", "QS311", "QS401", "QS402", "QS403", "QS404", "QS404C",
      "QS406", "QS409", "QS411", "QS413", "QS413A", "QS413C", "QS413D", "QS413F", "QS415",
      "QS418A", "QS503", "QS505A", "QS505B", "QS505C", "QS505D", "QS506", "QS506B",
      "QS603", "QS604", "QS606", "QS607", "QS608", "QS609", "QS610", "QS611", "QS707",
      "QS730", qs71x_cols,
      # CSALUD01 pair units
      "QS207U", "QS211U", "QS213U", "QS214U", "QS215U", "QS216U", "QS217U", "QS218U",
      "QS219U", "QS220U", "QS302U", "QS410U", "QS412U", "QS416U", "QS501U",
      # RECH1
      "HV106", "HV109", "HV111", "HV113", "HV122", "HV126", "HV129",
      # RECH4
      "SH12", "SH15Y", "SH18N", "SH18Y", "SH20N", "SH20Y", "SH20G",
      # RECH5
      "HA66",
      # RECH23
      "HV249", "HV250", "SH51", "SH69", "SH76A"
    ),
    # Rehúsa in the alcohol items; test not done in SH227
    `9` = c(qs71x_cols, "SH227"),
    `9.9` = c("QS214C", "QS216C", "QS218C", "QS220CV"), # No hay dato (one decimal)
    `96`  = "HV245",                                     # area not in hectares
    `97`  = c("HV123", "HV124", "HV127", "HV128"),       # Inconsistente
    `98`  = c(
      "QS25BB", "QS101", "QS108", "QS303", "QS313", qs212_cols,
      "HV107", "HV108", "HV123", "HV124", "HV127", "HV128",
      "SH13", "HA67",
      "HV220", "HV238", "HV245", paste0("HV246", LETTERS[1:11]), "HML1", "SH58", "SH225",
      "PS100_1A", "PS100_1M", "PS102_1A", "PS102_1M", "PS107_1A", "PS107_1M", "PS109_1A",
      "PS109_1M"
    ),
    `994`  = "HV234", # test not done; 995 (no salt) stays a category
    `998`  = "HV204",
    `9996` = "SH79",  # area in other units
    `9998` = c("HA40", "HA41", "SH79")
  )

  # Check-all lists with a "No sabe" option, named by that option: once it is ticked, the
  # other options' No is unknown. QS212's options are read from letter Y of QS212T, as the
  # dictionary labels QS212Z inverted (0 = No sabe)
  places      <- c("A", "AA", "AB", "B", "C", "D", "E", "EA", "F", "G", "H", "I", "J", "X")
  dk_lists    <- list(QS708_Y = paste0("QS708_", places),
                      QS731_Y = paste0("QS731_", setdiff(places, c("F", "I", "J"))), # see drop_cols
                      SH11Y = paste0("SH11", c(LETTERS[1:5], "Z")))
  qs212_flags <- paste0("QS212", c(LETTERS[1:7], "O")) # 1 Sí, 2 No

  # Each unit/quantity pair becomes one lower-case continuous column, so synthesis cannot
  # pair a unit with another unit's quantity.
  # Time since a diagnosis or test: unit 1 = months, 2 = years.
  # QS103, QS110, QS305: the user found them empty in raw. QS312, QS413B, QS413E and QS419A
  # would be collinear (see drop_cols for both)
  since_pairs <- list(
    qs302_years  = c("QS302U", "QS302C"),   # eye examination
    qs410_years  = c("QS410U", "QS410C"),   # breast examination
    qs412_years  = c("QS412U", "QS412C"),   # Papanicolaou
    qs416_years  = c("QS416U", "QS416C")    # mammography
  )
  # Cough with phlegm: unit 1 = days, 2 = months
  qs501_pair <- list(qs501_months = c("QS501U", "QS501C"))
  # Days in the last week: unit 1 = days, 3 = none (0 days)
  day_pairs <- list(
    qs213_days = c("QS213U", "QS213C"), # fruit
    qs215_days = c("QS215U", "QS215C"), # fruit juice
    qs217_days = c("QS217U", "QS217C"), # fruit salad
    qs219_days = c("QS219U", "QS219C")  # vegetable salad
  )
  # Unit 1 is the only valid one. QS204: the user found it empty in raw. QS203 (age started
  # smoking daily) and QS205 (cigarettes a day) would be collinear (see drop_cols for both)
  count_pairs <- list(
    qs207_age        = c("QS207U", "QS207C"), # age at first alcoholic drink
    qs211_times      = c("QS211U", "QS211C"), # times drank in the last 30 days
    qs214_portions   = c("QS214U", "QS214C"), # slices or bunches of fruit
    qs216_glasses    = c("QS216U", "QS216C"), # glasses of fruit juice
    qs218_portions   = c("QS218U", "QS218C")  # portions of fruit salad
  )
  # Years + months as a beneficiary. JUNTOS, BECA 18, Trabaja Perú, Cuna Más and Comedor
  # would be collinear (see drop_cols)
  benefit_pairs <- list(
    ps100_years   = c("PS100_1A", "PS100_1M"),   # Pensión 65
    ps102_years   = c("PS102_1A", "PS102_1M"),   # Vaso de Leche
    ps107_years   = c("PS107_1A", "PS107_1M"),   # Wawa Wasi
    ps109_years   = c("PS109_1A", "PS109_1M")    # Qali Warma
  )
  all_pairs <- c(since_pairs, qs501_pair, day_pairs, count_pairs, benefit_pairs)
  # QS220's unit says which quantity holds the answer
  qs220_cols <- c("QS220U", "QS220CV", "QS220CC")
  derived    <- c(names(all_pairs), "qs220_portions", "qs220_spoons")

  # Member-level programme modules: a row exists only for a beneficiary (PS_QALIWARMA is
  # the exception, see below), so the left join leaves every module column NA for anyone else
  ps_members <- list(
    beca18     = c("PS92_1A", "PS92_1M", "PS92_1S", "PS1"),
    trabaja    = c("PS94_1A", "PS94_1M", "PS94_1S", "PS2"),
    pension65  = c("PS100_1A", "PS100_1M", "PS100_1S", "PS3"),
    vaso_leche = c("PS102_1A", "PS102_1M", "PS102_1S", "PS4"),
    comedor    = c("PS104_1A", "PS104_1M", "PS104_1S", "PS5"),
    wawa_wasi  = c("PS107_1A", "PS107_1M", "PS107_1S", "PS6"),
    qaliwarma  = c("PS109_1R", "PS109_1A", "PS109_1M", "PS109_1S", "PS7"),
    cuna_mas   = c("PS100C1_A", "PS100C1_M", "PS100C_1S", "PS8")
  )
  ps_order <- paste0("PS", 1:8) # repeat the member's roster line

  # Person and household weights. synthpop ignores weights, so they go last and are
  # synthesised from everything else
  weights <- c("Peso15_AMAS", "HV005")

  # Implied decimals, from the dictionary ranges. Weights as in checking_endes()
  scale_by <- list(
    `10`  = c("HA2", "HA3", "HA53", "HA56", "HA56A", "SH79"),
    `100` = c("HA40", "HA41"),
    `1e6` = c("Peso15_AMAS", "HV005")
  )

  # Measurements, counts, ages and durations. Every other column becomes a factor.
  # Top codes stay as values: QS23 and HV220 97 = 97 or more, HV238 95 = 10 or more
  # households, HV245 and HV246* 95 = more than 95
  cont_cols <- c(
    # CSALUD01
    "QS23", "QSNINOS", qs212_cols, "QS900", "QS901", "QS903S", "QS903D", "QS905S", "QS905D",
    "QS907",
    # RECH0
    "HV009", "HV010", "HV011", "HV012", "HV013", "HV014", "HV035", "HV040", "HV041",
    # RECH1
    "HV108", "HV124", "HV128",
    # RECH5
    "HA2", "HA3", "HA35", "HA40", "HA41", "HA53", "HA56", "HA56A",
    # RECH23
    "HV204", "HV216", "HV220", "HV238", "HV245", paste0("HV246", LETTERS[1:11]), "HV248",
    "HV250", "HV251", "HML1", "HML1A", "HML2", "SHTOTH", "SH71", "SH79", "HV271",
    derived
  )

  # Plausible ranges for BP, weight and height (NCD-RisC) and waist (its dictionary range).
  # Diagnosed only: a sentinel the dictionary does not list would show up here
  limits <- list(QS903S = c(70, 270), QS905S = c(70, 270), QS903D = c(30, 150),
                 QS905D = c(30, 150), QS900 = c(12, 300), QS901 = c(100, 250),
                 QS907 = c(55, 170))

  # --- Scope: adult + household, no MEF modules ---

  # MEF columns: what women.parquet holds beyond persons.parquet. Schemas only, so `data`
  # must come from the same building_endes() run
  mef_cols <- file.path(endes_proc, "women.parquet") |> open_dataset() |> names() |>
    setdiff(file.path(endes_proc, "persons.parquet") |> open_dataset() |> names())
  adults <- as.data.table(data)[, setdiff(names(data), mef_cols), with = FALSE]

  # A misspelt dictionary name would otherwise be skipped silently
  missing_cols <- c(drop_cols, unlist(na_codes), cont_cols, weights, unlist(all_pairs),
                    unlist(ps_members), qs220_cols, unlist(dk_lists), qs212_flags, "QS27",
                    "QS902", "HA13", "HHID", "QSRESULT", "HV102", "HV024", "SHDISTRI") |>
    setdiff(c(derived, names(adults)))
  if (length(missing_cols) > 0) {
    stop("Dictionary variables missing from the data: ", toString(missing_cols))
  }
  stopifnot("A variable is both dropped and used after the drop" =
              !any(drop_cols %in% c(unlist(na_codes), cont_cols, weights, unlist(all_pairs),
                                     qs220_cols, unlist(scale_by), unlist(dk_lists),
                                     qs212_flags, "QS902", "HA13", "HV024", "SHDISTRI")),
            "A variable is both continuous and a weight" = !any(cont_cols %in% weights))

  # --- Eligibility, as in checking_endes(): complete interview, 18+, usual resident ---

  adults <- adults[QSRESULT == 1L & QS23 >= 18L & HV102 == 1L]
  stopifnot("More than one selected adult per household and year" =
              anyDuplicated(adults, by = c("ID1", "HHID")) == 0)
  n_years <- uniqueN(adults$ID1)

  # --- Structure, read before codes become NA ---

  # Flags first: a beneficiary who answered 98 to every duration would look like a
  # non-beneficiary once 98 is NA
  iwalk(ps_members, \(cols, flag) {
    set(adults, j = flag, value = as.integer(reduce(map(cols, \(v) !is.na(adults[[v]])), `|`)))
  })
  # PS_QALIWARMA also lists members who did not get the meal, so its flag reads PS109_1R
  # (1 Sí, 2 No, 8 No sabe -> NA)
  adults[, qaliwarma := fcase(qaliwarma == 0L, 0L, PS109_1R == 1L, 1L, PS109_1R == 2L, 0L)]

  # QS27 holds every insurer as letters A:E, X. Not asked without insurance (QS26 == 2),
  # so a blank stays NA in every dummy
  qs27 <- trimws(adults$QS27)
  qs27[qs27 == ""] <- NA_character_
  walk(c(LETTERS[1:5], "X"), \(l) {
    set(adults, j = paste0("QS27_", l),
        value = fifelse(is.na(qs27), NA_integer_, as.integer(grepl(l, qs27, fixed = TRUE))))
  })

  # Rows with "No sabe" ticked: their other options' No (0, or 2 in QS212) becomes NA
  qs212_dk <- grepl("Y", adults$QS212T, fixed = TRUE)
  print(c(imap_int(dk_lists, \(cols, dk) sum(adults[[dk]] == 1L, na.rm = TRUE)),
          QS212T_Y = sum(qs212_dk)))
  iwalk(dk_lists, \(cols, dk) walk(cols, \(v) {
    set(adults, i = which(adults[[dk]] == 1L & adults[[v]] == 0L), j = v, value = NA)
  }))
  walk(qs212_flags, \(v) set(adults, i = which(qs212_dk & adults[[v]] == 2L), j = v, value = NA))

  # HV204 996 = water on the premises: no time spent collecting it
  print(adults[, .(HV204_on_premises = sum(HV204 == 996L, na.rm = TRUE))])
  adults[HV204 == 996L, HV204 := 0L]

  # Department + province, two digits each, so each province has its own code
  adults[, prov_key := fifelse(is.na(SHPROVIN), NA_character_,
                               sprintf("%02d%02d", HV024, SHPROVIN))]

  adults[, c(drop_cols, ps_order, "QS27", "HHID") := NULL]

  # --- Special codes -> NA: diagnose, then recode ---

  na_spec <- imap(na_codes, \(cols, code) data.table(var = cols, code = as.numeric(code))) |>
    rbindlist()
  print(na_spec[, .(n_to_na = sum(adults[[var]] %in% code)), by = var][n_to_na > 0],
        nrows = Inf)
  walk(unique(na_spec$var), \(v) {
    set(adults, i = which(adults[[v]] %in% na_spec[var == v, code]), j = v, value = NA)
  })

  # --- Unit/quantity pairs -> one continuous column ---

  # f(unit, quantity) gives the new column
  from_pairs <- \(pairs, f) iwalk(pairs, \(uv, new) {
    set(adults, j = new, value = f(adults[[uv[1]]], as.numeric(adults[[uv[2]]])))
  })
  from_pairs(since_pairs, \(u, q) fcase(u == 1L, q / 12, u == 2L, q))
  from_pairs(qs501_pair, \(u, q) fcase(u == 1L, q / (365.25 / 12), u == 2L, q))
  from_pairs(day_pairs, \(u, q) fcase(u == 1L, q, u == 3L, 0))
  from_pairs(count_pairs, \(u, q) fifelse(u == 1L, q, NA_real_))
  # At most one of the two holds a value per row; synthesis does not know that
  adults[, `:=`(qs220_portions = fifelse(QS220U == 1L, as.numeric(QS220CV), NA_real_),
                qs220_spoons   = fifelse(QS220U == 2L, as.numeric(QS220CC), NA_real_))]

  # A duration needs years and months. Rows holding only one decide whether a missing
  # part should count as 0
  print(imap(benefit_pairs, \(am, new) {
    data.table(var = new, n_one_component = sum(xor(is.na(adults[[am[1]]]),
                                                    is.na(adults[[am[2]]]))))
  }) |> rbindlist())
  # Decision: a missing part leaves the duration NA, so none is invented. Switch to
  # fcoalesce(m, 0) if the counts above are large
  from_pairs(benefit_pairs, \(a, m) a + m / 12)

  adults[, c(unlist(all_pairs), qs220_cols) := NULL]

  # --- Implied decimals ---

  iwalk(scale_by, \(cols, k) {
    walk(cols, \(v) set(adults, j = v, value = adults[[v]] / as.numeric(k)))
  })
  # Each year's weights add up to the population, so pooled years share it (crosswalk §2)
  walk(weights, \(v) set(adults, j = v, value = adults[[v]] / n_years))

  # QS902 == 4: weighed in the household round, so weight and height are in RECH5, valid
  # when HA13 is 0 (measured) or 5 (partly measured). A no-op if INEI already copied them
  adults[QS902 == 4L & HA13 %in% c(0L, 5L),
         `:=`(QS900 = fcoalesce(QS900, HA2), QS901 = fcoalesce(QS901, HA3))]

  # Values outside the plausible ranges stay until these counts are read
  print(imap(limits, \(lh, v) {
    data.table(var = v, n_outside = sum(!between(adults[[v]], lh[1], lh[2]), na.rm = TRUE))
  }) |> rbindlist())

  # --- Types for synthpop ---

  # CART reads an integer code as ordered, so coded variables become factors whose levels
  # are the dictionary codes
  walk(c(weights, cont_cols), \(v) set(adults, j = v, value = as.numeric(adults[[v]])))
  walk(setdiff(names(adults), c(weights, cont_cols)), \(v) {
    set(adults, j = v, value = factor(adults[[v]]))
  })
  # Salt brand: for a response with 3+ classes rpart tries every split of a factor predictor,
  # doubling per level (one tree: 10 s at 31 levels, >120 s at 37). The 20 commonest brands
  # stay and the rest join Otros (96)
  adults[, SH225 := forcats::fct_lump_n(SH225, n = 20, other_level = "96", ties.method = "first")]

  # One value, NA included, carries nothing to synthesise: QSRESULT and HV102 after the
  # filter. Pooled years can only fill columns, so the output may gain columns, never lose
  constant_cols <- names(adults)[map_lgl(adults, \(x) uniqueN(x) == 1L)]
  message("Constant or empty columns dropped: ", toString(constant_cols))
  adults[, (constant_cols) := NULL]

  # synthpop visits columns left to right, so the weights go last and are modelled from
  # everything else. generate_synth() leaves prov_key and SHDISTRI out of syn() and draws
  # them afterwards, weighted, within department, area and natural region
  last <- intersect(c(weights, "prov_key", "SHDISTRI"), names(adults))
  setcolorder(adults, c(setdiff(names(adults), last), last))
  stopifnot("Columns left neither factor nor double" =
              all(map_lgl(adults, \(x) is.factor(x) || is.double(x))))

  if (!dir.exists(endes_clean)) {
    message("Creating output folder for clean ENDES data: ", endes_clean)
    dir.create(endes_clean, recursive = TRUE)
  }
  write_parquet(adults, sink = file.path(endes_clean, "adults-clean.parquet"))

  invisible(adults)
}
