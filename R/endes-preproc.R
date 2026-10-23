#-----------------------------------------------------------------
# Cleaning ENDES adults and households before synthetic creation
# Author: Darwin Del Castillo
#-----------------------------------------------------------------

print("Importing functions to clean ENDES before synthetic data generation")

# Cleaning CSALUD01's selected adult together with their roster line and household
#
# The synthetic population is built for the whole of Peru and filtered to Tumbes
# afterwards, so every variable available for an adult and their household stays in.

# `data` is building_endes()'s compiled table. Returns one row per eligible adult, with
# continuous variables as double and everything else as factor, and writes it to
# data/endes-clean/adults-clean.parquet.

cleaning_endes <- function(data = here::here("data", "endes-proc", "compiled.parquet") |>
                             arrow::read_parquet()) {
  library(data.table)
  library(purrr)
  library(arrow)

  endes_proc  <- here::here("data", "endes-proc")
  endes_clean <- here::here("data", "endes-clean")

  # --- Specification, from the dictionaries ---

  # Survey process, identifiers, line numbers, consents and duplicates of the adult's
  # own CSALUD01 answers. UBIGEO already holds the district, so the province and district
  # codes and everything below the district (populated centre, coordinates) go
  drop_cols <- c(
    # CSALUD01. QS212T repeats the QS212A-QS212O flags; QS407 was removed from the questionnaire
    "QHCLUSTER", "QHNUMBER", "QHHOME", "QSNUMERO", "QSINTM", "QSINTY", "QSTOTVISIT",
    "QSRESINF", "QS20C", "QSMEF", "QSDIA", "QS22M", "QS22A", "QS212T", "QS407", "QS704T",
    "QFIN", "QS8INICIO",
    # RECH0
    "HV000", "HV002", "HV002A", "HV003", "HV004", "HV007", "HV008", "HV017", "HV020",
    "HV021", "HV027", "HV028", "HV030", "HV031", "HV032", "HV033", "HV042", "HV043",
    "HV044", "NCONGLOME1", "CODCCPP", "NOMCCPP", "LONGITUDX", "LATITUDY",
    # RECH1. HV104 and HV105 repeat QSSEXO and QS23; QH25A is free-text nationality
    "HV104", "HV105", "HV112", "HV114", "HV117", "HV118", "HV120", "QH25A", "QH25CM",
    # RECH5. HA1 repeats QS23; HA4-HA12B are child growth indices derived from HA2/HA3
    "HA1", "HA4", "HA5", "HA6", "HA11", "HA12", "HA12A", "HA12B", "HA32", "HA33", "HA50",
    "HA51", "HA52", "HA58", "HA61", "HA62", "HA63", "HA64", "HA65", "HA69",
    # RECH23
    "HV218", "SHVER", "SHSEMES", "SHPROVIN", "SHDISTRI", "QH227A",
    # Programas Sociales x Hogar. JUNTOS's affiliation date repeats QH96A/QH96M
    "QH96", "QH97D", "QH97M", "QH97A", "QHVIOLEN"
  )

  # Codes that mean "No sabe", "No recuerda", "Rehúsa", "No hay dato" or a test that was
  # not done, named by code. A code is listed per variable because its meaning is not
  # global: 8 is Leña in HV226 and Jubilado in SH13, and QS711 uses 8 and 9 as frequency
  # levels ("A veces", "A menudo")
  na_codes <- list(
    # Programas Sociales x Hogar: 3 = No sabe/No recuerda
    `3` = c("QH91", "QH93", "QH95", "QH99", "QH101", "QH103", "QH106"),
    # RECH4: 7 = education given in grades, the value is in SH*G
    `7` = c("SH15Y", "SH18Y", "SH20Y"),
    `8` = c(
      # CSALUD01 yes/no items coded 1:2, 8
      "QS102", "QS104", "QS105", "QS106", "QS107", "QS109", "QS111", "QS112", "QS113",
      "QS200", "QS201", "QS206", "QS208", "QS209", "QS210", "QS301", "QS304", "QS306",
      "QS307", "QS308", "QS309", "QS311", "QS401", "QS402", "QS403", "QS404", "QS404C",
      "QS406", "QS409", "QS411", "QS413", "QS413A", "QS413C", "QS413D", "QS413F", "QS415",
      "QS418A", "QS503", "QS505A", "QS505B", "QS505C", "QS505D", "QS506", "QS506B",
      "QS603", "QS604", "QS606", "QS607", "QS608", "QS609", "QS610", "QS611", "QS707",
      "QS730",
      # CSALUD01 units of the unit/quantity pairs
      "QS103U", "QS110U", "QS203U", "QS204U", "QS205U", "QS207U", "QS211U", "QS213U",
      "QS214U", "QS215U", "QS216U", "QS217U", "QS218U", "QS219U", "QS220U", "QS302U",
      "QS305U", "QS312U", "QS410U", "QS412U", "QS413B", "QS413E", "QS416U", "QS419A",
      "QS501U",
      # RECH1
      "HV106", "HV109", "HV111", "HV113", "HV122", "HV126", "HV129",
      # RECH4
      "SH12", "SH15N", "SH15Y", "SH15G", "SH18N", "SH18Y", "SH18G", "SH20N", "SH20Y",
      "SH20G",
      # RECH5
      "HA66", "HA68",
      # RECH23
      "HV249", "HV250", "SH51", "SH69", "SH76A", "SH78",
      # PS_QALIWARMA
      "PS109_1R"
    ),
    # 8 = No sabe and 9 = Rehúsa in the alcohol-use items
    `9` = c("QS713", "QS714", "QS715", "QS716", "QS717", "QS719", "QS720", "QS721",
            "QS722", "QS723", "QS724", "QS725", "QS726", "QS727", "QS728", "QS729",
            "SH224", "SH227"),
    # Portions and glasses carry one decimal; 9.9 = No hay dato
    `9.9` = c("QS214C", "QS216C", "QS218C", "QS220CV"),
    # 96 = area given in a unit other than hectares
    `96` = "HV245",
    # 97 = Inconsistente, outside every valid range of these four
    `97` = c("HV123", "HV124", "HV127", "HV128"),
    `98` = c(
      "QS25BB", "QS101", "QS108", "QS303", "QS313",
      paste0("QS212", rep(c(LETTERS[1:7], "X"), each = 2), c("V", "B")),
      "HV107", "HV108", "HV123", "HV124", "HV127", "HV128",
      "SH13", "HA67",
      "HV220", "HV238", "HV245", paste0("HV246", LETTERS[1:11]), "HML1", "SH58", "SH225",
      "QH96A", "QH96M",
      "PS92_1A", "PS92_1M", "PS94_1A", "PS94_1M", "PS100_1A", "PS100_1M", "PS100C1_A",
      "PS100C1_M", "PS102_1A", "PS102_1M", "PS104_1A", "PS104_1M", "PS107_1A", "PS107_1M",
      "PS109_1A", "PS109_1M"
    ),
    # 994 = iodine test not done. 995 = no salt in the household is kept as a category
    `994` = "HV234",
    `998` = "HV204",
    `9996` = "SH79",
    `9998` = c("HA40", "HA41", "SH79")
  )

  # Unit/quantity pairs become one continuous variable each, so the synthesis cannot pair
  # a unit with a quantity from another unit. Lower-case names mark the derived columns
  # Time since a diagnosis or test: unit 1 = months (under 2 years), 2 = years
  since_pairs <- list(
    qs103_years  = c("QS103U", "QS103C"),   # hypertension diagnosis
    qs110_years  = c("QS110U", "QS110C"),   # diabetes diagnosis
    qs302_years  = c("QS302U", "QS302C"),   # eye examination
    qs305_years  = c("QS305U", "QS305C"),   # cataract diagnosis
    qs312_years  = c("QS312U", "QS312C"),   # dental care
    qs410_years  = c("QS410U", "QS410C"),   # breast examination
    qs412_years  = c("QS412U", "QS412C"),   # Papanicolaou
    qs413b_years = c("QS413B", "QS413B_A"), # IVAA
    qs413e_years = c("QS413E", "QS413E_A"), # HPV molecular test
    qs416_years  = c("QS416U", "QS416C"),   # mammography
    qs419a_years = c("QS419A", "QS419A_A")  # PSA
  )
  # Days in the last week: unit 1 = days, 3 = did not eat or drink it (0 days)
  day_pairs <- list(
    qs213_days = c("QS213U", "QS213C"), # fruit
    qs215_days = c("QS215U", "QS215C"), # fruit juice
    qs217_days = c("QS217U", "QS217C"), # fruit salad
    qs219_days = c("QS219U", "QS219C")  # vegetable salad
  )
  # Unit 1 is the only valid unit; 8 (No sabe) is already NA
  count_pairs <- list(
    qs203_age        = c("QS203U", "QS203C"), # age started smoking daily
    qs204_years      = c("QS204U", "QS204C"), # years since started smoking daily
    qs205_cigarettes = c("QS205U", "QS205C"), # cigarettes a day
    qs207_age        = c("QS207U", "QS207C"), # age at first alcoholic drink
    qs211_times      = c("QS211U", "QS211C"), # times drank in the last 30 days
    qs214_portions   = c("QS214U", "QS214C"), # slices or bunches of fruit
    qs216_glasses    = c("QS216U", "QS216C"), # glasses of fruit juice
    qs218_portions   = c("QS218U", "QS218C")  # portions of fruit salad
  )
  # Time as a beneficiary, in years + months
  benefit_pairs <- list(
    qh96_years    = c("QH96A", "QH96M"),         # JUNTOS (household)
    ps92_years    = c("PS92_1A", "PS92_1M"),     # BECA 18
    ps94_years    = c("PS94_1A", "PS94_1M"),     # Trabaja Perú
    ps100_years   = c("PS100_1A", "PS100_1M"),   # Pensión 65
    ps100c1_years = c("PS100C1_A", "PS100C1_M"), # Cuna Más
    ps102_years   = c("PS102_1A", "PS102_1M"),   # Vaso de Leche
    ps104_years   = c("PS104_1A", "PS104_1M"),   # Comedor popular
    ps107_years   = c("PS107_1A", "PS107_1M"),   # Wawa Wasi
    ps109_years   = c("PS109_1A", "PS109_1M")    # Qali Warma
  )
  # QS220's unit says which of the two quantities holds the answer
  qs220_cols <- c("QS220U", "QS220CV", "QS220CC")

  # Member-level social programme modules. A row exists only for a beneficiary, so after
  # building_endes()'s left join every column of the module is NA for anyone else
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
  # PS1-PS8 repeat the member's own roster line
  ps_order <- paste0("PS", 1:8)

  # Implied decimals, from the dictionary ranges. Weights follow synth-proc.R
  scale_by <- list(
    `10`  = c("HA2", "HA3", "HA53", "HA56", "HA56A", "SH79"),
    `100` = c("HA40", "HA41"),
    `1e6` = c("Peso15_AMAS", "HV005")
  )

  # Measurements, counts, ages and durations. Every other column becomes a factor
  cont_cols <- c(
    # CSALUD01
    "QS23", "QSNINOS",
    paste0("QS212", rep(c(LETTERS[1:7], "X"), each = 2), c("V", "B")),
    "QS900", "QS901", "QS903S", "QS903D", "QS905S", "QS905D", "QS907",
    # RECH0
    "HV009", "HV010", "HV011", "HV012", "HV013", "HV014", "HV035", "HV040", "HV041",
    # RECH1
    "HV108", "HV124", "HV128", "QH25CA",
    # RECH5
    "HA2", "HA3", "HA35", "HA40", "HA41", "HA53", "HA56", "HA56A",
    # RECH23
    "HV204", "HV216", "HV220", "HV238", "HV245", paste0("HV246", LETTERS[1:11]), "HV248",
    "HV250", "HV251", "HML1", "HML1A", "HML2", "SHTOTH", "SH71", "SH72", "SH79", "HV271",
    # Derived below
    names(since_pairs), "qs501_months", names(day_pairs), names(count_pairs),
    "qs220_portions", "qs220_spoons", names(benefit_pairs)
  )

  # Sample design: PSU, stratum and the person and household weights. Kept numeric and
  # first, for generate_synth() to decide how to use them
  design_cols <- c("HV001", "HV022", "Peso15_AMAS", "HV005")

  # --- Scope: adult + household, without the MEF modules ---

  # MEF columns are what women.parquet holds beyond persons.parquet. Schemas only
  mef_cols <- file.path(endes_proc, "women.parquet") |>
    open_dataset() |>
    names() |>
    setdiff(file.path(endes_proc, "persons.parquet") |> open_dataset() |> names())

  adults <- as.data.table(data)
  adults <- adults[, setdiff(names(adults), mef_cols), with = FALSE]

  # A misspelt variable in the specification would otherwise be skipped silently
  derived <- c(names(since_pairs), "qs501_months", names(day_pairs), names(count_pairs),
               "qs220_portions", "qs220_spoons", names(benefit_pairs))
  spec_cols <- c(drop_cols, unlist(na_codes), setdiff(cont_cols, derived), design_cols,
                 unlist(c(since_pairs, day_pairs, count_pairs, benefit_pairs, ps_members)),
                 qs220_cols, "QS501U", "QS501C", ps_order, "QS27", "QS902", "HHID",
                 "QSRESULT", "QS23", "HV102") |>
    unique()
  missing_cols <- setdiff(spec_cols, names(adults))
  if (length(missing_cols) > 0) {
    stop("Dictionary variables missing from the data: ", paste(missing_cols, collapse = ", "))
  }

  stopifnot("A variable is both dropped and kept" =
              length(intersect(drop_cols, c(unlist(na_codes), cont_cols, design_cols))) == 0,
            "A variable is both continuous and a design column" =
              length(intersect(cont_cols, design_cols)) == 0)

  # --- Eligibility: SALT enrolled usual residents aged 18+ (crosswalk §3d) ---

  # Same filter as checking_endes(): complete health questionnaire, aged 18+, usual resident
  adults <- adults[QSRESULT == 1L & QS23 >= 18L & HV102 == 1L]
  stopifnot("More than one selected adult per household" = anyDuplicated(adults$HHID) == 0)

  # --- Structure, read before any code becomes NA ---

  # Programme flags first: a beneficiary who answered 98 to every duration would
  # otherwise look like a non-beneficiary once 98 is NA
  iwalk(ps_members, \(cols, flag) {
    set(adults, j = flag, value = as.integer(reduce(map(cols, \(v) !is.na(adults[[v]])), `|`)))
  })

  # QS27 stores every insurer named as letters A:E, X in one string. Not asked when the
  # adult has no insurance (QS26 == 2), so a blank string stays NA in every dummy
  qs27 <- trimws(adults$QS27)
  qs27[qs27 == ""] <- NA_character_
  walk(c(LETTERS[1:5], "X"), \(l) {
    set(adults, j = paste0("QS27_", l),
        value = fifelse(is.na(qs27), NA_integer_, as.integer(grepl(l, qs27, fixed = TRUE))))
  })

  # HV204 996 = water on the premises, so no time is spent collecting it
  print(adults[, .(HV204_on_premises = sum(HV204 == 996L, na.rm = TRUE))])
  adults[HV204 == 996L, HV204 := 0L]

  adults[, c(drop_cols, ps_order, "QS27", "HHID") := NULL]

  # --- Special codes -> NA: diagnose, then recode ---

  na_spec <- imap(na_codes, \(cols, code) data.table(var = cols, code = as.numeric(code))) |>
    rbindlist()
  print(na_spec[, .(n_to_na = sum(adults[[var]] %in% code)), by = var][n_to_na > 0],
        nrows = Inf)
  walk(unique(na_spec$var), \(v) {
    set(adults, i = which(adults[[v]] %in% na_spec[var == v, code]), j = v, value = NA)
  })

  # --- Unit/quantity pairs -> one continuous variable ---

  iwalk(since_pairs, \(uv, new) {
    u <- adults[[uv[1]]]
    q <- as.numeric(adults[[uv[2]]])
    set(adults, j = new, value = fcase(u == 1L, q / 12, u == 2L, q))
  })
  # Cough with phlegm: unit 1 = days (under a month), 2 = months
  adults[, qs501_months := fcase(QS501U == 1L, QS501C / (365.25 / 12),
                                 QS501U == 2L, as.numeric(QS501C))]
  iwalk(day_pairs, \(uv, new) {
    u <- adults[[uv[1]]]
    q <- as.numeric(adults[[uv[2]]])
    set(adults, j = new, value = fcase(u == 1L, q, u == 3L, 0))
  })
  iwalk(count_pairs, \(uv, new) {
    set(adults, j = new,
        value = fifelse(adults[[uv[1]]] == 1L, as.numeric(adults[[uv[2]]]), NA_real_))
  })
  adults[, `:=`(qs220_portions = fifelse(QS220U == 1L, as.numeric(QS220CV), NA_real_),
                qs220_spoons   = fifelse(QS220U == 2L, as.numeric(QS220CC), NA_real_))]

  # A duration needs both years and months. How many rows hold only one of the two
  # decides whether a missing component should count as 0
  print(imap(benefit_pairs, \(am, new) {
    data.table(var = new, n_one_component = sum(xor(is.na(adults[[am[1]]]),
                                                    is.na(adults[[am[2]]]))))
  }) |> rbindlist())
  iwalk(benefit_pairs, \(am, new) {
    set(adults, j = new, value = adults[[am[1]]] + adults[[am[2]]] / 12)
  })

  pair_cols <- c(since_pairs, day_pairs, count_pairs, benefit_pairs) |> unlist()
  adults[, c(pair_cols, "QS501U", "QS501C", qs220_cols) := NULL]

  # --- Implied decimals ---

  iwalk(scale_by, \(cols, k) {
    walk(cols, \(v) set(adults, j = v, value = adults[[v]] / as.numeric(k)))
  })

  # Women 12-49 weighed in the household round have QS902 == 4 and their weight and
  # height in RECH5. A no-op if INEI already copied them into QS900/QS901
  adults[QS902 == 4L, `:=`(QS900 = fcoalesce(QS900, HA2), QS901 = fcoalesce(QS901, HA3))]

  # --- Types for synthpop ---

  # CART treats an integer code as ordered, so every coded variable becomes a factor.
  # Levels are the dictionary codes; their labels stay in the dictionaries
  walk(c(design_cols, cont_cols), \(v) set(adults, j = v, value = as.numeric(adults[[v]])))
  walk(setdiff(names(adults), c(design_cols, cont_cols)), \(v) {
    set(adults, j = v, value = factor(adults[[v]]))
  })

  # Columns with a single value, NA included, carry nothing to synthesise: QSRESULT and
  # HV102 after the filter, ID1 while there is one survey year
  constant_cols <- names(adults)[map_lgl(adults, \(x) uniqueN(x) == 1L)]
  message("Constant or empty columns dropped: ", paste(constant_cols, collapse = ", "))
  adults[, (constant_cols) := NULL]

  setcolorder(adults, intersect(design_cols, names(adults)))
  stopifnot("Columns left neither factor nor double" =
              all(map_lgl(adults, \(x) is.factor(x) || is.double(x))))

  if (!dir.exists(endes_clean)) {
    message("Creating output folder for clean ENDES data: ", endes_clean)
    dir.create(endes_clean, recursive = TRUE)
  }
  write_parquet(adults, sink = file.path(endes_clean, "adults-clean.parquet"))

  invisible(adults)
}
