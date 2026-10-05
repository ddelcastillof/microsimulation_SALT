#-----------------------------------------------------------------
# Cleaning and preprocessing ENDES data before synthetic creation
# Author: Darwin Del Castillo
#-----------------------------------------------------------------

print("Importing functions to build ENDES datasets...")

# Compiling all ENDES files

building_endes <- function() {
  library(data.table)
  library(purrr)

  endes_raw <- here::here("data", "endes-raw")
  endes_proc <- here::here("data", "endes-proc")

  if(!dir.exists(endes_raw)) {
    message("Creating output folder for processed ENDES data: ", endes_proc)
    dir.create(endes_proc, recursive = TRUE)
  }

  if(!dir.exists(endes_proc)) {
    message("Creating output folder for processed ENDES data: ", endes_proc)
    dir.create(endes_proc, recursive = TRUE)
  }

  # Are there files in the ENDES raw directory?
  check_endes_files <- list.files(endes_raw, pattern = "\\.csv$", ignore.case = TRUE)
  
  if(length(check_endes_files) == 0) {
    stop("No ENDES CSV files found in: ", endes_raw, "\n",
         "  Add the ENDES 2025 module CSV files downloaded from the INEI ",
         "microdata portal, so you can create the synthetic population.")
  }

  message("Folders and files ready. Compiling original ENDES dataset")

  # Importing ENDES tables. IDs (HHID) should be a character
  required_datasets <- c("RECH0", "RECH1", "CSALUD01", "RECH5", "RECH23")
  walk(required_datasets, \(m) {
    assign(m, fread(file.path(endes_raw, paste0(m, "_2025.csv")))[, HHID := as.character(HHID)],
           envir = globalenv())
  })

  # Merging datasets
  households <- RECH0[RECH23[, !"ID1"], on = "HHID"]
  persons <- households[RECH1[, !"ID1"], on = "HHID"]

  # CSALUD01 uses QSNUMERO. Check it against QS20C before joining
  stopifnot("QSNUMERO disagrees with QS20C, the selected person's code" =
              CSALUD01[!is.na(QS20C), all(QSNUMERO == QS20C)])

  # Selected adult + their roster line and household.
  adults <- merge(CSALUD01, persons[, !"ID1"], by.x = c("HHID", "QSNUMERO"),
                  by.y = c("HHID", "HVIDX"), all.x = TRUE)

  # Anaemia and anthropometry
  full <- merge(adults, RECH5[, !"ID1"], by.x = c("HHID", "QSNUMERO"),
                  by.y = c("HHID", "HA0"), all.x = TRUE)

  # Columns of interest
  endes_cols <- list(
    CSALUD01 = c("HHID", "QSNUMERO", "QS20C", "QHCLUSTER", "QSINTM", "QSINTY",
                 "QSRESULT", "QSSEXO", "QS23", "QS25N", "QS25AG", "QS25A", "QS25G",
                 "QS102", "QS103U", "QS103C", "QS104", "QS109", "QS111",
                 "QS200", "QS201", "QS202", "QS900", "QS901", "QS902",
                 "QS903S", "QS903D", "QS905S", "QS905D", "QS906",
                 "Peso15_AMAS"),
    RECH0    = c("HV001", "HV002A", "HV005", "HV009", "HV012", "HV013",
                 "HV022", "HV024", "HV025", "HV026", "HV040", "UBIGEO", "CODCCPP"),
    RECH1    = c("HV102", "HV104", "HV105", "HV106", "HV108"),
    RECH5    = c("HA2", "HA3", "HA13", "HA54"),
    RECH23   = c("SHREGION", "SHTOTH", "HV216", "HV270", "HV271",
                 "HV201", "HV205", "HV207", "HV208", "HV209", "HV210", "HV211",
                 "HV212", "HV213", "HV221", "HV226", "HV243A",
                 "SH61J", "SH61K", "SH61L", "SH61N", "SH61O", "SH61P", "SH61Q",
                 "SH225", "HV234")
  )

  final <- full[, list_c(endes_cols), with = FALSE]

  # Saving file before filtering for non-respondents
  nanoparquet::write_parquet(final, file = file.path(endes_proc, "compiled.parquet"))
}


print("Importing functions to check ENDES datasets...")

# Checking current state of the file
# Will guide synthetic data generation statistics to compare with original

checking_endes <- function() {
  library(data.table)
  library(purrr)

  raw <- nanoparquet::read_parquet(here::here("data", "endes-proc", "compiled.parquet")) |>
    as.data.table()

  # Blue/orange pass the dataviz palette checks; grey de-emphasises
  blue <- "#2a78d6"
  orange <- "#eb6834"
  grey <- "grey65"
  fmt <- \(n) formatC(n, big.mark = ",", format = "d")

  # Each figure sets its own layout; restore the caller's settings on exit
  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  # Each figure is recorded; file devices (png, pdf) need the display list switched on
  dev.control(displaylist = "enable")
  plots <- list()

  # Horizontal bars, first element on top, value printed at each tip
  hbar <- \(x, labels, col = grey, main = "", sub = NULL) {
    mids <- barplot(rev(x), horiz = TRUE, las = 1, col = rev(col), border = NA, space = 1,
                    axes = FALSE, xlim = c(0, 1.3 * max(x)), main = main)
    text(rev(x), mids, rev(labels), pos = 4, cex = 0.85, xpd = TRUE)
    if (!is.null(sub)) mtext(sub, side = 3, line = 0.2, cex = 0.8)
  }

  # 1. Rows per QSRESULT code; labels from the CSALUD01 dictionary
  qsresult <- raw[, .N, by = QSRESULT][order(-N)]
  qsresult[, label := factor(QSRESULT, levels = c(1:6, 9),
                             labels = c("Complete", "Absent", "Postponed", "Refused",
                                        "Incomplete", "Disabled", "Other"))]
  par(mfrow = c(1, 1), mar = c(1, 7, 4, 1))
  hbar(set_names(qsresult$N, as.character(qsresult$label)),
       sprintf("%s (%.1f%%)", fmt(qsresult$N), 100 * qsresult$N / sum(qsresult$N)),
       col = ifelse(qsresult$QSRESULT == 1L, blue, grey),
       main = "Health questionnaire result")
  plots$qsresult <- recordPlot()

  # SALT-eligible adults (crosswalk §2, §3d): complete interview, aged 18+, usual resident
  adults <- raw[QSRESULT == 1L & QS23 >= 18L & HV102 == 1L]
  adults[, `:=`(w   = Peso15_AMAS / 1e6,
                sex = factor(QSSEXO, 1:2, c("Men", "Women")))]

  # 2. Rows left after each filter; the last bar is the analytic sample
  flow <- c("All rows"               = nrow(raw),
            "Complete interview"     = raw[QSRESULT == 1L, .N],
            "Aged 18+"               = raw[QSRESULT == 1L & QS23 >= 18L, .N],
            "Usual resident"         = nrow(adults),
            "BP measured"            = adults[QS906 == 1L, .N],
            "Weight/height measured" = adults[QS906 == 1L & QS902 %in% c(1L, 4L), .N])
  par(mar = c(1, 11, 4, 1))
  hbar(flow, c(fmt(flow[1]), sprintf("%s (-%s)", fmt(flow[-1]), fmt(-diff(flow)))),
       col = c(rep(grey, 5), blue), main = "Rows left after each filter")
  plots$flow <- recordPlot()

  # 3. Age-sex pyramid, weighted % of eligible adults
  adults[, age_grp := cut(QS23, c(18, seq(25, 80, 5), Inf), right = FALSE,
                          labels = c("18-24", paste0(seq(25, 75, 5), "-", seq(29, 79, 5)), "80+"))]
  total_w <- adults[, sum(w)]
  pyr <- dcast(adults[, .(pct = 100 * sum(w) / total_w), keyby = .(age_grp, sex)],
               age_grp ~ sex, value.var = "pct")
  lim <- 1.1 * max(pyr$Men, pyr$Women)
  ticks <- pretty(c(0, lim))
  par(mar = c(4, 5, 4, 1))
  barplot(-pyr$Men, names.arg = pyr$age_grp, horiz = TRUE, las = 1, col = blue, border = NA,
          space = 0.15, xlim = c(-lim, lim), axes = FALSE,
          main = "Age and sex", xlab = "Weighted % of eligible adults")
  barplot(pyr$Women, horiz = TRUE, col = orange, border = NA, space = 0.15, add = TRUE,
          axes = FALSE)
  axis(1, at = c(-rev(ticks[-1]), ticks), labels = c(rev(ticks[-1]), ticks))
  legend("topright", c("Men", "Women"), fill = c(blue, orange), border = NA, bty = "n")
  plots$age_sex <- recordPlot()

  # 4. Continuous Globorisk inputs, weighted densities. BP needs QS906 == 1. Weight and
  #    height need QS902 1 or 4: for code 4, INEI already copied RECH5's HA2/HA3 into QS900/QS901
  bp <- adults[QS906 == 1L]
  anthro <- adults[QS902 %in% c(1L, 4L)][, bmi := QS900 / (QS901 / 100)^2]
  wdens <- \(x, w) density(x, weights = w / sum(w), bw = bw.nrd0(x))

  bp_panel <- \(first, second, ref, main) {
    d1 <- wdens(bp[[first]], bp$w)
    d2 <- wdens(bp[[second]], bp$w)
    plot(d1, type = "n", main = main, xlab = "mmHg", ylab = "", yaxt = "n", bty = "n",
         ylim = c(0, max(d1$y, d2$y)))
    abline(v = ref, col = "grey85")
    lines(d1, col = blue, lwd = 2)
    lines(d2, col = orange, lwd = 2)
  }
  # Three panels on top; a 1.5 cm strip underneath holds the shared legend
  layout(matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, byrow = TRUE), heights = c(1, lcm(1.5)))
  par(mar = c(4, 1, 4, 1))
  bp_panel("QS903S", "QS905S", c(130, 140), "Systolic BP")
  bp_panel("QS903D", "QS905D", c(80, 90), "Diastolic BP")
  d_bmi <- wdens(anthro$bmi, anthro$w)
  plot(d_bmi, type = "n", main = "BMI", xlab = expression(kg/m^2), ylab = "", yaxt = "n",
       bty = "n")
  abline(v = c(18.5, 25, 30), col = "grey85")
  lines(d_bmi, col = blue, lwd = 2)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", c("1st reading", "2nd reading"), col = c(blue, orange), lwd = 2,
         bty = "n", horiz = TRUE)
  plots$bp_bmi <- recordPlot()

  # 5. Self-reported risk factors by sex, weighted %. "Don't know" and skipped questions
  #    count as No (QS201 is asked only if QS200 == 1, QS202 only if QS201 == 1)
  rf_labels <- c(smoke_12m = "Smoked, last 12 months", smoke_30d = "Smoked, last 30 days",
                 smoke_daily = "Smokes daily", htn_dx = "Diagnosed hypertension",
                 dm_dx = "Diagnosed diabetes")
  adults[, `:=`(smoke_12m   = QS200 %in% 1L,
                smoke_30d   = QS201 %in% 1L,
                smoke_daily = QS202 %in% 1L,
                htn_dx      = QS102 %in% 1L,
                dm_dx       = QS109 %in% 1L)]
  rf <- adults[, map(.SD, \(v) 100 * sum(w * v) / sum(w)), keyby = sex,
               .SDcols = names(rf_labels)]
  # Women first so Men's bar sits on top of each pair, matching the legend order
  m <- as.matrix(rf, rownames = "sex")[c("Women", "Men"), rev(names(rf_labels))]
  par(mfrow = c(1, 1), mar = c(1, 11, 4, 1))
  mids <- barplot(m, beside = TRUE, horiz = TRUE, las = 1, col = c(orange, blue), border = NA,
                  names.arg = rf_labels[colnames(m)], axes = FALSE, xlim = c(0, 1.25 * max(m)),
                  main = "Self-reported risk factors, weighted %")
  text(m, mids, sprintf("%.1f%%", m), pos = 4, cex = 0.75, xpd = TRUE)
  legend("bottomright", c("Men", "Women"), fill = c(blue, orange), border = NA, bty = "n")
  plots$risk_factors <- recordPlot()

  # 6. Socio-economic and place covariates, weighted %. HV108 in SALT's eduacat bands;
  #    98 (don't know) falls outside the breaks and becomes NA
  adults[, `:=`(
    wealth = factor(HV270, 1:5, c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    educ   = cut(HV108, c(-Inf, 6, 11, 97), labels = c("<7 years", "7-11 years", "12+ years")),
    region = factor(SHREGION, 1:4, c("Lima Metro", "Rest of coast", "Highlands", "Amazon")),
    area   = factor(HV025, 1:2, c("Urban", "Rural"))
  )]
  covariates <- c(wealth = "Wealth quintile", educ = "Education",
                  region = "Natural region", area = "Area")
  par(mfrow = c(2, 2), mar = c(1, 8, 4, 1))
  iwalk(covariates, \(main, v) {
    p <- adults[, .(pct = sum(w)), keyby = v][, pct := 100 * pct / sum(pct)]
    hbar(set_names(p$pct, as.character(p[[v]])), sprintf("%.1f%%", p$pct), col = blue,
         main = main)
  })
  plots$covariates <- recordPlot()

  # How many adults in Tumbes
  print(paste("Number of adults in Tumbes:", 
              as.character(adults[HV024 == 24L, .N])))
  # Respondent on health questionnaire: 29651
  print(paste("Number of respondents in health questionnaire:", 
              as.character(raw[QSRESULT == 1L, .N])))

  # Graphs to the global environment
  iwalk(plots, \(p, name) assign(paste0("plot_", name), p, envir = globalenv()))
}