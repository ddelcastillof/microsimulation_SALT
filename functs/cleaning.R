#-------------------
# Cleaning functions
#-------------------

print("Loading cleaning functions")

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

clean_long <- function() {
  require(data.table)
  require(lubridate)
  require(forcats)
  tidied_data <- import_data("long")
  
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
      "niveduca",
      "eqindex",
      "disaprev1",
      "disaprev2",
      "disaprev3"
    )
  
  tidied_data <- tidied_data[, ..cols_to_select]
  # transforming date variables into date format
  tidied_data[, c("fecha", "fec_nac") := lapply(.SD, dmy),                                                                                                                                                                                                                
              .SDcols = c("fecha", "fec_nac")]
  # collapsing superior education categories
  tidied_data[, niveduca2 := fct_collapse(
    niveduca,
    Superior = c("Superior NO universitaria", "Superior universitaria")
  )]
  # assigning wave numbers, filling missing from 0:6
  # wave will represent visit number, with 0 being baseline
  tidied_data[, wave := as.integer(time) - 1L]
  tidied_data[, wave := {
    missing_waves <- setdiff(0:6, wave[!is.na(wave)])
    wave[is.na(wave)] <- sort(missing_waves)
    wave
  }, by = codigo]
  return(tidied_data)
}

clean_wide <- function() { #in development, check dictionary
  require(data.table)
  require(lubridate)
  require(forcats)

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
  # collapsing superior education categories
  tidied_data[, niveduca2 := fct_collapse(
    niveduca,
    Superior = c("Superior NO universitaria", "Superior universitaria")
  )]
  return(tidied_data)
}