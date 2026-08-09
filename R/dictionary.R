#-------------------
# Dictionary functions
#-------------------

print("Loading dictionary functions")

create_dictionary <- function() {
  require(haven)
  require(data.table)
  require(yaml)

  long_file <- here::here("data", "raw", "long.dta")
  wide_file <- here::here("data", "raw", "wide.dta")
  dict_dir  <- here::here("data", "dictionary")
  yaml_path <- here::here("data", "dictionary", "salt_dictionary.yaml")

  if (!file.exists(long_file)) stop("File 'long.dta' not found in 'data/raw'.")
  if (!file.exists(wide_file)) stop("File 'wide.dta' not found in 'data/raw'.")

  read_labelled <- function(path) {
    read_dta(path) |>
      as.data.table() |>
      (\(dt) dt[, lapply(.SD, \(x) {
        if (is.labelled(x)) as_factor(x, levels = "labels") else x
      })])()
  }

  extract_var_info <- function(x, nm) {
    label <- attr(x, "label")
    info  <- list(
      name  = nm,
      label = if (is.null(label)) "" else as.character(label),
      type  = class(x)[1L]
    )
    if (is.factor(x)) info$values <- levels(x)
    info
  }

  message("Reading raw .dta files with labels...")
  raw_long <- read_labelled(long_file)
  raw_wide <- read_labelled(wide_file)

  message("Building dictionary...")
  dict <- list(
    long = unname(Map(extract_var_info, raw_long, names(raw_long))),
    wide = unname(Map(extract_var_info, raw_wide, names(raw_wide)))
  )

  if (!dir.exists(dict_dir)) {
    dir.create(dict_dir, recursive = TRUE)
    message("Created 'dictionary' directory.")
  }

  message("Saving dictionary to YAML...")
  write_yaml(dict, yaml_path)

  message("Dictionary saved to: ", yaml_path)
  yaml_path
}

export_village_order <- function(village_col = "Codigo",
                                 wave_col    = "Orden") {
  require(readxl)
  require(data.table)
  require(yaml)

  xls_path  <- here::here("data", "raw", "SALT - Orden de las villas.xls")
  dict_dir  <- here::here("data", "dictionary")
  yaml_path <- here::here("data", "village_order.yaml")

  if (!file.exists(xls_path)) stop("File 'SALT - Orden de las villas.xls' not found in 'data/raw'.")
  message("Reading village order file...")
  raw <- as.data.table(read_excel(xls_path))

  if (!village_col %in% names(raw)) stop("Column '", village_col, "' not found. Available: ", paste(names(raw), collapse = ", "))
  if (!wave_col    %in% names(raw)) stop("Column '", wave_col,    "' not found. Available: ", paste(names(raw), collapse = ", "))

  setorderv(raw, wave_col)

  villages <- raw[, Map(
    \(v, w) list(village_id = as.character(v), crossover_wave = as.integer(w)),
    get(village_col), get(wave_col)
  )]

  if (!dir.exists(dict_dir)) {
    dir.create(dict_dir, recursive = TRUE)
    message("Created 'dictionary' directory.")
  }

  message("Saving village order to YAML...")
  write_yaml(list(villages = unname(villages)), yaml_path)

  message("Village order saved to: ", yaml_path)
  yaml_path
}