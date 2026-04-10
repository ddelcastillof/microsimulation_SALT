#-------------------
# Dictionary functions
#-------------------

print("Loading dictionary functions")

create_dictionary <- function() {
  require(tidyverse)
  require(haven)
  require(data.table)
  require(labelled)
  require(openxlsx)

  long_file <- here::here("data", "raw", "long.dta")
  wide_file <- here::here("data", "raw", "wide.dta")
  dict_dir <- here::here("data", "raw", "dictionary")
  dict_path <- here::here("data", "raw", "dictionary",
                           "salt_dictionary.xlsx")

  if (!file.exists(long_file)) {
    stop("File 'long.dta' not found in 'data/raw'.")
  }
  if (!file.exists(wide_file)) {
    stop("File 'wide.dta' not found in 'data/raw'.")
  }

  message("Reading raw .dta files with labels...")
  raw_long_dta <- read_dta(long_file) |>
    as.data.table() |>
    mutate(
      across(
        where(is.labelled),
        ~ as_factor(.x, levels = "labels")
      ),
      across(
        everything(),
        ~ { attr(.x, "format.stata") <- NULL; .x }
      )
    )

  raw_wide_dta <- read_dta(wide_file) |>
    as.data.table() |>
    mutate(
      across(
        where(is.labelled),
        ~ as_factor(.x, levels = "labels")
      ),
      across(
        everything(),
        ~ { attr(.x, "format.stata") <- NULL; .x }
      )
    )

  message("Building data dictionaries...")
  data_dictionary_long <- data.frame(
    variable_name = names(raw_long_dta),
    variable_label = sapply(raw_long_dta, function(x) {
      label <- attr(x, "label")
      if (is.null(label)) "" else label
    }),
    row.names = NULL
  )

  data_dictionary_wide <- data.frame(
    variable_name = names(raw_wide_dta),
    variable_label = sapply(raw_wide_dta, function(x) {
      label <- attr(x, "label")
      if (is.null(label)) "" else label
    }),
    row.names = NULL
  )

  if (!dir.exists(dict_dir)) {
    dir.create(dict_dir, recursive = TRUE)
    message("Created 'dictionary' directory.")
  }

  message("Saving dictionary to Excel...")
  wb <- createWorkbook()
  addWorksheet(wb, "data_dictionary_long")
  addWorksheet(wb, "data_dictionary_wide")
  writeData(wb, data_dictionary_long,
            sheet = "data_dictionary_long",
            startRow = 1, startCol = 1)
  writeData(wb, data_dictionary_wide,
            sheet = "data_dictionary_wide",
            startRow = 1, startCol = 1)
  saveWorkbook(wb, dict_path, overwrite = TRUE)

  message("Dictionary saved to: ", dict_path)
  dict_path
}