#------------------
# Testing cleaning functions
#------------------

pacman::p_load(testthat)

source(here::here("functions", "cleaning.R"))
source(here::here("functions", "dictionary.R"))

print("Running tests for cleaning and dictionary functions...")

# Dictionary functions
print("Testing dictionary functions")
test_that("dictionary functions load without error", {
  expect_error(create_dictionary(), NA)
})

# Cleaning functions
print("Testing cleaning functions")
test_that("import_data returns a data.table", {
  result <- import_data("wide")
  expect_s3_class(result, "data.table")
})

test_that("import_data returns a data.table", {
  result <- import_data("long")
  expect_s3_class(result, "data.table")
})

test_that("import_data has no labelled variables", {
  result <- import_data("wide")
  classes <- sapply(result, class)
  expect_false(any(grepl("labelled", classes)))
})

test_that("import_data has no labelled variables", {
  result <- import_data("long")
  classes <- sapply(result, class)
  expect_false(any(grepl("labelled", classes)))
})

test_that("import_data preserves value labels as factors", {
  result <- import_data("wide")
  expect_true(any(sapply(result, is.factor)))
})

test_that("cleaning process does not break data integrity", {
  result <- clean_long()
  expect_true(nrow(result) > 0)
})