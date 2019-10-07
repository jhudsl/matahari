context("Test that dance_recital sanely evaluates R code.")

code_file <- system.file("test", "sample_code.R", package = "matahari")

# # Create file to compare
# code_file %>%
#   dance_recital() %>%
#   saveRDS(file = file.path("inst", "test", "code_file_data.rds"))

recital_result <- code_file %>%
  dance_recital()

expected_result <- readRDS(system.file("test", "code_file_data.rds",
                                       package = "matahari"))

test_that("dance_recital can read a code file", {
  expect_equal(purrr::flatten(recital_result), purrr::flatten(expected_result))
})

code_string <- "m <- function(){'bb'};m();x <- c(7L, 8L, 3L, 4L, 5L);median(x);y"

# # Create file to compare
# code_string %>%
#   dance_recital() %>%
#   saveRDS(file = file.path("inst", "test", "code_string_data.rds"))

expected_string_result <- readRDS(system.file("test", "code_string_data.rds",
                                              package = "matahari"))

recital_string_result <- code_string %>%
  dance_recital()

test_that("dance_recital can read a code string", {
  expect_equal(purrr::flatten(recital_string_result),
               purrr::flatten(expected_string_result))
})

test_that("dance_recital warns when string ends in .R", {
  expect_warning(dance_recital("x.R = 2;5 * x.R"))
})
