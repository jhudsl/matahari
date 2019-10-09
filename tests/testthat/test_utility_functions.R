context("Test utility functions.")

test_that("ie works logically", {
  expect_true(ie(TRUE, TRUE, FALSE))
  expect_false(ie(FALSE, TRUE, FALSE))
})

env$.dance <- readRDS(system.file("test", "interactive_dance_data.rds",
  package = "matahari"
))

report <- dance_report()

test_that("dance_report creates a string", {
  expect_true(is.character(report))
})

test_that("dance_report makes table", {
  expect_equal(7, nrow(dance_report(input = report)))
})

test_that("dance_report can clip", {
  skip_if(interactive())
  expect_error(dance_report(clip = TRUE))
})

test_that("there_is_a_dance", {
  expect_true(there_is_a_dance())
})
