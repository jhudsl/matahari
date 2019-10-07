context("Test that dance_start and related functions handle tbl correctly.")

dance_start()
4 + 4
"wow!"
mean(1:10)
dance_stop()

result <- matahari:::env$.dance

test_that("dance_start logged expressions", {
  expect_null(result)
})

env$.dance <- readRDS(system.file("test", "interactive_dance_data.rds",
                                            package = "matahari"))

test_that("dance_tbl retrived table", {
  expect_equal(6, nrow(dance_tbl()))
})

temp_file <- tempfile()

test_that("dance_save sanity check", {
  expect_false(file.exists(temp_file))
})

dance_save(temp_file)

test_that("dance_save can save a table", {
  expect_true(file.exists(temp_file))
})

dance_remove()
