context("Test that functions fail gracefully")

test_that("dance_remove returns FALSE", {
 expect_false(dance_remove())
})

test_that("dance_tbl returns NULL", {
  expect_null(dance_tbl())
})
