context("Test that functions fail gracefully.")

test_that("dance_remove returns FALSE", {
  dance_remove()
  expect_false(dance_remove())
})

test_that("dance_tbl returns NULL", {
  dance_remove()
  expect_null(dance_tbl())
})
