context("test-tbl_cross")

test_that("tbl_cross creates output without error with continuous args", {
  expect_error(
    tbl_cross(mtcars, row = gear, col = am),
    NA
  )
})


test_that("tbl_cross throws error if both `col` and `row`` are not specified", {
  expect_error(
    tbl_cross(trial, col = trt),
    "*"
  )
  expect_error(
    tbl_cross(trial, row = trt),
    "*"
  )
})

test_that("tbl_cross throws works if no `col` or `row` specified", {
  expect_error(
    tbl_cross(trial, col = trt, row = response),
    NA
  )
})



test_that("tbl_summary works in character inputs for `col` and `row", {
  my_variable <- "trt"

  expect_error(
    tbl_cross(trial, col = my_variable, row = response),
    NA
  )
  expect_error(
    tbl_cross(trial, row = my_variable, col = response),
    NA
  )
})


