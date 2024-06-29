test_that("tbl_cross() works", {
  expect_error(
    tbl_cross(trial, trt, grade),
    NA
  )
})
