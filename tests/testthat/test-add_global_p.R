skip_on_cran()
skip_if_not(is_pkg_installed("parameters"))

test_that("add_global_p() works", {
  expect_error(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_global_p(),
    NA
  )
})
