test_that("add_glance() works", {
  expect_error(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_source_note(),
    NA
  )

  expect_error(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_table(),
    NA
  )
})
