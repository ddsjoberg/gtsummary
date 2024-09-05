skip_on_cran()

test_that("modify_column_hide() works", {
  expect_snapshot(
    lm(age ~ marker + grade, trial) |>
      tbl_regression() |>
      modify_column_hide(column = conf.low) |>
      modify_column_unhide(column = std.error) |>
      as.data.frame()
  )
})
