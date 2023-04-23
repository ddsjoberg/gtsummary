skip_on_cran()

test_that("input checks", {
  expect_snapshot(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      modify_column_hide(column = ci) %>%
      modify_column_unhide(column = std.error) %>%
      as.data.frame()
  )
})
