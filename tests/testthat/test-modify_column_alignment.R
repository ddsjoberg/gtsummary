test_that("modify_column_alignment() works", {
  expect_error(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      modify_column_alignment(columns = everything(), align = "left"),
    NA
  )
})
