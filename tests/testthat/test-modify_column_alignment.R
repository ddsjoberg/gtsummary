skip_on_cran()

test_that("modify_column_alignment() works", {
  expect_snapshot(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      modify_column_alignment(columns = everything(), align = "left") %>%
      as.data.frame()
  )
})
