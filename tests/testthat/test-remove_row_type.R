context("test-remove_row_type")
testthat::skip_on_cran()

test_that("no errors/warnings with standard use", {
  tbl_sum <- trial %>%
    select(trt, age, grade, stage) %>%
    tbl_summary(by = trt)
  tbl_reg <- lm(age ~ grade + stage, trial) %>% tbl_regression()

  expect_error(tbl1 <- remove_row_type(tbl_sum, type = "header"), NA)
  expect_warning(remove_row_type(tbl_sum, type = "header"), NA)

  expect_equivalent(
    tbl1$table_body$label,
    c("Age", "Unknown", "I", "II", "III", "T1", "T2", "T3", "T4")
  )

  expect_error(tbl2 <- remove_row_type(tbl_sum, type = "missing"), NA)

  expect_equal(
    tbl2$table_body$label,
    c("Age", "Grade", "I", "II", "III", "T Stage", "T1", "T2", "T3", "T4")
  )

  expect_error(tbl3 <- remove_row_type(tbl_reg, type = "reference"), NA)
  expect_equivalent(
    tbl3$table_body$label,
    c("Grade", "II", "III", "T Stage", "T2", "T3", "T4")
  )

  expect_error(remove_row_type(tbl_sum, type = "reference"))
})
