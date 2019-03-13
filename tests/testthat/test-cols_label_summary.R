context("test-cols_label_summary")

tbl_summary_noby <- trial %>% tbl_summary()
tbl_summary_by <- trial %>% tbl_summary(by = "trt")

test_that("input checks", {
  expect_error(
    cols_label_summary(mtcars),
    "Class of 'x' must be tbl_summary"
  )
  expect_error(
    tbl_summary_noby %>% cols_label_summary(stat_by = "test"),
    "Cannot specify 'stat_by' without first including 'by' in 'tbl_summary'"
  )
  expect_error(
    tbl_summary_by %>% cols_label_summary(stat_overall = "test"),
    "Cannot specify 'stat_overall' when no overall statistics are present in 'tbl_summary'"
  )
})
