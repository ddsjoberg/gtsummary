context("test-modify_header")

tbl_summary_noby <- trial %>% tbl_summary()
tbl_summary_by <- trial %>% tbl_summary(by = "trt")

test_that("input checks", {
  expect_error(
    tbl_summary_noby %>% modify_header(stat_by = "test"),
    "'stat_by' argument can only be applied to a 'tbl_summary' object that includes a 'by' argument."
  )
})
