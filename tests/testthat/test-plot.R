skip_if_not(requireNamespace("ggstats"))

test_that("plot.tbl_regression() works", {
  expect_error(
    lm(age ~ trt, trial) %>%
      tbl_regression() %>%
      plot(remove_reference_rows = TRUE),
    NA
  )
})
