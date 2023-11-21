skip_if_not(broom.helpers::.assert_package("ggstats", pkg_search = "gtsummary", boolean = TRUE))

test_that("plot.tbl_regression() works", {
  expect_error(
    lm(age ~ trt, trial) %>%
      tbl_regression() %>%
      plot(remove_reference_rows = TRUE),
    NA
  )
})
