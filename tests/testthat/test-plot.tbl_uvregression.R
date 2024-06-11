skip_if_not(broom.helpers::.assert_package("ggstats", pkg_search = "gtsummary", boolean = TRUE))

test_that("plot.tbl_regression() works", {
  expect_silent(
    tbl1 <- tbl_uvregression(
      trial,
      x = trt,
      include = c(marker, age),
      show_single_row = trt,
      method = lm
    )
  )
  expect_error(
    tbl1 %>%
      plot(remove_reference_rows = TRUE),
    NA
  )
})
