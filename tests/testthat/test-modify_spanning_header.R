context("test-modify_spanning_header")
testthat::skip_on_cran()

test_that("modify_spanning_header works", {
  expect_error(
    trial %>%
      dplyr::select(trt, age, grade) %>%
      tbl_summary(by = trt) %>%
      modify_spanning_header(starts_with("stat_") ~ "**Randomization Assignment**"),
    NA
  )
})
