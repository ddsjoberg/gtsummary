context("test-add_vif")
testthat::skip_on_cran()

test_that("no errors/warnings with standard", {
  expect_error(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      add_vif(),
    NA
  )

  expect_error(
    lm(age ~ marker + response, trial) %>%
      tbl_regression() %>%
      add_vif(),
    NA
  )

  expect_error(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = "GVIF^(1/(2*Df))"),
    NA
  )

  expect_error(
    lm(age ~ marker + response, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = "GVIF^(1/(2*Df))"),
    "*"
  )
})
