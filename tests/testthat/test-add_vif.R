skip_on_cran()
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))

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
      add_vif(statistic = "aGVIF"),
    NA
  )

  expect_error(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = c("aGVIF", "df")),
    NA
  )

  expect_error(
    lm(age ~ marker + response, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = "aGVIF"),
    "*"
  )

  expect_error(
    lm(age ~ marker + response, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = "VIF"),
    NA
  )

  expect_error(
    lm(age ~ marker + response, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = "VIF"),
    NA
  )
})
