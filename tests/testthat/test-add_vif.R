skip_on_cran()
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))

test_that("no errors/warnings with standard", {
  expect_snapshot(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      add_vif() %>%
      as.data.frame()
  )

  expect_snapshot(
    lm(age ~ marker + response, trial) %>%
      tbl_regression() %>%
      add_vif() %>%
      as.data.frame()
  )

  expect_snapshot(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = "aGVIF") %>%
      as.data.frame()
  )

  expect_snapshot(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = c("aGVIF", "df")) %>%
      as.data.frame()
  )

  expect_error(
    lm(age ~ marker + response, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = "aGVIF"),
    "*"
  )

  expect_snapshot(
    lm(age ~ marker + response, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = "VIF") %>%
      as.data.frame()
  )

  expect_snapshot(
    lm(age ~ marker + response, trial) %>%
      tbl_regression() %>%
      add_vif(statistic = "VIF") %>%
      as.data.frame()
  )
})
