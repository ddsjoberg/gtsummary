context("test-set_gtsummary_theme")
testthat::skip_on_cran()

test_that("setting themes", {
  expect_error(
    set_gtsummary_theme(theme_gtsummary_journal("jama")),
    NA)

  expect_error(
    set_gtsummary_theme(theme_gtsummary_compact()),
    NA)

  expect_error(
    tbl_summary(trial, by = trt) %>% add_p() %>% add_stat_label(),
    NA)

  expect_error(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression(),
    NA)

  expect_error(
    reset_gtsummary_theme(),
    NA)
})
