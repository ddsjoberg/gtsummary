skip_if_not(is_pkg_installed(c("broom.helpers", "cardx"), reference_pkg = "gtsummary"))

test_that("tidy_standardize()", {
  skip_if_not(is_pkg_installed(c("parameters", "effectsize"), reference_pkg = "gtsummary"))

  expect_snapshot(
    lm(age ~ grade + marker, trial) |>
      tidy_standardize()
  )
})

test_that("tidy_bootstrap()", {
  skip_if_not(is_pkg_installed(c("parameters", "withr"), reference_pkg = "gtsummary"))
  withr::local_seed(11235)

  expect_snapshot(
    lm(age ~ grade + marker, trial) |>
      tidy_bootstrap(exponentiate = TRUE)
  )
})

test_that("tidy_wald_test()", {
  skip_if_not(is_pkg_installed(c("aod"), reference_pkg = "gtsummary"))

  expect_snapshot(
    lm(age ~ grade + marker, trial) |>
      tidy_wald_test(exponentiate = TRUE)
  )
})

test_that("tidy_robust()", {
  skip_if_not(is_pkg_installed(c("parameters", "insight"), reference_pkg = "gtsummary"))

  expect_snapshot(
    lm(age ~ grade + marker, trial) |>
      tidy_robust(exponentiate = TRUE)
  )
})
