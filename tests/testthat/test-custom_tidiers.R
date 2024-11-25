skip_on_cran()
skip_if_not(is_pkg_installed(c("broom.helpers", "cardx")))

test_that("tidy_standardize()", {
  skip_if_not(is_pkg_installed(c("parameters", "effectsize")))

  expect_snapshot(
    lm(age ~ grade + marker, trial) |>
      tidy_standardize()
  )
})

test_that("tidy_bootstrap()", {
  skip_if_not(is_pkg_installed(c("parameters", "withr")))
  withr::local_seed(11235)

  expect_error(
    df <- lm(age ~ grade + marker, trial) |>
      tidy_bootstrap(exponentiate = TRUE) |>
      suppressWarnings(), # ignores deprecation warning out of our control present on 2024-10-02. Can later be deleted
    NA
  )
  expect_snapshot(df)
})

test_that("tidy_wald_test()", {
  skip_if_not(is_pkg_installed(c("aod")))

  expect_snapshot(
    lm(age ~ grade + marker, trial) |>
      tidy_wald_test(exponentiate = TRUE)
  )
})

test_that("tidy_robust()", {
  skip_if_not(is_pkg_installed(c("parameters", "insight")))

  expect_snapshot(
    lm(age ~ grade + marker, trial) |>
      tidy_robust(exponentiate = TRUE)
  )
})
