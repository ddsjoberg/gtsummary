context("test-add_n")
testthat::skip_on_cran()

mod <- lm(age ~ marker + grade, trial)

test_that("no errors/warnings with tidy_standardize", {
  testthat::skip_if_not_installed("effectsize")
  expect_error(tbl_regression(mod, tidy_fun = tidy_standardize), NA)
  expect_warning(tbl_regression(mod, tidy_fun = tidy_standardize), NA)
})

test_that("no errors/warnings with tidy_bootstrap", {
  testthat::skip_if_not_installed("parameters")
  expect_error(tbl_regression(mod, tidy_fun = tidy_bootstrap), NA)
  expect_warning(tbl_regression(mod, tidy_fun = tidy_bootstrap), NA)
})
