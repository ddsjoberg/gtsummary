context("test-add_n")
testthat::skip_on_cran()

mod <- lm(age ~ marker + grade, trial)

test_that("no errors/warnings with tidy_standardize", {
  expect_error(tbl_regression(mod, tidy_fun = tidy_standardize), NA)
  expect_warning(tbl_regression(mod, tidy_fun = tidy_standardize), NA)
})

test_that("no errors/warnings with tidy_bootstrap", {
  expect_error(tbl_regression(mod, tidy_fun = tidy_bootstrap), NA)
  expect_warning(tbl_regression(mod, tidy_fun = tidy_bootstrap), NA)
})


test_that("no errors/warnings with pool_and_tidy_mice", {
  mod_mice <-
    suppressWarnings(mice::mice(trial, m = 2)) %>%
    with(glm(response ~ age + marker + grade, family = binomial))

  expect_error(tbl_regression(mod_mice), NA)
  expect_warning(tbl_regression(mod_mice, exponentiate = TRUE), NA)
})
