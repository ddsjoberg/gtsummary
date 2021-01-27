context("test-add_n")
testthat::skip_on_cran()

mod <- lm(age ~ marker + grade, trial)

test_that("no errors/warnings with tidy_standardize", {
  skip_if(!require("effectsize"))
  expect_error(tbl_regression(mod, tidy_fun = tidy_standardize), NA)
  expect_warning(tbl_regression(mod, tidy_fun = tidy_standardize), NA)
})

test_that("no errors/warnings with tidy_bootstrap", {
  skip_if(!require("parameters"))
  expect_error(tbl_regression(mod, tidy_fun = tidy_bootstrap), NA)
  expect_warning(tbl_regression(mod, tidy_fun = tidy_bootstrap), NA)
})


test_that("no errors/warnings with pool_and_tidy_mice", {
  skip_if(!require("mice"))
  mod_mice <-
    suppressWarnings(mice::mice(trial, m = 2)) %>%
    with(glm(response ~ age + marker + grade, family = binomial))

  expect_error(tbl_regression(mod_mice), NA)
  expect_warning(tbl_regression(mod_mice, exponentiate = TRUE), NA)

  expect_output(mice::pool(mod_mice), NA)
})


test_that("no errors/warnings with tbl_regression.multinom", {
  skip_if(!require("nnet"))
  expect_output(
    nnet::multinom(grade ~ age, trial) %>%
      tbl_regression()
  )
})

test_that("no errors/warnings with tbl_regression.gam", {
  skip_if(!require("mgcv"))
  mod <- mgcv::gam(response ~ s(marker, age) + grade, data = trial, family = binomial)

  expect_error(
    mod %>% tidy_gam(),
    NA
  )

  expect_error(
    mod %>%
      tbl_regression(
        exponentiate = TRUE,
        label = `s(marker,age)` ~ "Smoothed marker/age"
      ),
    NA
  )
})
