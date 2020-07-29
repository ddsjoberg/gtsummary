context("test-add_nevent")
testthat::skip_on_cran()

library(survival)
fit_cox <- coxph(Surv(time, status) ~ sex, lung)
fit_glm <- glm(response ~ trt, trial, family = binomial)

test_that("add_nevent after tbl_regression creates output without error/warning", {
  # cox model
  expect_error(
    fit_cox %>%
      tbl_regression() %>%
      add_nevent(),
    NA
  )
  expect_warning(
    fit_cox %>%
      tbl_regression() %>%
      add_nevent(),
    NA
  )

  # glm model
  expect_error(
    fit_glm %>%
      tbl_regression() %>%
      add_nevent(),
    NA
  )
  expect_warning(
    fit_glm %>%
      tbl_regression() %>%
      add_nevent(),
    NA
  )
})



test_that("add_nevent after tbl_uvregression creates output without error/warning", {
  # cox model
  expect_error(
    tbl_uvregression(
      trial,
      method = coxph,
      y = Surv(ttdeath, death),
    ) %>%
      add_nevent(),
    NA
  )
  expect_warning(
    tbl_uvregression(
      trial,
      method = coxph,
      y = Surv(ttdeath, death),
    ) %>%
      add_nevent(),
    NA
  )

  # glm model
  expect_error(
    tbl_uvregression(
      trial,
      method = glm,
      y = response,
      method.args = list(family = binomial)
    ) %>%
      add_nevent(),
    NA
  )
  expect_warning(
    tbl_uvregression(
      trial,
      method = glm,
      y = response,
      method.args = list(family = binomial)
    ) %>%
      add_nevent(),
    NA
  )
})

test_that("add_nevent error with bad inputs", {
  expect_error(
    lm(hp ~ mpg, mtcars) %>%
      tbl_regression() %>%
      add_nevent(),
    NULL
  )
  expect_error(
    lme4::lmer(hp ~ mpg + (1 | cyl), mtcars) %>%
      tbl_regression() %>%
      add_nevent(),
    NULL
  )
})
