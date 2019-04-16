context("test-add_nevent")

library(survival)
fit_cox = coxph(Surv(time, status) ~ sex, lung)
fit_glm = glm(response ~ trt, trial, family = binomial)

test_that("add_comparison creates output without error/warning (with by var)", {
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
