context("test-fmt_uni_regression")
library(survival)
library(lme4)


test_that("lm: no errors/warnings with standard use", {
  expect_error(mtcars %>%
    fmt_uni_regression(
      method = "lm",
      y = "mpg"
    ), NA)
  expect_warning(mtcars %>%
    fmt_uni_regression(
      method = "lm",
      y = "mpg"
    ), NA)
})

test_that("coxph: no errors/warnings with standard use", {
  expect_error(lung %>%
    fmt_uni_regression(
      method = "coxph",
      y = "Surv(time, status)"
    ), NA)
  expect_warning(lung %>%
    fmt_uni_regression(
      method = "coxph",
      y = "Surv(time, status)"
    ), NA)
})


test_that("glmer: no errors/warnings with standard use", {
  expect_error(
    mtcars %>%
      dplyr::select("am", "gear", "hp", "cyl") %>%
      fmt_uni_regression(
        method = "glmer",
        y = "am",
        formula = "{y} ~ {.x} + (1 | gear)",
        method.args = list(family = binomial)
      ), NA
  )
  expect_warning(
    mtcars %>%
      dplyr::select("am", "gear", "hp", "cyl") %>%
      fmt_uni_regression(
        method = "glmer",
        y = "am",
        formula = "{y} ~ {.x} + (1 | gear)",
        method.args = list(family = binomial)
      ), NA
  )
})
