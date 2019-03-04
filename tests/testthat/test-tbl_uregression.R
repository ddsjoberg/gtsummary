context("test-tbl_uregression")
library(survival)
library(lme4)


test_that("lm: no errors/warnings with standard use", {
  expect_error(mtcars %>%
    tbl_uregression(
      method = "lm",
      y = "mpg"
    ), NA)
  expect_warning(mtcars %>%
    tbl_uregression(
      method = "lm",
      y = "mpg"
    ), NA)
})

test_that("coxph: no errors/warnings with standard use", {
  expect_error(lung %>%
    tbl_uregression(
      method = "coxph",
      y = "Surv(time, status)"
    ), NA)
  expect_warning(lung %>%
    tbl_uregression(
      method = "coxph",
      y = "Surv(time, status)"
    ), NA)
})


test_that("glmer: no errors/warnings with standard use", {
  expect_error(
    mtcars %>%
      dplyr::select("am", "gear", "hp", "cyl") %>%
      tbl_uregression(
        method = "glmer",
        y = "am",
        formula = "{y} ~ {.x} + (1 | gear)",
        method.args = list(family = binomial)
      ), NA
  )
  expect_warning(
    mtcars %>%
      dplyr::select("am", "gear", "hp", "cyl") %>%
      tbl_uregression(
        method = "glmer",
        y = "am",
        formula = "{y} ~ {.x} + (1 | gear)",
        method.args = list(family = binomial)
      ), NA
  )
})
