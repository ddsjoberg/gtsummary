context("test-tbl_regression")
library(survival)
library(lme4)


mod_lm <- lm(hp ~ am, data = mtcars)
mod_survreg <- survreg(Surv(time, status) ~ age + ph.ecog, data = lung)
mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
mod_poisson <- glm(count ~ age + trt,
  trial %>% dplyr::mutate(count = sample.int(20, size = nrow(trial), replace = TRUE)),
  family = poisson
)
mod_lmer <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
mod_glmer <- glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial)

mod_lm_interaction <- lm(age ~ trt * grade * response, data = trial)


test_that("glm: logistic and poisson regression", {
  expect_error(tbl_regression(mod_logistic), NA)
  expect_warning(tbl_regression(mod_logistic), NA)
  expect_error(tbl_regression(mod_poisson), NA)
  expect_warning(tbl_regression(mod_poisson), NA)

  expect_error(tbl_regression(mod_logistic, exponentiate = TRUE), NA)
  expect_warning(tbl_regression(mod_logistic, exponentiate = TRUE), NA)
  expect_error(tbl_regression(mod_poisson, exponentiate = TRUE), NA)
  expect_warning(tbl_regression(mod_poisson, exponentiate = TRUE), NA)
})

test_that("lm: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lm), NA)
  expect_warning(tbl_regression(mod_lm), NA)
})


test_that("survreg: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_survreg), NA)
  expect_warning(tbl_regression(mod_survreg), NA)
})

test_that("lmer: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lmer), NA)
  expect_warning(tbl_regression(mod_lmer), NA)
})

test_that("glmer: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_glmer), NA)
  expect_warning(tbl_regression(mod_glmer), NA)
})

test_that("lm with interactions: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lm_interaction), NA)
  expect_warning(tbl_regression(mod_lm_interaction), NA)
})

test_that("tbl_regression creates errors when non-function in input", {
  expect_error(
    tbl_regression(mod_lm_interaction, pvalue_fun = mtcars),
    "*"
  )
  expect_error(
    tbl_regression(mod_lm_interaction, estimate_fun = mtcars),
    "*"
  )
})

test_that("tbl_regression creates errors when inputs are wrong", {
  expect_error(
    tbl_regression(mod_lm_interaction, label = "Age"),
    "*"
  )
  expect_error(
    tbl_regression(mod_lm_interaction, include = "INCLUDE ME!"),
    "*"
  )
})
