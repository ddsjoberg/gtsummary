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

lung2 <- lung
Hmisc::label(lung2$sex) <- "Gender"
Hmisc::label(lung2$age) <- "AGE"
cox_hmisclbl <- coxph(Surv(time, status) ~ age + sex, data = lung2)


test_that("glm: logistic and poisson regression", {
  expect_error(tbl_regression(mod_logistic), NA)
  expect_warning(tbl_regression(mod_logistic), NA)
  expect_error(tbl_regression(mod_poisson, show_single_row = "trt"), NA)
  expect_warning(tbl_regression(mod_poisson, show_single_row = "trt"), NA)

  expect_error(tbl_regression(mod_logistic, exponentiate = TRUE), NA)
  expect_warning(tbl_regression(mod_logistic, exponentiate = TRUE), NA)
  expect_error(tbl_regression(mod_poisson, exponentiate = TRUE, show_single_row = "trt"), NA)
  expect_warning(tbl_regression(mod_poisson, exponentiate = TRUE, show_single_row = "trt"), NA)
})

test_that("lm: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lm), NA)
  expect_warning(tbl_regression(mod_lm), NA)
})

test_that("lm with tidyfun: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lm, tidy_fun = broom::tidy), NA)
  expect_warning(tbl_regression(mod_lm, tidy_fun = broom::tidy), NA)
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
  expect_error(
    tbl_regression(mod_lm_interaction, tidy_fun = mtcars),
    "*"
  )
})

test_that("tbl_regression creates errors when inputs are wrong", {
  expect_error(
    tbl_regression(mod_lm_interaction, label = "Age"),
    "*"
  )
  expect_error(
    tbl_regression(mod_lm_interaction, label = list("Age")),
    "*"
  )
  expect_error(
    tbl_regression(mod_lm_interaction, label = list("age" ~ c("Age", "Two"))),
    "*"
  )
  expect_error(
    tbl_regression(mod_lm_interaction, include = "INCLUDE ME!"),
    "*"
  )
})


test_that("No errors/warnings when data is labelled using Hmisc", {
  expect_error(tbl_regression(cox_hmisclbl), NA)
  expect_warning(tbl_regression(cox_hmisclbl), NA)
})

test_that("show_single_row errors print", {
  expect_error(
    tbl_regression(mod_lm_interaction, show_single_row = "NOT_A_VA"),
    "*"
  )
  expect_error(
    tbl_regression(mod_lm_interaction, show_single_row = "grade"),
    "*"
  )
})


test_that("All labels print with cubic splines", {
  spline_fun <- Hmisc::rcspline.eval
  rsc_mod <- lm(age ~ spline_fun(marker, inclx = TRUE) + response, trial)

  expect_equal(
    tbl_regression(rsc_mod) %>% purrr::pluck("table_body", "label") %>% {sum(is.na(.))},
    0
  )
})


test_that("Testing lme4 results", {
  mod_glmer <- glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial)

  # tbl_regerssion runs without error
  expect_error(
    tbl_lme4 <- tbl_regression(mod_glmer, exponentiate = TRUE,
                               conf.level = 0.90),
    NA
  )

  # coefs are exponentiated properly
  expect_equivalent(
    coef(mod_glmer)[[1]] %>% {.[1, 2:ncol(.)]} %>% map_dbl(exp),
    tbl_lme4$table_body %>% pull(estimate) %>% discard(is.na)
  )
})


