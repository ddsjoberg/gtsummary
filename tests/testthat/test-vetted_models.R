# Proposed list of checks all "vetted" models should pass.
# When adding a new "vetted model", copy paste the below list and
# add appropriate section of unit tests to cover the below.

# 1.  Runs as expected without errors, warnings, messages (standard usage)
# 2.  Runs as expected without errors, warnings, messages (edge cases specific
#     to the model type, such as using cubic splines, interaction terms, different outcome types)
# 3.  Check labels
# 4.  Check that numbers in model match the table_body of tbl_regression object
# 5.  Check Exponentiating works
# 6.  add_nevent and check that it works appropriately
# 7.  add_n returns appropriate number for model
# 8.  Model works with tbl_uvregression()
# 9.  Model works with add_global_p()
# 10. Model works with combine_terms()

context("test-vetted_models")
library(survival)
library(lme4)
set.seed(23433)

# function to pull estimates from tbl_regression object
# input gt object, output vector of coefficients
coefs_in_gt <- function(x) {
  x$table_body %>%
    dplyr::pull(estimate) %>%
    na.omit() %>%
    as.vector()
}

# lm() -------------------------------------------------------------------------
test_that("vetted_models lm()", {
  # 1. Runs as expected without errors, warnings, messages (basic usage)
  # build model
  mod_lm <- lm(hp ~ am+ disp + as.factor(cyl), data = mtcars)
  coefs <- data.frame(betas = coef(mod_lm)[-1])
  # check standard usage
  expect_error(tbl_lm <- tbl_regression(mod_lm), NA)
  expect_warning(tbl_regression(mod_lm), NA)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    coefs$betas,
    coefs_in_gt(tbl_lm)
  )

  # 2.  Runs as expected without errors, warnings, messages (edge cases specific
  #     to the model type, such as using cubic splines, interaction terms, different outcome types)
  expect_error(tbl_regression(mod_lm, tidy_fun = broom::tidy), NA)
  expect_warning(tbl_regression(mod_lm, tidy_fun = broom::tidy), NA)

  # 3. check labels
  mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
  tbl_logistic <- tbl_regression(mod_logistic)
  #Check labels match
  expect_equivalent(
    tbl_logistic$table_body$label[tbl_logistic$table_body$row_type == "label"], as.character(map(trial %>% select(age, stage), ~attr(.x,"label"))))

  #Update labels and check if new labels match
  mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
  tbl_logistic <- tbl_regression(mod_logistic, list(stage ~ "Path Stage"))
  #Check labels match
  expect_equivalent(
    tbl_logistic$table_body$label[tbl_logistic$table_body$row_type == "label"], c("Age, yrs" ,  "Path Stage"))
})


# survreg() --------------------------------------------------------------------
test_that("vetted_models survreg()", {
  # 1. Runs as expected without errors, warnings, messages (basic usage)
  # build model
  mod_survreg <- survreg(Surv(time, status) ~ age + ph.ecog, data = lung)
  coefs <- data.frame(betas = coef(mod_survreg)[-1])
  # check standard usage
  expect_error(tbl_surv <- tbl_regression(mod_survreg), NA)
  expect_warning(tbl_regression(mod_survreg), NA)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    coefs$betas,
    coefs_in_gt(tbl_surv)
  )
})

# glm() ------------------------------------------------------------------------
test_that("vetted_models glm()", {
  # 1. Runs as expected without errors, warnings, messages (basic usage)
  # build model
  mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
  coefs <- data.frame(betas = coef(mod_logistic)[-1])
  # check standard usage
  expect_error(tbl_logistic <- tbl_regression(mod_logistic), NA)
  expect_warning(tbl_regression(mod_logistic), NA)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    coefs$betas,
    coefs_in_gt(tbl_logistic)
  )

  # build model
  mod_poisson <- glm(count ~ age + trt,
                     trial %>% dplyr::mutate(count = sample.int(20, size = nrow(trial), replace = TRUE)),
                     family = poisson
  )
  coefs <- data.frame(betas = coef(mod_poisson)[-1])
  # check standard usage
  expect_error(tbl_poisson <- tbl_regression(mod_poisson), NA)
  expect_warning(tbl_regression(mod_poisson), NA)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    coefs$betas,
    coefs_in_gt(tbl_poisson)
  )

  # 5. Check Exponentiating works (for applicable models)
  mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
  coefs <- data.frame(betas = coef(mod_logistic)[-1])
  tbl_logistic <- tbl_regression(mod_logistic, exponentiate = TRUE)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    exp(coefs$betas),
    coefs_in_gt(tbl_logistic))

  #glm poisson
  mod_poisson <- glm(count ~ age + trt,
                     trial %>% dplyr::mutate(count = sample.int(20, size = nrow(trial), replace = TRUE)),
                     family = poisson)
  coefs <- data.frame(betas = coef(mod_poisson)[-1])
  tbl_pois <- tbl_regression(mod_poisson, exponentiate = TRUE)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    exp(coefs$betas),
    coefs_in_gt(tbl_pois))

  # 6. add_nevent and check that it works appropriately
  #glm logistic
  mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
  tbl_logistic <- tbl_regression(mod_logistic, exponentiate = TRUE) %>%
    add_nevent()
  # check that tbl_regression object output matches model output
  expect_equivalent(table(mod_logistic$y)[2],tbl_logistic$nevent)
})

# lmer() -----------------------------------------------------------------------
test_that("vetted_models lmer()", {
  # build model
  mod_lmer <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  coefs <- data.frame(betas = fixef(mod_lmer)[-1])
  # check standard usage
  expect_error(tbl_lmer <- tbl_regression(mod_lmer), NA)
  expect_warning(tbl_regression(mod_lmer), NA)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    coefs$betas,
    coefs_in_gt(tbl_lmer)
  )
})

# glmer() ----------------------------------------------------------------------
test_that("vetted_models glmer()", {
  # build model
  mod_glmer <- glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial)
  coefs <- data.frame(betas = fixef(mod_glmer)[-1])
  # check standard usage
  expect_error(tbl_glmer <- tbl_regression(mod_glmer), NA)
  expect_warning(tbl_regression(mod_glmer), NA)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    coefs$betas,
    coefs_in_gt(tbl_glmer)
  )

  # 6. add_nevent and check that it works appropriately
  #glmer
  mod_glmer <- glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial)
  tbl_glmer <- tbl_regression(mod_glmer, exponentiate = TRUE) %>%
    add_nevent()
  # check that tbl_regression object output matches model output
  expect_equivalent(table(mod_glmer@resp$y)[2], tbl_glmer$nevent)

  # 5. Check Exponentiating works (for applicable models)
  #glmer
  mod_glmer <- glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial)
  coefs <- data.frame(betas = fixef(mod_glmer)[-1])
  tbl_glmer <- tbl_regression(mod_glmer, exponentiate = TRUE)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    exp(coefs$betas),
    coefs_in_gt(tbl_glmer))
})


# coxph() ----------------------------------------------------------------------
test_that("vetted_models coxph()", {
  # 5. Check Exponentiating works (for applicable models)
  # coxph
  mod_surv <- coxph(Surv(time, status) ~ age + ph.ecog, data = lung)
  coefs <- data.frame(betas = coef(mod_surv))
  tbl_surv <- tbl_regression(mod_surv, exponentiate = TRUE)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    exp(coefs$betas),
    coefs_in_gt(tbl_surv))

  # coxph
  mod_surv <- coxph(Surv(time, status) ~ age + ph.ecog, data = lung)
  tbl_surv <- tbl_regression(mod_surv, exponentiate = TRUE) %>%
    add_nevent()
  # check that add_nevents matches output events
  expect_equivalent(
    mod_surv$nevent, tbl_surv$nevent)
})
