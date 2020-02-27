# Proposed list of checks all "vetted" models should pass.
# When adding a new "vetted model", copy paste the below list and
# add appropriate section of unit tests to cover the below.

# 1.  Runs as expected with standard use
#       - without errors, warnings, messages
#       - numbers in table are correct
#       - labels are correct
# 2.  If applicable, runs as expected with logit and log link
#       - without errors, warnings, messages
#       - numbers in table are correct
# 3.  Interaction terms are correctly printed in output table
#       - without errors, warnings, messages
#       - numbers in table are correct
#       - interaction labels are correct
# 4.  Other gtsummary functions work with model: add_global_p(), combine_terms(), add_nevent()
#       - without errors, warnings, messages
#       - numbers in table are correct
# 5.  tbl_uvregression() works as expected
#       - without errors, warnings, messages
#       - works with add_global_p(), add_nevent()

# stats::lm()           DONE
# stats::glm()
# survival::survreg()
# survival::coxph()
# lme4::lmer()
# lme4::glmer()
# geepack::geeglm()

context("test-vetted_models")
library(dplyr)
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
  # build model
  mod_lm <- lm(hp ~ am + disp + as.factor(cyl), data = mtcars)

  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(tbl_lm <- tbl_regression(mod_lm), NA)
  expect_warning(tbl_regression(mod_lm), NA)

  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_lm)[-1],
    coefs_in_gt(tbl_lm)
  )

  #       - labels are correct
  expect_equivalent(
    tbl_lm$table_body %>%
    filter(row_type == "label") %>%
    pull(label),
    c("am", "disp", "as.factor(cyl)")
  )

  # 2.  If applicable, runs as expected with logit and log link
  # NOT APPLICABLE

  # 3.  Interaction terms are correctly printed in output table
  mod_lm2 <- lm(hp ~ disp + as.factor(cyl) * am, data = mtcars)
  #       - without errors, warnings, messages
  expect_error(tbl_lm2 <- tbl_regression(mod_lm2), NA)
  expect_warning(tbl_regression(mod_lm2), NA)

  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_lm2)[-1],
    coefs_in_gt(tbl_lm2)
  )

  #       - interaction labels are correct
  expect_equivalent(
    tbl_lm2$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("disp", "as.factor(cyl)", "am", "as.factor(cyl) * am")
  )
  expect_equivalent(
    tbl_lm2$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("as.factor(cyl) * am", "6 * am", "8 * am")
  )

  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms(), add_nevent()
  #       - without errors, warnings, messages
  expect_error(tbl_lm3 <- tbl_lm2 %>%
                 add_global_p(include =  everything()), NA)
  expect_warning(tbl_lm3, NA)
  expect_error(tbl_lm2 %>%
                 combine_terms(formula_update = . ~ . - disp), NA)
  expect_warning(tbl_lm3, NA)

  #       - numbers in table are correct
  expect_equivalent(
    tbl_lm3$table_body %>%
      pull(p.value) %>%
      na.omit(),
    car::Anova(mod_lm2, type = "III") %>%
      as.data.frame() %>%
      slice(-1) %>% # removing intercept
      pull(`Pr(>F)`) %>%
      na.omit()
  )

  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  expect_error(
    tbl_lmuv <- tbl_uvregression(
      trial,
      y = age,
      method = lm
    ),
    NA
  )
  expect_warning(
    tbl_lmuv,
    NA
  )
  #       - works with add_global_p()
  expect_error(
    tbl_lmuv <- tbl_uvregression(
      trial,
      y = age,
      method = lm
    ) %>%
      add_global_p(),
    NA
  )
})

# glm() ------------------------------------------------------------------------
test_that("vetted_models glm()", {
  # build model
  mod_glm <- glm(response ~ age + trt + grade, data = trial, family = binomial)

  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(tbl_glm <- tbl_regression(mod_glm), NA)
  expect_warning(tbl_regression(mod_glm), NA)

  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_glm)[-1],
    coefs_in_gt(tbl_glm)
  )

  #       - labels are correct
  expect_equivalent(
    tbl_glm$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade")
  )

  # 2.  If applicable, runs as expected with logit and log link
  expect_equivalent(
    exp(coef(mod_glm)[-1]),
    tbl_regression(mod_glm, exponentiate = TRUE) %>%
      coefs_in_gt()
  )

  # 3.  Interaction terms are correctly printed in output table
  mod_glm2 <- glm(response ~ age + trt * grade, data = trial, family = binomial)
  #       - without errors, warnings, messages
  expect_error(tbl_glm2 <- tbl_regression(mod_glm2), NA)
  expect_warning(tbl_regression(mod_glm2), NA)

  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_glm2)[-1],
    coefs_in_gt(tbl_glm2)
  )

  #       - interaction labels are correct
  expect_equivalent(
    tbl_glm2$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade", "Chemotherapy Treatment * Grade")
  )
  expect_equivalent(
    tbl_glm2$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("Chemotherapy Treatment * Grade", "Drug B * II", "Drug B * III" )
  )

  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms(), add_nevent()
  #       - without errors, warnings, messages
  expect_error(tbl_glm3 <- tbl_glm2 %>%
                 add_global_p(include = everything()), NA)
  expect_warning(tbl_glm3, NA)
  expect_error(tbl_glm %>%
                 combine_terms(formula_update = . ~ . - trt,
                               test = "LRT") %>%
                 add_nevent(), NA)
  expect_warning(tbl_glm %>%
                   combine_terms(formula_update = . ~ . - trt,
                                 test = "LRT") %>%
                   add_nevent(), NA)

  #       - numbers in table are correct
  expect_equivalent(
    tbl_glm3$table_body %>%
      pull(p.value) %>%
      na.omit(),
    car::Anova(mod_glm2, type = "III") %>%
      as.data.frame() %>%
      pull(`Pr(>Chisq)`) %>%
      na.omit()
  )

  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  expect_error(
    tbl_glmuv <- tbl_uvregression(
      trial,
      y = age,
      method = glm
    ),
    NA
  )
  expect_warning(
    tbl_glmuv,
    NA
  )
  #       - works with add_global_p(), add_nevent()
  expect_error(
    tbl_glmuv <- tbl_uvregression(
      trial,
      y = response,
      method = glm,
      method.args = list(family = binomial)
    ) %>%
      add_global_p() %>%
      add_nevent(),
    NA
  )
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
