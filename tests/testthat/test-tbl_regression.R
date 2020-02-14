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


# Proposed list of checks all "vetted" models should pass.
# When adding a new "vetted model", copy paste the below list and
# add appropriate section of unit tests to cover the below.

# 1. Runs as expected without errors, warnings, messages (standard usage)
# 2. Runs as expected without errors, warnings, messages (edge cases specific to the model type, such as using cubic splines, interaction terms, different outcome types)
# 3. Check labels
# 4. Check that numbers in model match the table_body of tbl_regression object
# 5. Check Exponentiating works
# 6. add_nevent and check that it works appropriately
# 7. add_n returns appropriate number for model
# 8. Model works with tbl_regression and tbl_uvregression

# function to pull estimates from tbl_regression object
# input gt object, output vector of coefficients
coefs_in_gt <- function(object) {
  object$table_body %>%
    dplyr::pull(estimate) %>%
    na.omit() %>%
    as.vector()
}

# Linear models using lm() -----
mod_lm <- lm(hp ~ am + disp + as.factor(cyl), # adding different variable types
             data = mtcars)

# 1. Runs as expected without errors, warnings, messages (basic usage)
test_that("lm: no errors/warnings with standard use", {
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
})

# 1. Runs as expected without errors, warnings, messages (basic usage)
test_that("survreg: no errors/warnings with standard use", {
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

# 1. Runs as expected without errors, warnings, messages (basic usage)
test_that("glm logistic: no errors/warnings with standard use", {
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
})

test_that("glm poisson: no errors/warnings with standard use", {
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
})


test_that("lmer: no errors/warnings with standard use", {
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

test_that("glmer: no errors/warnings with standard use", {
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
})

# 2. Runs as expected without errors, warnings, messages (edge cases specific to the model type, such as using cubic splines, interaction terms, different outcome types)
test_that("lm with tidyfun: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lm, tidy_fun = broom::tidy), NA)
  expect_warning(tbl_regression(mod_lm, tidy_fun = broom::tidy), NA)

})

# 3. check labels
#Test using trial data models

#Check if match trial data labels
test_that("lm with tidyfun: no errors/warnings with standard use", {
  mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
  tbl_logistic <- tbl_regression(mod_logistic)
  #Check labels match
  expect_equivalent(
    tbl_logistic$table_body$label[tbl_logistic$table_body$row_type == "label"], as.character(map(trial %>% select(age, stage), ~attr(.x,"label"))))
  })


#Update labels and check if new labels match
test_that("lm with tidyfun: no errors/warnings with standard use", {
  mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
  tbl_logistic <- tbl_regression(mod_logistic, list(stage ~ "Path Stage"))
  #Check labels match
  expect_equivalent(
    tbl_logistic$table_body$label[tbl_logistic$table_body$row_type == "label"], c("Age, yrs" ,  "Path Stage"))
})


# 5. Check Exponentiating works (for applicable models)
## Surv reg broom:tidy does not work with exponentiation so used coxph
test_that("test exponentiating for applicable models", {
  # coxph
  mod_surv <- coxph(Surv(time, status) ~ age + ph.ecog, data = lung)
  coefs <- data.frame(betas = coef(mod_surv))
  tbl_surv <- tbl_regression(mod_surv, exponentiate = TRUE)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    exp(coefs$betas),
    coefs_in_gt(tbl_surv))

  #glm logistic
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

  #glmer
  mod_glmer <- glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial)
  coefs <- data.frame(betas = fixef(mod_glmer)[-1])
  tbl_glmer <- tbl_regression(mod_glmer, exponentiate = TRUE)
  # check that tbl_regression object output matches model output
  expect_equivalent(
    exp(coefs$betas),
    coefs_in_gt(tbl_glmer))
})

# 6. add_nevent and check that it works appropriately
test_that("test add_nevent", {
  # coxph
  mod_surv <- coxph(Surv(time, status) ~ age + ph.ecog, data = lung)
  tbl_surv <- tbl_regression(mod_surv, exponentiate = TRUE) %>%
    add_nevent()
  # check that add_nevents matches output events
  expect_equivalent(
    mod_surv$nevent, tbl_surv$nevent)

  #glm logistic
  mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
  tbl_logistic <- tbl_regression(mod_logistic, exponentiate = TRUE) %>%
    add_nevent()
  # check that tbl_regression object output matches model output
  expect_equivalent(table(mod_logistic$y)[2],tbl_logistic$nevent)

  #glmer
  mod_glmer <- glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial)
  tbl_glmer <- tbl_regression(mod_glmer, exponentiate = TRUE) %>%
    add_nevent()
  # check that tbl_regression object output matches model output
  expect_equivalent(table(mod_glmer@resp$y)[2], tbl_glmer$nevent)
})
