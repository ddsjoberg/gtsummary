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
#       - works with add_global_p(), add_nevent(), add_q()

# stats::lm()           DONE
# stats::glm()          DONE
# survival::survreg()   DONE
# survival::coxph()     DONE
# lme4::lmer()          DONE
# lme4::glmer()         DONE
# geepack::geeglm()     DONE
# survival::clogit()    DONE
# glmnet::glmnet()

context("test-vetted_models")
library(dplyr)
library(survival)
set.seed(23433)
r_version <- paste0(R.Version()$major, ".", R.Version()$minor)

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
  #       - works with add_global_p(), add_q()
  expect_error(
    tbl_lmuv <- tbl_uvregression(
      trial,
      y = age,
      method = lm
    ) %>%
      add_global_p() %>%
      add_q(),
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
  #       - works with add_global_p(), add_nevent(), add_q()
  expect_error(
    tbl_glmuv <- tbl_uvregression(
      trial,
      y = response,
      method = glm,
      method.args = list(family = binomial)
    ) %>%
      add_global_p() %>%
      add_nevent() %>%
      add_q(),
    NA
  )
})


# survreg() --------------------------------------------------------------------
test_that("vetted_models survreg()", {
  # building models to check
  mod_survreg_lin <- survreg(Surv(ttdeath, death) ~ age + trt + grade, data = trial)
  mod_survreg_int <- survreg(Surv(ttdeath, death) ~ age + trt * grade, data = trial)
  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(
    tbl_survreg_lin <- tbl_regression(mod_survreg_lin), NA
  )
  expect_warning(
    tbl_survreg_lin, NA
  )
  expect_error(
    tbl_survreg_int <- tbl_regression(mod_survreg_int), NA
  )
  expect_warning(
    tbl_survreg_int, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_survreg_lin)[-1],
    coefs_in_gt(tbl_survreg_lin)
  )
  expect_equivalent(
    coef(mod_survreg_int)[-1],
    coefs_in_gt(tbl_survreg_int)
  )
  expect_equivalent(
    coef(mod_survreg_lin),
    coefs_in_gt(tbl_regression(mod_survreg_lin, intercept = TRUE))
  )
  expect_equivalent(
    coef(mod_survreg_int),
    coefs_in_gt(tbl_regression(mod_survreg_int, intercept = TRUE))
  )
  #       - labels are correct
  expect_equivalent(
    tbl_survreg_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade")
  )
  expect_equivalent(
    tbl_survreg_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade", "Chemotherapy Treatment * Grade")
  )
  # 2.  If applicable, runs as expected with logit and log link (NOT APPLICABLE)
  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equivalent(
    tbl_survreg_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("Chemotherapy Treatment * Grade", "Drug B * II", "Drug B * III")
  )
  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms()
  #       - without errors, warnings, messages
  expect_error(
    tbl_survreg_lin2 <- tbl_survreg_lin %>% add_global_p(include = everything()), NA
  )
  expect_error(
    tbl_survreg_int2 <- tbl_survreg_int %>% add_global_p(include = everything()), NA
  )
  expect_warning(
    tbl_survreg_lin2, NA
  )
  expect_warning(
    tbl_survreg_int2, NA
  )
  expect_error(
    tbl_survreg_lin3 <- tbl_survreg_lin %>% combine_terms(. ~ . - trt), NA
  )
  expect_warning(
    tbl_survreg_lin3, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    tbl_survreg_lin2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_survreg_lin, type = "III") %>%
      as.data.frame() %>%
      pull(`Pr(>Chisq)`)
  )
  expect_equivalent(
    tbl_survreg_int2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_survreg_int, type = "III") %>%
      as.data.frame() %>%
      pull(`Pr(>Chisq)`)
  )
  expect_equivalent(
    tbl_survreg_lin3$table_body %>% filter(variable == "trt") %>% pull(p.value),
    car::Anova(mod_survreg_lin, type = "III") %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      filter(rowname == "trt") %>%
      pull(`Pr(>Chisq)`)
  )
  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  #       - works with add_global_p(), add_nevent()
  expect_error(
    trial %>%
      tbl_uvregression(
        y = Surv(ttdeath, death),
        method = survreg
      ) %>%
      add_global_p() %>%
      add_q(),
    NA
  )
  expect_warning(
    trial %>%
      tbl_uvregression(
        y = Surv(ttdeath, death),
        method = survreg
      ) %>%
      add_global_p() %>%
      add_q(),
    NA
  )
})

# coxph() ----------------------------------------------------------------------
test_that("vetted_models coxph()", {
  # building models to check
  mod_coxph_lin <- coxph(Surv(ttdeath, death) ~ age + trt + grade, data = trial)
  mod_coxph_int <- coxph(Surv(ttdeath, death) ~ age + trt * grade, data = trial)
  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(
    tbl_coxph_lin <- tbl_regression(mod_coxph_lin), NA
  )
  expect_warning(
    tbl_coxph_lin, NA
  )
  expect_error(
    tbl_coxph_int <- tbl_regression(mod_coxph_int), NA
  )
  expect_warning(
    tbl_coxph_int, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_coxph_lin),
    coefs_in_gt(tbl_coxph_lin)
  )
  expect_equivalent(
    coef(mod_coxph_int),
    coefs_in_gt(tbl_coxph_int)
  )

  #       - labels are correct
  expect_equivalent(
    tbl_coxph_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade")
  )
  expect_equivalent(
    tbl_coxph_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade", "Chemotherapy Treatment * Grade")
  )
  # 2.  If applicable, runs as expected with logit and log link
  expect_equivalent(
    coef(mod_coxph_lin) %>% exp(),
    coefs_in_gt(mod_coxph_lin %>% tbl_regression(exponentiate = TRUE))
  )

  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equivalent(
    tbl_coxph_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("Chemotherapy Treatment * Grade", "Drug B * II", "Drug B * III")
  )
  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms(), add_nevent()
  #       - without errors, warnings, messages
  expect_error(
    tbl_coxph_lin2 <- tbl_coxph_lin %>% add_global_p(include = everything()), NA
  )
  expect_error(
    tbl_coxph_int2 <- tbl_coxph_int %>% add_global_p(include = everything()), NA
  )
  expect_warning(
    tbl_coxph_lin2, NA
  )
  expect_warning(
    tbl_coxph_int2, NA
  )
  expect_error(
    tbl_coxph_lin3 <- tbl_coxph_lin %>% combine_terms(. ~ . - trt), NA
  )
  expect_warning(
    tbl_coxph_lin3, NA
  )
  expect_error(
    tbl_coxph_lin4 <- tbl_coxph_lin %>% add_nevent(), NA
  )
  expect_warning(
    tbl_coxph_lin4, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    tbl_coxph_lin2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_coxph_lin, type = "III") %>%
      as.data.frame() %>%
      pull(`Pr(>Chisq)`)
  )
  expect_equivalent(
    tbl_coxph_int2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_coxph_int, type = "III") %>%
      as.data.frame() %>%
      pull(`Pr(>Chisq)`)
  )
  expect_equivalent(
    tbl_coxph_lin3$table_body %>% filter(variable == "trt") %>% pull(p.value),
    car::Anova(mod_coxph_lin, type = "III") %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      filter(rowname == "trt") %>%
      pull(`Pr(>Chisq)`)
  )
  expect_equivalent(
    trial %>% select(death, age, trt, grade) %>% na.omit() %>% pull(death) %>% sum(),
    tbl_coxph_lin4$table_body %>% slice(1) %>% pull(nevent)
  )
  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  #       - works with add_global_p(), add_nevent()
  expect_error(
    trial %>%
      tbl_uvregression(
        y = Surv(ttdeath, death),
        method = coxph
      ) %>%
      add_global_p() %>%
      add_q(),
    NA
  )
  expect_warning(
    trial %>%
      tbl_uvregression(
        y = Surv(ttdeath, death),
        method = coxph
      ) %>%
      add_nevent() %>%
      add_global_p() %>%
      add_q(),
    NA
  )
})

# lmer() -----------------------------------------------------------------------
test_that("vetted_models lmer()", {
  # building models to check
  mod_lmer_lin <- lme4::lmer(marker ~ age + trt + grade + (1 | response), data = trial)
  mod_lmer_int <- lme4::lmer(marker ~ age + trt * grade + (1 | response), data = trial)
  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(
    tbl_lmer_lin <- tbl_regression(mod_lmer_lin), NA
  )
  expect_warning(
    tbl_lmer_lin, NA
  )
  expect_error(
    tbl_lmer_int <- tbl_regression(mod_lmer_int), NA
  )
  expect_warning(
    tbl_lmer_int, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    summary(mod_lmer_lin)$coefficients[-1, 1],
    coefs_in_gt(tbl_lmer_lin)
  )
  expect_equivalent(
    summary(mod_lmer_int)$coefficients[-1, 1],
    coefs_in_gt(tbl_lmer_int)
  )
  expect_equivalent(
    summary(mod_lmer_lin)$coefficients[, 1],
    coefs_in_gt(tbl_regression(mod_lmer_lin, intercept = TRUE))
  )
  expect_equivalent(
    summary(mod_lmer_int)$coefficients[, 1],
    coefs_in_gt(tbl_regression(mod_lmer_int, intercept = TRUE))
  )
  #       - labels are correct
  expect_equivalent(
    tbl_lmer_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "trt", "Grade")
  )
  expect_equivalent(
    tbl_lmer_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "trt", "Grade", "trt * Grade")
  )
  # 2.  If applicable, runs as expected with logit and log link (NOT APPLICABLE)
  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equivalent(
    tbl_lmer_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("trt * Grade", "Drug B * II", "Drug B * III")
  )
  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms()
  #       - without errors, warnings, messages
  expect_error(
    tbl_lmer_lin2 <- tbl_lmer_lin %>% add_global_p(include = everything()), NA
  )
  expect_error(
    tbl_lmer_int2 <- tbl_lmer_int %>% add_global_p(include = everything()), NA
  )
  expect_warning(
    tbl_lmer_lin2, NA
  )
  expect_warning(
    tbl_lmer_int2, NA
  )
  expect_error(
    tbl_lmer_lin3 <- tbl_lmer_lin %>% combine_terms(. ~ . - trt), NA
  )
  expect_warning(
    tbl_lmer_lin3, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    tbl_lmer_lin2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_lmer_lin, type = "III") %>%
      as.data.frame() %>%
      slice(-1) %>%
      pull(`Pr(>Chisq)`)
  )
  expect_equivalent(
    tbl_lmer_int2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_lmer_int, type = "III") %>%
      as.data.frame() %>%
      slice(-1) %>%
      pull(`Pr(>Chisq)`)
  )
  # See Issue #406
  # expect_equivalent(
  #   tbl_lmer_lin3$table_body %>% filter(variable == "trt") %>% pull(p.value),
  #   car::Anova(mod_lmer_lin, type = "III") %>%
  #     as.data.frame() %>%
  #     tibble::rownames_to_column() %>%
  #     filter(rowname == "trt") %>%
  #     pull(`Pr(>Chisq)`)
  # )
  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  #       - works with add_global_p(), add_nevent(), add_q()
  expect_error(
    trial %>%
      tbl_uvregression(
        y = marker,
        method = lme4::lmer,
        formula = "{y} ~ {x} + (1 | response)"
      ) %>%
      add_global_p() %>%
      add_q(),
    NA
  )
  expect_warning(
    trial %>%
      tbl_uvregression(
        y = marker,
        method = lme4::lmer,
        formula = "{y} ~ {x} + (1 | response)"
      ) %>%
      add_global_p() %>%
      add_q(),
    NA
  )
})

# glmer() --------------------------------------------------------------------
test_that("vetted_models glmer()", {
  # building models to check
  mod_glmer_lin <- lme4::glmer(response ~ age + trt + grade + (1 | death),
                               data = trial, family = binomial)
  mod_glmer_int <- lme4::glmer(response ~ age + trt * grade + (1 | death),
                               data = trial, family = binomial)
  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(
    tbl_glmer_lin <- tbl_regression(mod_glmer_lin), NA
  )
  expect_warning(
    tbl_glmer_lin, NA
  )
  expect_error(
    tbl_glmer_int <- tbl_regression(mod_glmer_int), NA
  )
  expect_warning(
    tbl_glmer_int, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    summary(mod_glmer_lin)$coefficients[-1, 1],
    coefs_in_gt(tbl_glmer_lin)
  )
  expect_equivalent(
    summary(mod_glmer_int)$coefficients[-1, 1],
    coefs_in_gt(tbl_glmer_int)
  )
  expect_equivalent(
    summary(mod_glmer_lin)$coefficients[, 1],
    coefs_in_gt(tbl_regression(mod_glmer_lin, intercept = TRUE))
  )
  expect_equivalent(
    summary(mod_glmer_int)$coefficients[, 1],
    coefs_in_gt(tbl_regression(mod_glmer_int, intercept = TRUE))
  )
  #       - labels are correct
  expect_equivalent(
    tbl_glmer_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "trt", "Grade")
  )
  expect_equivalent(
    tbl_glmer_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "trt", "Grade", "trt * Grade")
  )
  # 2.  If applicable, runs as expected with logit and log link
  expect_equivalent(
    summary(mod_glmer_lin)$coefficients[-1, 1] %>% exp(),
    coefs_in_gt(mod_glmer_lin %>% tbl_regression(exponentiate = TRUE))
  )

  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equivalent(
    tbl_glmer_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("trt * Grade", "Drug B * II", "Drug B * III")
  )
  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms()
  #       - without errors, warnings, messages
  expect_error(
    tbl_glmer_lin2 <- tbl_glmer_lin %>% add_global_p(include = everything()), NA
  )
  expect_error(
    tbl_glmer_int2 <- tbl_glmer_int %>% add_global_p(include = everything()), NA
  )
  expect_warning(
    tbl_glmer_lin2, NA
  )
  expect_warning(
    tbl_glmer_int2, NA
  )
  expect_error(
    tbl_glmer_lin3 <- tbl_glmer_lin %>% combine_terms(. ~ . - trt), NA
  )
  expect_warning(
    tbl_glmer_lin3, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    tbl_glmer_lin2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_glmer_lin, type = "III") %>%
      as.data.frame() %>%
      slice(-1) %>%
      pull(`Pr(>Chisq)`)
  )
  expect_equivalent(
    tbl_glmer_int2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_glmer_int, type = "III") %>%
      as.data.frame() %>%
      slice(-1) %>%
      pull(`Pr(>Chisq)`)
  )
  # See Issue #406
  # expect_equivalent(
  #   tbl_glmer_lin3$table_body %>% filter(variable == "trt") %>% pull(p.value),
  #   car::Anova(mod_glmer_lin, type = "III") %>%
  #     as.data.frame() %>%
  #     tibble::rownames_to_column() %>%
  #     filter(rowname == "trt") %>%
  #     pull(`Pr(>Chisq)`)
  # )
  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  #       - works with add_global_p(), add_nevent(), add_q()
  expect_error(
    trial %>%
      tbl_uvregression(
        y = response,
        method = lme4::glmer,
        formula = "{y} ~ {x} + (1 | death)",
        method.args = list(family = binomial)
      ) %>%
      add_global_p() %>%
      add_q(),
    NA
  )
  expect_warning(
    trial %>%
      tbl_uvregression(
        y = response,
        method = lme4::glmer,
        formula = "{y} ~ {x} + (1 | death)",
        method.args = list(family = binomial)
      ) %>%
      add_global_p() %>%
      add_q(),
    NA
  )
})

# geeglm() --------------------------------------------------------------------
test_that("vetted_models geeglm()", {
  # building models to check
  mod_geeglm_lin <- geepack::geeglm(marker ~ age + trt + grade,
                                    data = na.omit(trial), id = death)
  mod_geeglm_int <- geepack::geeglm(marker ~ age + trt * grade,
                                    data = na.omit(trial), id = death)
  mod_geeglm_log <- geepack::geeglm(response ~ age + trt + grade,
                                    data = na.omit(trial), family = binomial,
                                    id = death)
  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(
    tbl_geeglm_lin <- tbl_regression(mod_geeglm_lin,
                                     label = list(age ~ "Age, yrs",
                                                  trt ~ "Chemotherapy Treatment",
                                                  grade ~ "Grade")), NA
  )
  expect_warning(
    tbl_geeglm_lin, NA
  )
  expect_error(
    tbl_geeglm_int <- tbl_regression(mod_geeglm_int,
                                     label = list(age ~ "Age, yrs",
                                                  trt ~ "Chemotherapy Treatment",
                                                  grade ~ "Grade")), NA
  )
  expect_warning(
    tbl_geeglm_int, NA
  )
  expect_error(
    tbl_geeglm_log <- tbl_regression(mod_geeglm_log,
                                     label = list(age ~ "Age, yrs",
                                                  trt ~ "Chemotherapy Treatment",
                                                  grade ~ "Grade")), NA
  )
  expect_warning(
    tbl_geeglm_log, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_geeglm_lin)[-1],
    coefs_in_gt(tbl_geeglm_lin)
  )
  expect_equivalent(
    coef(mod_geeglm_int)[-1],
    coefs_in_gt(tbl_geeglm_int)
  )
  expect_equivalent(
    coef(mod_geeglm_log)[-1],
    coefs_in_gt(tbl_geeglm_log)
  )

  #       - labels are correct
  expect_equivalent(
    tbl_geeglm_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade")
  )
  expect_equivalent(
    tbl_geeglm_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade", "Chemotherapy Treatment * Grade")
  )
  expect_equivalent(
    tbl_geeglm_log$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade")
  )
  # 2.  If applicable, runs as expected with logit and log link
  expect_equivalent(
    coef(mod_geeglm_log)[-1] %>% exp(),
    coefs_in_gt(mod_geeglm_log %>% tbl_regression(exponentiate = TRUE))
  )

  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equivalent(
    tbl_geeglm_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("Chemotherapy Treatment * Grade", "Drug B * II", "Drug B * III")
  )
  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms(), add_nevent()
  #       - without errors, warnings, messages
  # geeglm() does not work with car::Anova()!  update this after #409 issue complete?
  expect_error(
    tbl_geeglm_lin2 <- tbl_geeglm_lin %>% add_global_p(include = everything()), "*"
  )
  # expect_error(
  #   tbl_geeglm_int2 <- tbl_geeglm_int %>% add_global_p(include = everything()), NA
  # )
  # expect_error(
  #   tbl_geeglm_log2 <- tbl_geeglm_log %>% add_global_p(include = everything()), NA
  # )
  # expect_warning(
  #   tbl_geeglm_lin2, NA
  # )
  # expect_warning(
  #   tbl_geeglm_int2, NA
  # )
  # expect_warning(
  #   tbl_geeglm_log2, NA
  # )
  expect_error(
    tbl_geeglm_log3 <- tbl_geeglm_log %>% combine_terms(. ~ . - trt), NA
  )
  expect_warning(
    tbl_geeglm_log3, NA
  )
  expect_error(
    tbl_geeglm_log4 <- tbl_geeglm_lin %>% add_nevent(), "*"
  )
  #       - numbers in table are correct
  # expect_equivalent(
  #   tbl_geeglm_lin2$table_body %>%
  #     pull(p.value) %>%
  #     na.omit() %>%
  #     as.vector(),
  #   car::Anova(mod_geeglm_lin, type = "III") %>%
  #     as.data.frame() %>%
  #     pull(`Pr(>Chisq)`)
  # )
  # expect_equivalent(
  #   tbl_geeglm_int2$table_body %>%
  #     pull(p.value) %>%
  #     na.omit() %>%
  #     as.vector(),
  #   car::Anova(mod_geeglm_int, type = "III") %>%
  #     as.data.frame() %>%
  #     pull(`Pr(>Chisq)`)
  # )
  expect_equivalent(
    tbl_geeglm_log3$table_body %>% filter(variable == "trt") %>% pull(p.value),
    update(mod_geeglm_log, formula. = . ~ . - trt) %>%
      {anova(mod_geeglm_log, .)} %>%
      as.data.frame() %>%
      pull(`P(>|Chi|)`)
  )
  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  #       - works with add_global_p(), add_nevent()
  expect_error(
    na.omit(trial) %>%
      tbl_uvregression(
        y = response,
        method = geepack::geeglm,
        method.args = list(family = binomial,
                           id = death),
        include = -death
      ),
    NA
  )
  expect_warning(
    na.omit(trial) %>%
      tbl_uvregression(
        y = response,
        method = geepack::geeglm,
        method.args = list(family = binomial,
                           id = death),
        include = -death
      ),
    NA
  )
})

# clogit() ---------------------------------------------------------------------
test_that("vetted_models clogit()", {
  # building models to check
  mod_clogit_lin <- clogit(response ~ age + trt + grade + strata(stage),
                           data = trial)
  mod_clogit_int <- clogit(response ~ age + trt * grade + strata(stage),
                           data = trial)
  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(
    tbl_clogit_lin <- tbl_regression(mod_clogit_lin), NA
  )
  expect_warning(
    tbl_clogit_lin, NA
  )
  expect_error(
    tbl_clogit_int <- tbl_regression(mod_clogit_int), NA
  )
  expect_warning(
    tbl_clogit_int, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_clogit_lin),
    coefs_in_gt(tbl_clogit_lin)
  )
  expect_equivalent(
    coef(mod_clogit_int),
    coefs_in_gt(tbl_clogit_int)
  )

  #       - labels are correct
  expect_equivalent(
    tbl_clogit_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade")
  )
  expect_equivalent(
    tbl_clogit_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade", "Chemotherapy Treatment * Grade")
  )
  # 2.  If applicable, runs as expected with logit and log link
  expect_equivalent(
    coef(mod_clogit_lin) %>% exp(),
    coefs_in_gt(mod_clogit_lin %>% tbl_regression(exponentiate = TRUE))
  )

  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equivalent(
    tbl_clogit_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("Chemotherapy Treatment * Grade", "Drug B * II", "Drug B * III")
  )
  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms(), add_nevent()
  #       - without errors, warnings, messages
  # clogit models fail in car::Anova on old versions
  if (r_version >= "3.5.0") {
    expect_error(
      tbl_clogit_lin2 <- tbl_clogit_lin %>% add_global_p(include = everything(), test = "Wald"), NA
    )
    expect_error(
      tbl_clogit_int2 <- tbl_clogit_int %>% add_global_p(include = everything(), test = "Wald"), NA
    )
    expect_warning(
      tbl_clogit_lin2, NA
    )
    expect_warning(
      tbl_clogit_int2, NA
    )
    expect_error(
      tbl_clogit_lin3 <- tbl_clogit_lin %>% combine_terms(. ~ . - trt, test = "Wald"), NA
    )
    expect_warning(
      tbl_clogit_lin3, NA
    )
  }

  expect_error(
    tbl_clogit_lin4 <- tbl_clogit_lin %>% add_nevent(), "*"
  )
  #       - numbers in table are correct
  # clogit models fail in car::Anova on old versions
  if (r_version >= "3.5.0") {
    expect_equivalent(
      tbl_clogit_lin2$table_body %>%
        pull(p.value) %>%
        na.omit() %>%
        as.vector(),
      car::Anova(mod_clogit_lin, type = "III", test = "Wald") %>%
        as.data.frame() %>%
        pull(`Pr(>Chisq)`)
    )
    expect_equivalent(
      tbl_clogit_int2$table_body %>%
        pull(p.value) %>%
        na.omit() %>%
        as.vector(),
      car::Anova(mod_clogit_int, type = "III", test = "Wald") %>%
        as.data.frame() %>%
        pull(`Pr(>Chisq)`)
    )
    # anova() and car::Anova() do not match
    # expect_equivalent(
    #   tbl_clogit_lin3$table_body %>% filter(variable == "trt") %>% pull(p.value),
    #   car::Anova(mod_clogit_lin, type = "III", test = "Wald") %>%
    #     as.data.frame() %>%
    #     tibble::rownames_to_column() %>%
    #     filter(rowname == "trt") %>%
    #     pull(`Pr(>Chisq)`)
    # )
  }

  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  #       - works with add_global_p(), add_nevent()
  expect_error(
    trial %>%
      tbl_uvregression(
        y = response,
        method = clogit,
        formula = "{y} ~ {x} + strata(stage)"
      ) %>%
      add_global_p(test = "Wald") %>%
      add_q(),
    NA
  )
  expect_warning(
    trial %>%
      tbl_uvregression(
        y = response,
        method = clogit,
        formula = "{y} ~ {x} + strata(stage)"
      ) %>%
      add_global_p(test = "Wald") %>%
      add_q(),
    NA
  )
})
