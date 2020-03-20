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


context("test-vetted_models-glm")
library(dplyr)

# glm() ------------------------------------------------------------------------
test_that("vetted_models glm()", {
  # building models to check
  mod_glm_lin <- glm(marker ~ age + trt + grade,
                     data = trial)
  mod_glm_int <- glm(marker ~ age + trt * grade,
                     data = trial)
  mod_glm_log <- glm(response ~ age + trt + grade,
                     data = trial, family = binomial)
  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(
    tbl_glm_lin <- tbl_regression(mod_glm_lin,
                                  label = list(age ~ "Age, yrs",
                                               trt ~ "Chemotherapy Treatment",
                                               grade ~ "Grade")), NA
  )
  expect_warning(
    tbl_glm_lin, NA
  )
  expect_error(
    tbl_glm_int <- tbl_regression(mod_glm_int,
                                  label = list(age ~ "Age, yrs",
                                               trt ~ "Chemotherapy Treatment",
                                               grade ~ "Grade")), NA
  )
  expect_warning(
    tbl_glm_int, NA
  )
  expect_error(
    tbl_glm_log <- tbl_regression(mod_glm_log,
                                  label = list(age ~ "Age, yrs",
                                               trt ~ "Chemotherapy Treatment",
                                               grade ~ "Grade")), NA
  )
  expect_warning(
    tbl_glm_log, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_glm_lin)[-1],
    coefs_in_gt(tbl_glm_lin)
  )
  expect_equivalent(
    coef(mod_glm_int)[-1],
    coefs_in_gt(tbl_glm_int)
  )
  expect_equivalent(
    coef(mod_glm_log)[-1],
    coefs_in_gt(tbl_glm_log)
  )

  #       - labels are correct
  expect_equivalent(
    tbl_glm_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade")
  )
  expect_equivalent(
    tbl_glm_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade", "Chemotherapy Treatment * Grade")
  )
  expect_equivalent(
    tbl_glm_log$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade")
  )
  # 2.  If applicable, runs as expected with logit and log link
  expect_equivalent(
    coef(mod_glm_log)[-1] %>% exp(),
    coefs_in_gt(mod_glm_log %>% tbl_regression(exponentiate = TRUE))
  )

  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equivalent(
    tbl_glm_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("Chemotherapy Treatment * Grade", "Drug B * II", "Drug B * III")
  )
  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms(), add_nevent()
  #       - without errors, warnings, messages
  expect_error(
    tbl_glm_lin2 <- tbl_glm_lin %>% add_global_p(include = everything()), NA
  )
  expect_error(
    tbl_glm_int2 <- tbl_glm_int %>% add_global_p(include = everything()), NA
  )
  expect_error(
    tbl_glm_log2 <- tbl_glm_log %>% add_global_p(include = everything()), NA
  )
  expect_warning(
    tbl_glm_lin2, NA
  )
  expect_warning(
    tbl_glm_int2, NA
  )
  expect_warning(
    tbl_glm_log2, NA
  )
  expect_error(
    tbl_glm_log3 <- tbl_glm_log %>% combine_terms(. ~ . - trt, test = "LRT"), NA
  )
  expect_warning(
    tbl_glm_log3, NA
  )
  expect_error(
    tbl_glm_log4 <- tbl_glm_lin %>% add_nevent(), "*"
  )
  #       - numbers in table are correct
  expect_equivalent(
    tbl_glm_lin2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_glm_lin, type = "III") %>%
      as.data.frame() %>%
      pull(`Pr(>Chisq)`)
  )
  expect_equivalent(
    tbl_glm_int2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_glm_int, type = "III") %>%
      as.data.frame() %>%
      pull(`Pr(>Chisq)`)
  )
  expect_equivalent(
    tbl_glm_log3$table_body %>% filter(variable == "trt") %>% pull(p.value),
    update(mod_glm_log, formula. = . ~ . - trt) %>%
      {anova(mod_glm_log, ., test = "LRT")} %>%
      as.data.frame() %>%
      slice(n()) %>%
      pull(`Pr(>Chi)`)
  )
  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  #       - works with add_global_p(), add_nevent()
  expect_error(
    na.omit(trial) %>%
      tbl_uvregression(
        y = response,
        method = glm,
        method.args = list(family = binomial),
      ),
    NA
  )
  expect_warning(
    na.omit(trial) %>%
      tbl_uvregression(
        y = response,
        method = glm,
        method.args = list(family = binomial),
      ),
    NA
  )
})

