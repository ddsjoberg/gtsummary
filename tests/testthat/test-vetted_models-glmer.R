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

context("test-vetted_models-glmer")
library(dplyr)

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
