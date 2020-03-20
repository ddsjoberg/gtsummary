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

context("test-vetted_models-lmer")
library(dplyr)

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
