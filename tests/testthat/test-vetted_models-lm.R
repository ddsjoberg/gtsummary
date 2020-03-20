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

context("test-vetted_models-lm")
library(dplyr)

# lm() ------------------------------------------------------------------------
test_that("vetted_models lm()", {
  # building models to check
  mod_lm_lin <- lm(marker ~ age + trt + grade,
                     data = trial)
  mod_lm_int <- lm(marker ~ age + trt * grade,
                     data = trial)

  # 1.  Runs as expected with standard use
  #       - without errors, warnings, messages
  expect_error(
    tbl_lm_lin <- tbl_regression(mod_lm_lin,
                                  label = list(age ~ "Age, yrs",
                                               trt ~ "Chemotherapy Treatment",
                                               grade ~ "Grade")), NA
  )
  expect_warning(
    tbl_lm_lin, NA
  )
  expect_error(
    tbl_lm_int <- tbl_regression(mod_lm_int,
                                  label = list(age ~ "Age, yrs",
                                               trt ~ "Chemotherapy Treatment",
                                               grade ~ "Grade")), NA
  )
  expect_warning(
    tbl_lm_int, NA
  )
  #       - numbers in table are correct
  expect_equivalent(
    coef(mod_lm_lin)[-1],
    coefs_in_gt(tbl_lm_lin)
  )
  expect_equivalent(
    coef(mod_lm_int)[-1],
    coefs_in_gt(tbl_lm_int)
  )

  #       - labels are correct
  expect_equivalent(
    tbl_lm_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade")
  )
  expect_equivalent(
    tbl_lm_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age, yrs", "Chemotherapy Treatment", "Grade", "Chemotherapy Treatment * Grade")
  )

  # 2.  If applicable, runs as expected with logit and log link

  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equivalent(
    tbl_lm_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("Chemotherapy Treatment * Grade", "Drug B * II", "Drug B * III")
  )
  # 4.  Other gtsummary functions work with model: add_global_p(), combine_terms(), add_nevent()
  #       - without errors, warnings, messages
  # lm() does not work with car::Anova()!  update this after #409 issue complete?
  expect_error(
    tbl_lm_lin2 <- tbl_lm_lin %>% add_global_p(include = everything()), NA
  )
  expect_error(
    tbl_lm_int2 <- tbl_lm_int %>% add_global_p(include = everything()), NA
  )
  expect_warning(
    tbl_lm_lin2, NA
  )
  expect_warning(
    tbl_lm_int2, NA
  )

  #       - numbers in table are correct
  expect_equivalent(
    tbl_lm_lin2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_lm_lin, type = "III") %>%
      as.data.frame() %>%
      filter(!rownames(.) %in% c("Residuals", "(Intercept)")) %>%
      pull(`Pr(>F)`)
  )
  expect_equivalent(
    tbl_lm_int2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_lm_int, type = "III") %>%
      as.data.frame() %>%
      filter(!rownames(.) %in% c("Residuals", "(Intercept)")) %>%
      pull(`Pr(>F)`)
  )

  # 5.  tbl_uvregression() works as expected
  #       - without errors, warnings, messages
  #       - works with add_global_p(), add_nevent()
  expect_error(
    na.omit(trial) %>%
      tbl_uvregression(
        y = response,
        method = lm
      ),
    NA
  )
  expect_warning(
    na.omit(trial) %>%
      tbl_uvregression(
        y = response,
        method = lm
      ),
    NA
  )
})
