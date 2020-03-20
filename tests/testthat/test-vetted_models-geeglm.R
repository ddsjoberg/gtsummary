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

context("test-vetted_models")
library(dplyr)

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
