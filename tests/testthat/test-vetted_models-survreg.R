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

skip_on_cran()
# vetted models checks take a long time--only perform on CI checks
skip_if(!isTRUE(as.logical(Sys.getenv("CI"))))
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))
library(dplyr)
library(survival)


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
  expect_equal(
    coef(mod_survreg_lin)[-1],
    coefs_in_gt(tbl_survreg_lin),
    ignore_attr = TRUE
  )
  expect_equal(
    coef(mod_survreg_int)[-1],
    coefs_in_gt(tbl_survreg_int),
    ignore_attr = TRUE
  )
  expect_equal(
    coef(mod_survreg_lin),
    coefs_in_gt(tbl_regression(mod_survreg_lin, intercept = TRUE)),
    ignore_attr = TRUE
  )
  expect_equal(
    coef(mod_survreg_int),
    coefs_in_gt(tbl_regression(mod_survreg_int, intercept = TRUE)),
    ignore_attr = TRUE
  )
  #       - labels are correct
  expect_equal(
    tbl_survreg_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age", "Chemotherapy Treatment", "Grade"),
    ignore_attr = TRUE
  )
  expect_equal(
    tbl_survreg_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age", "Chemotherapy Treatment", "Grade", "Chemotherapy Treatment * Grade"),
    ignore_attr = TRUE
  )
  # 2.  If applicable, runs as expected with logit and log link (NOT APPLICABLE)
  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equal(
    tbl_survreg_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("Chemotherapy Treatment * Grade", "Drug B * II", "Drug B * III"),
    ignore_attr = TRUE
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
  expect_equal(
    tbl_survreg_lin2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_survreg_lin, type = "III") %>%
      as.data.frame() %>%
      pull(`Pr(>Chisq)`),
    ignore_attr = TRUE
  )
  expect_equal(
    tbl_survreg_int2$table_body %>%
      pull(p.value) %>%
      na.omit() %>%
      as.vector(),
    car::Anova(mod_survreg_int, type = "III") %>%
      as.data.frame() %>%
      pull(`Pr(>Chisq)`),
    ignore_attr = TRUE
  )
  expect_equal(
    tbl_survreg_lin3$table_body %>% filter(variable == "trt") %>% pull(p.value),
    car::Anova(mod_survreg_lin, type = "III") %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      filter(rowname == "trt") %>%
      pull(`Pr(>Chisq)`),
    ignore_attr = TRUE
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
})
