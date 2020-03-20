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
