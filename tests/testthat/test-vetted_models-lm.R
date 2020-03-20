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
