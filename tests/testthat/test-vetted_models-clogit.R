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

skip_on_cran()
# vetted models checks take a long time--only perform on CI checks
skip_if(!isTRUE(as.logical(Sys.getenv("CI"))))
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))
library(dplyr)
library(survival)

# clogit() ---------------------------------------------------------------------
test_that("vetted_models clogit()", {
  # building models to check
  mod_clogit_lin <- clogit(response ~ age + trt + grade + strata(stage),
    data = trial
  )
  mod_clogit_int <- clogit(response ~ age + trt * grade + strata(stage),
    data = trial
  )
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
  expect_equal(
    coef(mod_clogit_lin),
    coefs_in_gt(tbl_clogit_lin),
    ignore_attr = TRUE
  )
  expect_equal(
    coef(mod_clogit_int),
    coefs_in_gt(tbl_clogit_int),
    ignore_attr = TRUE
  )

  #       - labels are correct
  expect_equal(
    tbl_clogit_lin$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age", "Chemotherapy Treatment", "Grade"),
    ignore_attr = TRUE
  )
  expect_equal(
    tbl_clogit_int$table_body %>%
      filter(row_type == "label") %>%
      pull(label),
    c("Age", "Chemotherapy Treatment", "Grade", "Chemotherapy Treatment * Grade"),
    ignore_attr = TRUE
  )
  # 2.  If applicable, runs as expected with logit and log link
  expect_equal(
    coef(mod_clogit_lin) %>% exp(),
    coefs_in_gt(mod_clogit_lin %>% tbl_regression(exponentiate = TRUE)),
    ignore_attr = TRUE
  )

  # 3.  Interaction terms are correctly printed in output table
  #       - interaction labels are correct
  expect_equal(
    tbl_clogit_int$table_body %>%
      filter(var_type == "interaction") %>%
      pull(label),
    c("Chemotherapy Treatment * Grade", "Drug B * II", "Drug B * III"),
    ignore_attr = TRUE
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
    tbl_clogit_lin4 <- tbl_clogit_lin %>% add_nevent(), NA
  )
  #       - numbers in table are correct
  # clogit models fail in car::Anova on old versions
  if (r_version >= "3.5.0") {
    expect_equal(
      tbl_clogit_lin2$table_body %>%
        pull(p.value) %>%
        na.omit() %>%
        as.vector(),
      car::Anova(mod_clogit_lin, type = "III", test = "Wald") %>%
        as.data.frame() %>%
        pull(`Pr(>Chisq)`)
    )
    expect_equal(
      tbl_clogit_int2$table_body %>%
        pull(p.value) %>%
        na.omit() %>%
        as.vector(),
      car::Anova(mod_clogit_int, type = "III", test = "Wald") %>%
        as.data.frame() %>%
        pull(`Pr(>Chisq)`)
    )
    # anova() and car::Anova() do not match
    # expect_equal(
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
