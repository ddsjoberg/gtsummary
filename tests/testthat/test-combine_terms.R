context("test-combine_terms")
library(Hmisc)
mod1 <- lm(age ~ marker + I(marker^2) + stage,
           trial[c("age", "marker", "stage")] %>% na.omit())

# adding splines
mod2 <- lm(age ~ rcspline.eval(marker, inclx = TRUE) + stage,
           trial[c("age", "marker", "stage")] %>% na.omit())
mod_reduce <- lm(age ~ stage,
                 trial[c("age", "marker", "stage")] %>% na.omit())

test_that("combine_terms works without error", {
  expect_error(
    tbl1 <- tbl_regression(mod1, label = stage ~ "Stage") %>%
      combine_terms(formula_update = . ~ . -marker -I(marker^2),
                    label = "Marker (non-linear terms)"),
    NA
  )

  expect_error(
    tbl2 <- tbl_regression(mod2, label = stage ~ "Stage") %>%
      combine_terms(formula_update = . ~ . -rcspline.eval(marker, inclx = TRUE),
                    label = "Marker (non-linear terms)"),
    NA
  )

  # testing anova p-value is correct
  expect_equal(
    tbl1$table_body %>% dplyr::slice(1) %>% dplyr::pull(p.value),
    anova(tbl1$model_obj, mod_reduce) %>% as_tibble() %>% dplyr::slice(dplyr::n()) %>% dplyr::pull(`Pr(>F)`)
  )
  expect_equal(
    tbl2$table_body %>% dplyr::slice(1) %>% dplyr::pull(p.value),
    anova(tbl2$model_obj, mod_reduce) %>% as_tibble() %>% dplyr::slice(dplyr::n()) %>% dplyr::pull(`Pr(>F)`)
  )

  # testing nrows in output is correct
  expect_equal(
    nrow(tbl1$table_body), 6
  )
  expect_equal(
    nrow(tbl2$table_body), 6
  )

  # works after add_global_p()
  expect_error(
    lm(age ~ marker + I(marker^2) + stage, na.omit(trial)) %>%
      tbl_regression() %>%
      add_global_p() %>%
      combine_terms(formula = . ~ . -marker - I(marker^2)),
    "*"
  )
})

test_that("error catching working properly", {
  expect_error(
    lm(age ~ marker + stage, trial) %>%
      tbl_regression() %>%
      combine_terms(formula = . ~ . -marker),
    "*"
  )

  expect_error(
    lm(age ~ marker + stage, trial) %>%
      tbl_regression() %>%
      combine_terms(formula = . ~ . -marker, label = c("marker", "marker2")),
    "*"
  )
})

