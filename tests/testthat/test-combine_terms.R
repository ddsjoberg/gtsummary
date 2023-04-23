skip_on_cran()
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("Hmisc", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("geepack", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

mod1 <- lm(
  age ~ marker + I(marker^2) + stage,
  trial[c("age", "marker", "stage")] %>% na.omit()
)

# adding splines
mod2 <- lm(
  age ~ Hmisc::rcspline.eval(marker, inclx = TRUE) + stage,
  trial[c("age", "marker", "stage")] %>% na.omit()
)
mod_reduce <- lm(
  age ~ stage,
  trial[c("age", "marker", "stage")] %>% na.omit()
)

test_that("combine_terms works without error", {
  expect_error(
    tbl1 <- tbl_regression(mod1, label = stage ~ "Stage") %>%
      combine_terms(
        formula_update = . ~ . - marker - I(marker^2),
        label = "Marker (non-linear terms)"
      ),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_error(
    tbl2 <- tbl_regression(mod2, label = stage ~ "Stage") %>%
      combine_terms(
        formula_update = . ~ . - Hmisc::rcspline.eval(marker, inclx = TRUE),
        label = "Marker (non-linear terms)"
      ),
    NA
  )
  expect_snapshot(tbl2 %>% as.data.frame())

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
  expect_snapshot(
    lm(age ~ marker + I(marker^2) + stage, na.omit(trial)) %>%
      tbl_regression() %>%
      add_global_p() %>%
      combine_terms(formula = . ~ . - marker - I(marker^2)) %>%
      as.data.frame()
  )

  # Confirm logistic regression model works (test option must be specified)
  expect_snapshot(
    glm(response ~ age + marker + sp2marker + sp3marker,
      data = trial %>%
        dplyr::bind_cols(
          Hmisc::rcspline.eval(.$marker, nk = 4, inclx = FALSE, norm = 0) %>%
            as.data.frame() %>%
            stats::setNames(c("sp2marker", "sp3marker"))
        ) %>%
        filter(complete.cases(.) == TRUE),
      family = "binomial"
    ) %>%
      tbl_regression(exponentiate = TRUE) %>%
      combine_terms(
        formula_update = . ~ . - marker - sp2marker - sp3marker,
        test = "LRT"
      ) %>%
      as.data.frame()
  )

  # Confirm Cox model works
  expect_snapshot(
    survival::coxph(survival::Surv(ttdeath, death) ~ grade + Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0),
      data = na.omit(trial)
    ) %>%
      tbl_regression() %>%
      combine_terms(
        formula_update = . ~ . - Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0)
      ) %>%
      as.data.frame()
  )

  # Confirm survreg model works
  expect_snapshot(
    survival::survreg(survival::Surv(ttdeath, death) ~ grade + Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0),
      data = na.omit(trial)
    ) %>%
      tbl_regression() %>%
      combine_terms(
        formula_update = . ~ . - Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0)
      ) %>%
      as.data.frame()
  )

  # Confirm GEE model works (as long as selected terms are not the only terms in model)
  # GEE does not work for comparison with null model
  expect_snapshot(
    geepack::geeglm(
      as.formula("weight ~ Diet + Time + sp2Time + sp3Time"),
      data = ChickWeight %>%
        dplyr::bind_cols(
          Hmisc::rcspline.eval(.$Time, nk = 4, inclx = FALSE, norm = 0) %>%
            as.data.frame() %>%
            stats::setNames(c("sp2Time", "sp3Time"))
        ),
      family = gaussian,
      id = Chick,
      corstr = "exchangeable"
    ) %>%
      tbl_regression() %>%
      combine_terms(
        formula_update = . ~ . - Time - sp2Time - sp3Time
      ) %>%
      as.data.frame()
  )

  expect_message(
    tbl_regression(mod1, label = stage ~ "Stage") %>%
      combine_terms(
        formula_update = . ~ . - marker - I(marker^2),
        label = "Marker (non-linear terms)",
        quiet = TRUE
      ),
    NA
  )

  expect_message(
    tbl_regression(mod1, label = stage ~ "Stage") %>%
      combine_terms(
        formula_update = . ~ . - marker - I(marker^2),
        label = "Marker (non-linear terms)",
        quiet = FALSE
      ),
    NULL
  )
})

test_that("error catching working properly", {
  expect_error(
    lm(age ~ marker + stage, trial) %>%
      tbl_regression() %>%
      combine_terms(formula = . ~ . - marker),
    NULL
  )

  expect_error(
    lm(age ~ marker + stage, trial) %>%
      tbl_regression() %>%
      combine_terms(formula = . ~ . - marker, label = c("marker", "marker2")),
    NULL
  )

  # there is no pvalue returned by anova in this model
  expect_error(
    lm(mpg ~ disp + am * factor(cyl), data = mtcars) %>%
      tbl_regression() %>%
      combine_terms(. ~ . - am),
    NULL
  )

  expect_error(
    glm(am ~ disp + factor(cyl), data = mtcars, family = binomial) %>%
      tbl_regression() %>%
      combine_terms(. ~ . - disp),
    NULL
  )
})

# Confirm map/apply situation works
expect_error(
  tibble(outcome = "marker", exp = FALSE, test = "F") %>%
    mutate(
      mod = purrr::map(
        outcome,
        ~ glm(
          formula = paste0(.x, " ~ age + stage") %>% as.formula(),
          data = trial, family = gaussian
        )
      ),
      tbl = purrr::map2(mod, exp, ~ tbl_regression(.x, exponentiate = .y)),
      tbl2 = purrr::map2(
        tbl, test, ~ combine_terms(..1, formula_update = . ~ . - stage, test = ..2)
      )
    ),
  NA
)
