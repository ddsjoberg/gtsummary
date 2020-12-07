context("test-add_difference")
testthat::skip_on_cran()

test_that("add_difference-basic use", {
  expect_error(
    tbl_diff <-
      trial %>%
      select(trt, marker, age) %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_difference(
        test = everything() ~ "t.test",
        test.args = all_tests("t.test") ~ list(var.equal = TRUE)
      ),
    NA
  )

  expect_equal(
    dplyr::filter(tbl_diff$table_body, variable == "marker") %>% select(estimate, conf.low, conf.high, p.value),
    t.test(marker ~ trt, trial, var.equal = TRUE) %>% broom::tidy() %>% select(estimate, conf.low, conf.high, p.value)
  )

})

test_that("p-values are replicated within tbl_summary()", {
  tbl_test.args <-
    trial %>%
    select(trt,
           var_t.test = age,
           var_t.test_dots = age,
           var_wilcox.test = age,
           var_wilcox.test_dots = age,
           var_prop.test = response,
           var_prop.test_dots = response,
           var_ancova = age,
           var_cohens_d = age
    ) %>%
    tbl_summary(by = trt, missing = "no") %>%
    add_difference(
      test = list(contains("t.test") ~ t.test,
                  contains("wilcox.test") ~ wilcox.test,
                  contains("prop.test") ~ prop.test,
                  contains("ancova") ~ "ancova",
                  contains("cohens_d") ~ "cohens_d"
      ),
      test.args = list(var_t.test_dots = list(var.equal = TRUE),
                       var_wilcox.test_dots = list(correct = FALSE),
                       var_prop.test_dots = list(alternative = "greater"),
                       var_mcnemar.test_dots = list(correct = FALSE))
    )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_t.test") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    t.test(age ~ as.factor(trt), data = trial) %>%
      broom::tidy() %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value")))
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_t.test_dots") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    t.test(age ~ as.factor(trt), data = trial, var.equal = TRUE) %>%
      broom::tidy() %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value")))
  )


  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_wilcox.test") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    wilcox.test(age ~ trt, data = trial, conf.int = TRUE) %>%
      broom::tidy() %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value")))
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_wilcox.test_dots") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    wilcox.test(age ~ trt, data = trial, conf.int = TRUE, correct = FALSE) %>%
      broom::tidy() %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value")))
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_prop.test") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    table(trial$trt, factor(trial$response) %>% forcats::fct_rev()) %>%
      prop.test() %>%
      broom::tidy() %>%
      dplyr::mutate(estimate = estimate1 - estimate2) %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value")))
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_prop.test_dots") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    table(trial$trt, factor(trial$response) %>% forcats::fct_rev()) %>%
      prop.test(alternative = "greater") %>%
      broom::tidy() %>%
      dplyr::mutate(estimate = estimate1 - estimate2) %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value")))
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_ancova") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    lm(age ~ forcats::fct_rev(factor(trt)), trial) %>%
      broom::tidy(conf.int = TRUE) %>%
      dplyr::slice(dplyr::n()) %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    check.attributes = FALSE
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_cohens_d") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    effectsize::cohens_d(age ~ trt, data = trial) %>%
      tibble::as_tibble() %>%
      select(-CI) %>%
      purrr::set_names(c("estimate", "conf.low", "conf.high"))
  )


  trial_group <- trial %>% group_by(trt) %>% mutate(id = dplyr::row_number()) %>% ungroup()
  trial_group_wide <-
    trial_group %>%
    dplyr::filter(trt == "Drug A") %>%
    dplyr::full_join(
      trial_group %>%
        filter(trt == "Drug B"),
      by = "id"
    )

  tbl_groups <-
    trial_group %>%
    select(trt, id, stage, marker,
           age_ancova_lme4 = age) %>%
    tbl_summary(by = trt, missing = "no", include = -c("id", "stage", "marker"),) %>%
    add_difference(
      test = list(contains("ancova_lme4") ~ "ancova_lme4"),
      group = "id",
      adj.vars = c("stage", "marker")
    )

  expect_equal(
    filter(tbl_groups$meta_data, variable == "age_ancova_lme4") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    lme4::lmer(age ~ stage + marker + forcats::fct_rev(factor(trt)) + (1|id), trial_group) %>%
      broom.mixed::tidy(conf.int = TRUE, effects = "fixed") %>%
      dplyr::slice(dplyr::n()) %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    check.attributes = FALSE
  )
})
