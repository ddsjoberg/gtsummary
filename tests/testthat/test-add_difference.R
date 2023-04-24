skip_on_cran()
skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("emmeans", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("smd", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))

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
  expect_snapshot(tbl_diff %>% as.data.frame())

  expect_equal(
    dplyr::filter(tbl_diff$table_body, variable == "marker") %>% select(estimate, conf.low, conf.high, p.value),
    t.test(marker ~ trt, trial, var.equal = TRUE) %>% broom::tidy() %>% select(estimate, conf.low, conf.high, p.value)
  )

  expect_snapshot(
    trial %>%
      select(trt, response, grade) %>%
      tbl_summary(by = trt, percent = "row") %>%
      add_difference() %>%
      as.data.frame()
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
      test = list(
        contains("t.test") ~ t.test,
        contains("wilcox.test") ~ wilcox.test,
        contains("prop.test") ~ prop.test,
        contains("ancova") ~ "ancova",
        contains("cohens_d") ~ "cohens_d"
      ),
      test.args = list(
        var_t.test_dots = list(var.equal = TRUE),
        var_wilcox.test_dots = list(correct = FALSE),
        var_prop.test_dots = list(alternative = "greater"),
        var_mcnemar.test_dots = list(correct = FALSE)
      )
    )
  expect_snapshot(tbl_test.args %>% as.data.frame())

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
    wilcox.test(age ~ trt, data = trial) %>%
      broom::tidy() %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value")))
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_wilcox.test_dots") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    wilcox.test(age ~ trt, data = trial, correct = FALSE) %>%
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
    ignore_attr = TRUE
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_cohens_d") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    effectsize::cohens_d(
      age ~ trt,
      data = trial %>% tidyr::drop_na("age", "trt")
    ) %>%
      tibble::as_tibble() %>%
      select(-CI) %>%
      purrr::set_names(c("estimate", "conf.low", "conf.high"))
  )


  trial_group <- trial %>%
    group_by(trt) %>%
    mutate(id = dplyr::row_number()) %>%
    ungroup()
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
      age_ancova_lme4 = age
    ) %>%
    tbl_summary(by = trt, missing = "no", include = -c("id", "stage", "marker"), ) %>%
    add_difference(
      test = list(contains("ancova_lme4") ~ "ancova_lme4"),
      group = "id",
      adj.vars = c("stage", "marker")
    )
  expect_snapshot(tbl_groups %>% as.data.frame())

  expect_equal(
    filter(tbl_groups$meta_data, variable == "age_ancova_lme4") %>%
      purrr::pluck("test_result", 1, "df_result") %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    lme4::lmer(age ~ stage + marker + forcats::fct_rev(factor(trt)) + (1 | id), trial_group) %>%
      broom.mixed::tidy(conf.int = TRUE, effects = "fixed") %>%
      dplyr::slice(dplyr::n()) %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )
})

test_that("row formatting of differences and CIs work", {
  expect_error(
    tbl1 <-
      trial %>%
      select(trt, age, marker, response, death) %>%
      tbl_summary(
        by = trt,
        statistic =
          list(
            all_continuous() ~ "{mean} ({sd})",
            all_dichotomous() ~ "{p}%"
          ),
        missing = "no"
      ) %>%
      add_difference() %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl1)

  expect_equal(
    tbl1$estimate,
    c("-0.44", "0.20", "-4.2%", "-5.8%")
  )

  expect_equal(
    tbl1$ci,
    c("-4.6, 3.7", "-0.05, 0.44", "-18%, 9.9%", "-21%, 9.0%")
  )
})

test_that("no error with missing data", {
  expect_message(
    t1 <-
      mtcars %>%
      mutate(mpg = NA, hp = NA) %>%
      select(mpg, hp, am) %>%
      tbl_summary(by = "am", type = hp ~ "continuous", missing = "no") %>%
      add_difference()
  )
  expect_snapshot(t1 %>% as.data.frame())

  expect_equal(
    t1 %>% as_tibble(col_labels = FALSE) %>% dplyr::pull(p.value),
    rep_len(NA_character_, 2)
  )
})

test_that("add_difference() with smd", {
  expect_error(
    tbl <-
      trial %>%
      select(trt, age, response, grade) %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_difference(test = everything() ~ "smd") %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    tbl$estimate[1:3],
    c("-0.03", "-0.09", "0.07")
  )
  expect_equal(
    tbl$ci[1:3],
    c("-0.32, 0.25", "-0.37, 0.19", "-0.20, 0.35")
  )
  expect_snapshot(tbl)
})

test_that("add_difference() with smd and survey weights", {
  # this is the same example from the tableone propensity score vignette
  rhc <-
    read.csv("https://raw.githubusercontent.com/Ngendahimana/epbi500/master/data-raw/rhc.csv")
  rhc$swang1 <- factor(rhc$swang1, levels = c("No RHC", "RHC"))

  psModel <- glm(
    formula = swang1 ~ age + sex + race + edu + income + ninsclas +
      cat1 + das2d3pc + dnr1 + ca + surv2md1 + aps1 + scoma1 +
      wtkilo1 + temp1 + meanbp1 + resp1 + hrt1 + pafi1 +
      paco21 + ph1 + wblc1 + hema1 + sod1 + pot1 + crea1 +
      bili1 + alb1 + resp + card + neuro + gastr + renal +
      meta + hema + seps + trauma + ortho + cardiohx + chfhx +
      dementhx + psychhx + chrpulhx + renalhx + liverhx + gibledhx +
      malighx + immunhx + transhx + amihx,
    family = binomial(link = "logit"),
    data = rhc
  )

  rhc$pRhc <- predict(psModel, type = "response")
  rhc$pNoRhc <- 1 - rhc$pRhc
  rhc$pAssign <- NA
  rhc$pAssign[rhc$swang1 == "RHC"] <- rhc$pRhc[rhc$swang1 == "RHC"]
  rhc$pAssign[rhc$swang1 == "No RHC"] <- rhc$pNoRhc[rhc$swang1 == "No RHC"]
  rhc$pMin <- pmin(rhc$pRhc, rhc$pNoRhc)
  rhc$mw <- rhc$pMin / rhc$pAssign

  rhcSvy <- survey::svydesign(ids = ~1, data = rhc, weights = ~mw)

  expect_error(
    tbl <-
      rhcSvy %>%
      {suppressWarnings(
        tbl_svysummary(
          .,
          by = swang1,
          statistic = all_continuous() ~ "{mean} ({sd})",
          include = all_of(c("age", "sex", "race"))
        ) %>%
          add_difference(
            everything() ~ "smd",
            estimate_fun = everything() ~ purrr::partial(style_sigfig, digits = 3)
          )
      )} %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    tbl$estimate %>% na.omit(),
    c("0.003", "0.003", "0.009"),
    ignore_attr = TRUE
  )
  expect_snapshot(tbl)
})


test_that("add_difference() with emmeans()", {
  tbl <-
    tbl_summary(
      trial,
      by = trt,
      include = c(age, response),
      missing = "no"
    )

  expect_error(
    res <- tbl %>%
      add_difference(test = everything() ~ "emmeans", adj.vars = "stage"),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
  expect_error(
    tbl %>%
      add_difference(test = everything() ~ "emmeans", group = "death"),
    NA
  )

  expect_error(
    survey::svydesign(ids = ~1, data = trial, weights = ~1) %>%
      tbl_svysummary(
        by = trt,
        include = c(age, response),
        missing = "no"
      ) %>%
      add_difference(test = everything() ~ "emmeans", adj.vars = "marker"),
    NA
  )
})
