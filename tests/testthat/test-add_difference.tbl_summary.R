skip_on_cran()
skip_if_not(is_pkg_installed(c(
  "broom", "broom.helpers", "lme4", "smd",
  "effectsize", "emmeans"
), ref = "cardx"))

test_that("add_difference.tbl_summary() works with basic usage", {
  expect_error(
    tbl_diff <-
      trial |>
      select(trt, marker, age) |>
      tbl_summary(by = trt, missing = "no") |>
      add_difference(
        test = everything() ~ "t.test",
        test.args = all_tests("t.test") ~ list(var.equal = TRUE)
      ),
    NA
  )
  expect_snapshot(
    tbl_diff |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )

  expect_equal(
    tbl_diff$cards$add_difference$marker |>
      dplyr::filter(stat_name %in% c("estimate", "statistic", "parameter", "conf.low", "conf.high", "p.value")) |>
      dplyr::mutate(stat = unlist(stat)) |>
      tidyr::pivot_wider(
        id_cols = c(group1, variable),
        names_from = stat_name,
        values_from = stat
      ) |>
      dplyr::select(all_of(c("estimate", "statistic", "parameter", "conf.low", "conf.high", "p.value"))),
    t.test(marker ~ trt, trial, var.equal = TRUE) %>%
      {
        withr::with_package("broom", tidy(.))
      } |>
      select(all_of(c("estimate", "statistic", "parameter", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_snapshot(
    trial |>
      select(trt, response, grade) |>
      tbl_summary(by = trt, percent = "row") |>
      add_difference() |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )
})

test_that("add_difference.tbl_summary(tests = 'emmeans')", {
  skip_if_not(is_pkg_installed("emmeans", ref = "cardx"))
  tbl1 <-
    trial |>
    tbl_summary(
      by = trt,
      include = ttdeath
    ) |>
    add_difference(test = ~"emmeans")

  expect_equal(
    tbl1$cards$add_difference$ttdeath |>
      dplyr::select(
        -cards::all_ard_groups(),
        -cards::all_ard_variables()
      ),
    cardx::ard_emmeans_mean_difference(
      data = trial,
      formula = ttdeath ~ trt,
      method = "lm",
      response_type = "continuous"
    ) |>
      dplyr::select(
        -cards::all_ard_groups(),
        -cards::all_ard_variables()
      )
  )

  tbl2 <-
    trial |>
    tbl_summary(
      by = trt,
      include = ttdeath
    ) |>
    add_difference(test = ~"emmeans", group = "grade")

  expect_equal(
    tbl2$cards$add_difference$ttdeath |>
      dplyr::select(
        -cards::all_ard_groups(),
        -cards::all_ard_variables()
      ),
    cardx::ard_emmeans_mean_difference(
      data = trial,
      formula = ttdeath ~ trt + (1 | grade),
      method = "lmer",
      package = "lme4",
      response_type = "continuous"
    ) |>
      dplyr::select(
        -cards::all_ard_groups(),
        -cards::all_ard_variables()
      )
  )

  tbl3 <-
    trial |>
    tbl_summary(
      by = trt,
      include = response
    ) |>
    add_difference(test = ~"emmeans")

  expect_equal(
    tbl3$cards$add_difference$response |>
      dplyr::select(
        -cards::all_ard_groups(),
        -cards::all_ard_variables()
      ),
    cardx::ard_emmeans_mean_difference(
      data = trial,
      formula = response ~ trt,
      method = "glm",
      method.args = list(family = binomial),
      response_type = "dichotomous"
    ) |>
      dplyr::select(
        -cards::all_ard_groups(),
        -cards::all_ard_variables()
      )
  )
})

test_that("statistics are replicated within add_difference.tbl_summary()", {
  tbl_test.args <-
    trial |>
    select(trt,
      var_t.test = age,
      var_t.test_dots = age,
      var_wilcox.test = age,
      var_wilcox.test_dots = age,
      var_prop.test = response,
      var_prop.test_dots = response,
      var_ancova = age,
      var_cohens_d = age,
      var_hedges_g = age,
      var_smd = age
    ) %>%
    tbl_summary(
      by = trt, missing = "no",
      statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%"),
      digits = all_continuous() ~ 3,
      label = as.list(names(.)) |> setNames(names(.))
    ) |>
    add_difference(
      test = list(
        contains("t.test") ~ t.test,
        contains("wilcox.test") ~ wilcox.test,
        contains("prop.test") ~ prop.test,
        contains("ancova") ~ "ancova",
        contains("cohens_d") ~ "cohens_d",
        contains("hedges_g") ~ "hedges_g",
        contains("smd") ~ "smd",
        contains("emmeans") ~ "emmeans"
      ),
      test.args = list(
        var_t.test_dots = list(var.equal = TRUE),
        var_wilcox.test_dots = list(correct = FALSE),
        var_prop.test_dots = list(alternative = "greater")
      )
    )
  expect_snapshot(
    tbl_test.args |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )

  expect_equal(
    tbl_test.args$cards$add_difference$var_t.test |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(group1, variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    t.test(age ~ as.factor(trt), data = trial) %>%
      {
        withr::with_package("broom", tidy(.))
      } |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_test.args$cards$add_difference$var_t.test_dots |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(group1, variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    t.test(age ~ as.factor(trt), data = trial, var.equal = TRUE) %>%
      {
        withr::with_package("broom", tidy(.))
      } |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )


  expect_equal(
    tbl_test.args$cards$add_difference$var_wilcox.test |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(group1, variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    wilcox.test(age ~ trt, data = trial, conf.int = TRUE) %>%
      {
        withr::with_package("broom", tidy(.))
      } |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_test.args$cards$add_difference$var_wilcox.test_dots |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(group1, variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    wilcox.test(age ~ trt, data = trial, correct = FALSE, conf.int = TRUE) %>%
      {
        withr::with_package("broom", tidy(.))
      } |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_test.args$cards$add_difference$var_prop.test |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(group1, variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    table(trial$trt, factor(trial$response) %>% fct_rev()) |>
      prop.test() %>%
      {
        withr::with_package("broom", tidy(.))
      } |>
      dplyr::mutate(estimate = estimate1 - estimate2) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_test.args$cards$add_difference$var_prop.test_dots |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(group1, variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    table(trial$trt, factor(trial$response) %>% fct_rev()) |>
      prop.test(alternative = "greater") %>%
      {
        withr::with_package("broom", tidy(.))
      } |>
      dplyr::mutate(estimate = estimate1 - estimate2) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_test.args$cards$add_difference$var_ancova |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    lm(age ~ fct_rev(trt), trial) %>%
      {
        withr::with_package("broom", tidy(., conf.int = TRUE))
      } |>
      dplyr::slice(dplyr::n()) %>%
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_test.args$cards$add_difference$var_cohens_d |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    withr::with_package(
      package = "effectsize",
      cohens_d(
        age ~ trt,
        data = trial %>% tidyr::drop_na("age", "trt"),
        verbose = FALSE
      )
    ) |>
      dplyr::as_tibble() |>
      select(-CI) |>
      set_names(c("estimate", "conf.low", "conf.high")),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_test.args$cards$add_difference$var_hedges_g |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    withr::with_package(
      package = "effectsize",
      hedges_g(
        age ~ trt,
        data = trial %>% tidyr::drop_na("age", "trt"),
        verbose = FALSE
      )
    ) |>
      dplyr::as_tibble() |>
      select(-CI) |>
      set_names(c("estimate", "conf.low", "conf.high")),
    ignore_attr = TRUE
  )
})


test_that("statistics are replicated within add_difference.tbl_summary(group)", {
  trial_group <- trial |>
    dplyr::mutate(.by = trt, id = dplyr::row_number())
  trial_group_wide <-
    trial_group |>
    dplyr::filter(trt == "Drug A") |>
    dplyr::full_join(
      trial_group |>
        dplyr::filter(trt == "Drug B"),
      by = "id"
    )

  tbl_groups <-
    trial_group |>
    select(trt, id, stage, marker,
      age_ancova_lme4 = age,
      age_paired_t_test = age,
      age_paired_cohens_d = age,
      age_paired_hedges_g = age
    ) %>%
    tbl_summary(
      by = trt,
      missing = "no",
      include = starts_with("age_"),
      label = as.list(names(.)) |> setNames(names(.))
    ) |>
    add_difference(
      test = list(
        age_ancova_lme4 = "ancova_lme4",
        age_paired_t_test = "paired.t.test",
        age_paired_cohens_d = "paired_cohens_d",
        age_paired_hedges_g = "paired_hedges_g"
      ),
      group = "id"
    )
  expect_snapshot(
    tbl_groups |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )

  expect_equal(
    tbl_groups$cards$add_difference$age_ancova_lme4 |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    withr::with_package(
      package = "lme4",
      lmer(age ~ fct_rev(factor(trt)) + (1 | id), trial_group)
    ) %>%
      {
        withr::with_package("broom.mixed", tidy(., conf.int = TRUE, effects = "fixed"))
      } |>
      dplyr::slice(dplyr::n()) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_groups$cards$add_difference$age_paired_t_test |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    stats::t.test(
      x = trial_group_wide$age.x,
      y = trial_group_wide$age.y,
      paired = TRUE
    ) %>%
      {
        withr::with_package("broom", tidy(.))
      } |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_groups$cards$add_difference$age_paired_cohens_d |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    withr::with_package(
      package = "effectsize",
      cohens_d(
        x = trial_group_wide$age.x,
        y = trial_group_wide$age.y,
        paired = TRUE,
        verbose = FALSE
      )
    ) %>%
      {
        withr::with_package("parameters", standardize_names(., style = "broom"))
      } |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_groups$cards$add_difference$age_paired_hedges_g |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    withr::with_package(
      package = "effectsize",
      hedges_g(
        x = trial_group_wide$age.x,
        y = trial_group_wide$age.y,
        paired = TRUE,
        verbose = FALSE
      )
    ) %>%
      {
        withr::with_package("parameters", standardize_names(., style = "broom"))
      } |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )
})

test_that("row formatting of differences and CIs work", {
  expect_error(
    tbl1 <-
      trial |>
      select(trt, age, marker, response, death) |>
      tbl_summary(
        by = trt,
        statistic =
          list(
            all_continuous() ~ "{mean} ({sd})",
            all_dichotomous() ~ "{p}%"
          ),
        missing = "no"
      ) %>%
      add_difference() |>
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl1)

  expect_equal(
    tbl1$estimate,
    c("-0.44", "0.20", "-4.2%", "-5.8%")
  )

  expect_equal(
    tbl1$conf.low,
    c("-4.6, 3.7", "-0.05, 0.44", "-18%, 9.9%", "-21%, 9.0%")
  )
})

test_that("no error with missing data", {
  expect_message(
    t1 <-
      mtcars |>
      mutate(mpg = NA, hp = NA) |>
      select(mpg, hp, am) |>
      tbl_summary(by = "am", type = hp ~ "continuous", missing = "no") |>
      add_difference()
  )
  expect_snapshot(t1 |> modify_column_hide(all_stat_cols()) |> as.data.frame())

  expect_equal(
    t1 %>% as_tibble(col_labels = FALSE) %>% dplyr::pull(p.value),
    rep_len(NA_character_, 2)
  )
})

test_that("add_difference() with smd", {
  expect_error(
    tbl <-
      trial |>
      select(trt, age, response, grade) |>
      tbl_summary(by = trt, missing = "no") |>
      add_difference(test = everything() ~ "smd") |>
      as.data.frame(col_labels = FALSE),
    NA
  )
  expect_equal(
    tbl$estimate[1:3],
    c("-0.03", "-0.09", "0.07")
  )
  expect_equal(
    tbl$conf.low[1:3],
    c("-0.32, 0.25", "-0.37, 0.19", "-0.20, 0.35")
  )
  expect_snapshot(tbl)
})

test_that("add_difference.tbl_summary() with emmeans()", {
  tbl <-
    tbl_summary(
      trial,
      by = trt,
      include = c(age, response),
      missing = "no"
    )

  expect_error(
    res <- add_difference(tbl, test = everything() ~ "emmeans", adj.vars = "stage"),
    NA
  )
  expect_snapshot(
    res |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )
  expect_error(
    tbl |>
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

test_that("ordering in add_difference.tbl_summary() with paired tests", {
  expect_snapshot(
    mtcars |>
      mutate(
        .by = am,
        id = dplyr::row_number(),
        am = factor(am, levels = c(0, 1))
      ) %>%
      tbl_summary(
        by = am,
        include = mpg
      ) |>
      add_difference(test = ~"paired.t.test", group = id) |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )
  expect_snapshot(
    mtcars |>
      mutate(
        .by = am,
        id = dplyr::row_number(),
        am = factor(am, levels = c(1, 0))
      ) |>
      tbl_summary(
        by = am,
        include = mpg
      ) |>
      add_difference(test = ~"paired.t.test", group = id) |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )
})
