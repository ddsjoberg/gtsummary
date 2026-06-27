skip_on_cran()
skip_if_pkg_not_installed(
  c("broom", "broom.helpers", "lme4", "smd", "effectsize", "emmeans"),
  ref = "cardx"
)

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
  skip_if_pkg_not_installed("emmeans", ref = "cardx")
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
        -cards::all_ard_variables(),
        -fmt_fun
      ),
    cardx::ard_emmeans_contrast(
      data = trial,
      formula = ttdeath ~ trt,
      method = "lm",
      response_type = "continuous"
    ) |>
      dplyr::select(
        -cards::all_ard_groups(),
        -cards::all_ard_variables(),
        -fmt_fun
      ),
    ignore_attr = TRUE
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
        -cards::all_ard_variables(),
        -fmt_fun
      ),
    cardx::ard_emmeans_contrast(
      data = trial,
      formula = ttdeath ~ trt + (1 | grade),
      method = "lmer",
      package = "lme4",
      response_type = "continuous"
    ) |>
      dplyr::select(
        -cards::all_ard_groups(),
        -cards::all_ard_variables(),
        -fmt_fun
      ),
    ignore_attr = TRUE
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
        -cards::all_ard_variables(),
        -fmt_fun
      ),
    cardx::ard_emmeans_contrast(
      data = trial,
      formula = response ~ trt,
      method = "glm",
      method.args = list(family = binomial),
      response_type = "dichotomous"
    ) |>
      dplyr::select(
        -cards::all_ard_groups(),
        -cards::all_ard_variables(),
        -fmt_fun
      ),
    ignore_attr = TRUE
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
  expect_snapshot({
    tbl <- mtcars |>
      mutate(
        .by = am,
        id = dplyr::row_number(),
        am = factor(am, levels = c(0, 1))
      ) %>%
      tbl_summary(
        by = am,
        include = mpg
      ) |>
      add_difference(test = ~"paired.t.test", group = id)

    tbl |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  })
  expect_snapshot({
    tbl <- mtcars |>
      mutate(
        .by = am,
        id = dplyr::row_number(),
        am = factor(am, levels = c(1, 0))
      ) |>
      tbl_summary(
        by = am,
        include = mpg
      ) |>
      add_difference(test = ~"paired.t.test", group = id)

    tbl |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  })
})

test_that("addressing GH #2165: Non-logical dichotomous comparisons using prop.test()", {
  # check the results are correct by matching ARDs
  expect_equal(
    trial |>
      dplyr::mutate(response = factor(response, levels = c(0, 1), labels = c("no", "yes"))) |>
      tbl_summary(
        by = trt,
        include = response
      ) |>
      add_difference() |>
      gather_ard() |>
      getElement("add_difference") |>
      getElement("response") |>
      dplyr::select(-"fmt_fun"),
    trial |>
      dplyr::mutate(response = response == 1) |>
      cardx::ard_stats_prop_test(by = trt, variable = response) |>
      cards::replace_null_statistic() |>
      dplyr::select(-"fmt_fun"),
    ignore_attr = TRUE
  )

  # check when the value presented is the opposite (FALSE)
  expect_equal(
    trial |>
      dplyr::mutate(response = as.logical(response)) |>
      tbl_summary(
        by = trt,
        include = response,
        value = list(response = FALSE)
      ) |>
      add_difference() |>
      gather_ard() |>
      getElement("add_difference") |>
      getElement("response") |>
      dplyr::select(-"fmt_fun"),
    trial |>
      dplyr::mutate(response = response == 0) |>
      cardx::ard_stats_prop_test(by = trt, variable = response) |>
      cards::replace_null_statistic() |>
      dplyr::select(-"fmt_fun"),
    ignore_attr = TRUE
  )

  # check results when variable has >2 levels
  expect_equal(
    trial |>
      tbl_summary(
        by = trt,
        include = grade,
        value = list(grade = "I")
      ) |>
      add_difference() |>
      gather_ard() |>
      getElement("add_difference") |>
      getElement("grade") |>
      dplyr::select(-"fmt_fun"),
    trial |>
      dplyr::mutate(grade = grade == "I") |>
      cardx::ard_stats_prop_test(by = trt, variable = grade) |>
      cards::replace_null_statistic() |>
      dplyr::select(-"fmt_fun"),
    ignore_attr = TRUE
  )
})


test_that("add_difference.tbl_summary(levels) selects two groups when by has 3+ levels", {
  # selecting two groups from a 3-level `by` runs without error and keeps all stat cols
  expect_error(
    tbl_diff <-
      trial |>
      tbl_summary(by = grade, include = c(age, marker), missing = "no") |>
      add_difference(levels = c("I", "III")),
    NA
  )
  # all N original stat columns are retained
  expect_equal(
    sum(grepl("^stat_\\d+$", tbl_diff$table_styling$header$column)),
    3L
  )
  # difference result columns are added (estimate, CI, p-value)
  expect_true(all(c("estimate", "conf.low", "conf.high", "p.value") %in% names(tbl_diff$table_body)))

  # estimate equals levels[1] - levels[2] (I - III)
  est_I_III <-
    tbl_diff$cards$add_difference$age |>
    dplyr::filter(stat_name == "estimate") |>
    dplyr::pull("stat") |>
    unlist()
  expected <-
    trial |>
    dplyr::filter(grade %in% c("I", "III")) |>
    (\(d) t.test(age ~ factor(grade, levels = c("I", "III")), data = d))() |>
    getElement("estimate") |>
    (\(m) unname(m[1] - m[2]))()
  expect_equal(est_I_III, expected, ignore_attr = TRUE)
})

test_that("add_difference.tbl_summary(levels) flips sign when levels are reversed", {
  est_fun <- function(lvls) {
    trial |>
      tbl_summary(by = grade, include = age, missing = "no") |>
      add_difference(levels = lvls) |>
      getElement("cards") |>
      getElement("add_difference") |>
      getElement("age") |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::pull("stat") |>
      unlist()
  }
  expect_equal(est_fun(c("I", "III")), -est_fun(c("III", "I")))

  # CI bounds swap (and negate) when reversed
  ci_fun <- function(lvls) {
    tbl <-
      trial |>
      tbl_summary(by = grade, include = age, missing = "no") |>
      add_difference(levels = lvls)
    card <- tbl$cards$add_difference$age
    c(
      low = card |> dplyr::filter(stat_name == "conf.low") |> dplyr::pull("stat") |> unlist(),
      high = card |> dplyr::filter(stat_name == "conf.high") |> dplyr::pull("stat") |> unlist()
    )
  }
  ci_fwd <- ci_fun(c("I", "III"))
  ci_rev <- ci_fun(c("III", "I"))
  expect_equal(unname(ci_fwd["low"]), -unname(ci_rev["high"]))
  expect_equal(unname(ci_fwd["high"]), -unname(ci_rev["low"]))
})

test_that("add_difference.tbl_summary(levels) works for two-level by (flip direction)", {
  est_fun <- function(...) {
    trial |>
      tbl_summary(by = trt, include = age, missing = "no") |>
      add_difference(...) |>
      getElement("cards") |>
      getElement("add_difference") |>
      getElement("age") |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::pull("stat") |>
      unlist()
  }
  est_default <- est_fun()
  est_flip <- est_fun(levels = c("Drug B", "Drug A"))
  expect_equal(est_default, -est_flip)

  # supplying levels in the default order matches the default output
  est_same <- est_fun(levels = c("Drug A", "Drug B"))
  expect_equal(est_default, est_same)
})

test_that("add_difference.tbl_summary(levels) backward compatibility for two-level by", {
  # a two-level call with no `levels` is unchanged
  expect_error(
    tbl_default <-
      trial |>
      tbl_summary(by = trt, include = c(age, marker), missing = "no") |>
      add_difference(),
    NA
  )
  expect_true(all(c("estimate", "conf.low", "conf.high", "p.value") %in% names(tbl_default$table_body)))
})

test_that("add_difference.tbl_summary(levels) validation errors", {
  # 3+ levels and no `levels` -> informative error pointing to `levels`
  expect_error(
    trial |>
      tbl_summary(by = grade, include = age, missing = "no") |>
      add_difference(),
    "levels"
  )
  # wrong length
  expect_error(
    trial |>
      tbl_summary(by = grade, include = age, missing = "no") |>
      add_difference(levels = "I"),
    "length-two"
  )
  # non-existent level
  expect_error(
    trial |>
      tbl_summary(by = grade, include = age, missing = "no") |>
      add_difference(levels = c("I", "X")),
    "not present|one of"
  )
  # duplicated level
  expect_error(
    trial |>
      tbl_summary(by = grade, include = age, missing = "no") |>
      add_difference(levels = c("I", "I")),
    "distinct"
  )
})

test_that("add_difference.tbl_summary(levels) composes with adj.vars and test", {
  expect_error(
    tbl_adj <-
      trial |>
      tbl_summary(
        by = grade,
        statistic = all_continuous() ~ "{mean} ({sd})",
        include = c(age, marker),
        missing = "no"
      ) |>
      add_difference(levels = c("I", "III"), adj.vars = stage),
    NA
  )
  expect_true("estimate" %in% names(tbl_adj$table_body))

  # explicit test argument composes with levels
  expect_error(
    trial |>
      tbl_summary(by = grade, include = age, missing = "no") |>
      add_difference(levels = c("I", "III"), test = everything() ~ "t.test"),
    NA
  )
})

test_that("add_difference.tbl_summary(levels) adds footnote naming compared pair", {
  tbl_diff <-
    trial |>
    tbl_summary(by = grade, include = age, missing = "no") |>
    add_difference(levels = c("I", "III"))
  footnotes <- tbl_diff$table_styling$footnote_header$footnote
  expect_true(any(grepl("I - III", footnotes)))
})
