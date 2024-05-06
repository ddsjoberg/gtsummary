skip_if_not(is_pkg_installed(c("broom", "broom.helpers", "lme4", "smd", "emmeans"), reference_pkg = "cardx"))

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
      as.data.frame())

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
      {withr::with_package("broom", tidy(.))} |>
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
           var_smd = age,
           var_emmeans = age,
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
      {withr::with_package("broom", tidy(.))} |>
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
      {withr::with_package("broom", tidy(.))} |>
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
    {withr::with_package("broom", tidy(.))} |>
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
      {withr::with_package("broom", tidy(.))} |>
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
      {withr::with_package("broom", tidy(.))} |>
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
      {withr::with_package("broom", tidy(.))} |>
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
      {withr::with_package("broom", tidy(., conf.int = TRUE))} |>
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
      tibble::as_tibble() |>
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
      tibble::as_tibble() |>
      select(-CI) |>
      set_names(c("estimate", "conf.low", "conf.high")),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_test.args$cards$add_difference$var_emmeans |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    dplyr::tibble(estimate = -0.4379906, conf.low = -4.558684, conf.high = 3.682703, p.value = 0.8341437),
    tolerance = 0.00001
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
           age_paired_hedges_g = age,
           age_emmeans = age
    ) %>%
    tbl_summary(
      by = trt,
      missing = "no",
      include = starts_with("age_"),
      label = as.list(names(.)) |> setNames(names(.))
    ) |>
    add_difference(
      test = list(age_ancova_lme4 = "ancova_lme4",
                  age_paired_t_test = "paired.t.test",
                  age_paired_cohens_d = "paired_cohens_d",
                  age_paired_hedges_g = "paired_hedges_g",
                  age_emmeans = "emmeans"),
      group = "id"
    )
  expect_snapshot(tbl_groups |> as.data.frame())

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
      {withr::with_package("broom.mixed", tidy(., conf.int = TRUE, effects = "fixed"))} |>
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
      {withr::with_package("broom", tidy(.))} |>
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
      {withr::with_package("parameters", standardize_names(., style = "broom"))} |>
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
      {withr::with_package("parameters", standardize_names(., style = "broom"))} |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )
})

