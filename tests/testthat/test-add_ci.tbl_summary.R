skip_on_cran()
skip_if_not(is_pkg_installed("cardx") && is_pkg_installed("broom", ref = "cardx"))

test_that("add_ci(method) with no `by`", {
  # create a tbl with all the methods present
  expect_silent(
    tbl <-
      trial |>
      select(
        age_t.test = age,
        age_wilcox.test = age,
        grade_wilson = grade,
        grade_wilson.no.correct = grade,
        grade_exact = grade,
        grade_wald = grade,
        grade_wald.no.correct = grade,
        grade_agresti.coull = grade,
        grade_jeffreys = grade
      ) %>%
      tbl_summary(
        missing = "no",
        label = as.list(names(.)) |> set_names(names(.)),
        statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
      ) |>
      add_ci(
        method = list(
          ends_with("t.test") ~ "t.test",
          ends_with("wilcox.test") ~ "wilcox.test",
          ends_with("wilson") ~ "wilson",
          ends_with("wilson.no.correct") ~ "wilson.no.correct",
          ends_with("wald") ~ "wald",
          ends_with("wald.no.correct") ~ "wald.no.correct",
          ends_with("exact") ~ "exact",
          ends_with("agresti.coull") ~ "agresti.coull",
          ends_with("jeffreys") ~ "jeffreys"
        ),
        style_fun =
          list(all_continuous() ~ label_style_sigfig(digits = 4),
               all_categorical() ~ label_style_sigfig(digits = 4, scale =  100))
      )
  )

  # check t.test results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_t.test") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_stats_t_test_onesample(trial, variables = age) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilcox.test results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_wilcox.test") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_stats_wilcox_test_onesample(trial, variables = age, conf.int = TRUE) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilson
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wilson", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "wilsoncc") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilson.no.correct
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wilson.no.correct", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "wilson") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wald
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wald", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "waldcc") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wald.no.correct
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wald.no.correct", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "wald") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check exact
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_exact", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "clopper-pearson") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check agresti.coull
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_agresti.coull", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "agresti-coull") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check jeffreys
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_jeffreys", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "jeffreys") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )
})

test_that("add_ci(method) with `by`", {
  # create a tbl with all the methods present
  expect_silent(
    tbl <-
      trial |>
      select(
        trt,
        age_t.test = age,
        age_wilcox.test = age,
        grade_wilson = grade,
        grade_wilson.no.correct = grade,
        grade_exact = grade,
        grade_wald = grade,
        grade_wald.no.correct = grade,
        grade_agresti.coull = grade,
        grade_jeffreys = grade
      ) %>%
      tbl_summary(
        by = trt,
        missing = "no",
        label = as.list(names(.)) |> set_names(names(.)),
        statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
      ) |>
      add_ci(
        method = list(
          ends_with("t.test") ~ "t.test",
          ends_with("wilcox.test") ~ "wilcox.test",
          ends_with("wilson") ~ "wilson",
          ends_with("wilson.no.correct") ~ "wilson.no.correct",
          ends_with("exact") ~ "exact",
          ends_with("agresti.coull") ~ "agresti.coull",
          ends_with("jeffreys") ~ "jeffreys"
        ),
        style_fun =
          list(all_continuous() ~ label_style_sigfig(digits = 4),
               all_categorical() ~ label_style_sigfig(digits = 4, scale =  100))
      )
  )

  # check t.test results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_t.test") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_stats_t_test_onesample(trial, variables = age, by = trt) |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilcox.test results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_wilcox.test") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_stats_wilcox_test_onesample(trial, variables = age, by = trt, conf.int = TRUE) |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilson
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wilson", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "wilsoncc", by = trt) |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilson.no.correct
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wilson.no.correct", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "wilson", by = trt) |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check exact
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_exact", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "clopper-pearson", by = trt) |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check agresti.coull
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_agresti.coull", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "agresti-coull", by = trt) |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check jeffreys
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_jeffreys", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "jeffreys", by = trt) |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )
})

test_that("add_ci(method) after `add_overall()`", {
  # create a tbl with all the methods present
  expect_silent(
    tbl <-
      trial |>
      select(
        trt,
        age_t.test = age,
        age_wilcox.test = age,
        grade_wilson = grade,
        grade_wilson.no.correct = grade,
        grade_exact = grade,
        grade_wald = grade,
        grade_wald.no.correct = grade,
        grade_agresti.coull = grade,
        grade_jeffreys = grade
      ) %>%
      tbl_summary(
        by = trt,
        missing = "no",
        label = as.list(names(.)) |> set_names(names(.)),
        statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
      ) |>
      add_overall() |>
      add_ci(
        method = list(
          ends_with("t.test") ~ "t.test",
          ends_with("wilcox.test") ~ "wilcox.test",
          ends_with("wilson") ~ "wilson",
          ends_with("wilson.no.correct") ~ "wilson.no.correct",
          ends_with("exact") ~ "exact",
          ends_with("agresti.coull") ~ "agresti.coull",
          ends_with("jeffreys") ~ "jeffreys"
        ),
        style_fun =
          list(all_continuous() ~ label_style_sigfig(digits = 4),
               all_categorical() ~ label_style_sigfig(digits = 4, scale =  100))
      )
  )

  # check t.test results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_t.test") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_stats_t_test_onesample(trial, variables = age) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilcox.test results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_wilcox.test") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_stats_wilcox_test_onesample(trial, variables = age, conf.int = TRUE) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilson
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wilson", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "wilsoncc") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilson.no.correct
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wilson.no.correct", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "wilson") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check exact
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_exact", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "clopper-pearson") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check agresti.coull
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_agresti.coull", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "agresti-coull") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check jeffreys
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_jeffreys", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "jeffreys") |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )
})


test_that("add_ci(method= ~'asymptotic')", {
  expect_equal(
    tbl_summary(trial, include = response, missing = "no") |>
      add_ci(method = ~"asymptotic", style_fun = ~label_style_number(digits = 2, scale = 100)) |>
      as.data.frame(),
    tbl_summary(trial, include = response, missing = "no") |>
      add_ci(method = ~"wald.no.correct", style_fun = ~label_style_number(digits = 2, scale = 100)) |>
      as.data.frame()
  )
})

test_that("add_ci(include)", {
  expect_snapshot(
    tbl_summary(
      trial,
      include = c(age, grade),
      by = trt,
      missing = "no"
    ) |>
      add_overall() |>
      add_ci(include = age)  |>
      as.data.frame()
  )
})

test_that("add_ci(statistic)", {
  expect_snapshot(
    tbl_summary(
      trial,
      include = c(age, grade),
      by = trt,
      missing = "no"
    ) |>
      add_ci(
        statistic = list(all_continuous() ~ "{conf.low} - {conf.high}",
                         all_categorical() ~ "{conf.low}% - {conf.high}%")
      ) |>
      as.data.frame()
  )
})

test_that("add_ci(conf.level)", {
  # create a tbl with all the methods present with modified confidence level
  expect_silent(
    tbl <-
      trial |>
      select(
        age_t.test = age,
        age_wilcox.test = age,
        grade_wilson = grade,
        grade_wilson.no.correct = grade,
        grade_exact = grade,
        grade_wald = grade,
        grade_wald.no.correct = grade,
        grade_agresti.coull = grade,
        grade_jeffreys = grade
      ) %>%
      tbl_summary(
        missing = "no",
        label = as.list(names(.)) |> set_names(names(.)),
        statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
      ) |>
      add_ci(
        method = list(
          ends_with("t.test") ~ "t.test",
          ends_with("wilcox.test") ~ "wilcox.test",
          ends_with("wilson") ~ "wilson",
          ends_with("wilson.no.correct") ~ "wilson.no.correct",
          ends_with("exact") ~ "exact",
          ends_with("agresti.coull") ~ "agresti.coull",
          ends_with("jeffreys") ~ "jeffreys"
        ),
        style_fun =
          list(all_continuous() ~ label_style_sigfig(digits = 4),
               all_categorical() ~ label_style_sigfig(digits = 4, scale =  100)),
        conf.level = 0.80
      )
  )

  # check t.test results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_t.test") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_stats_t_test_onesample(trial, variables = age, conf.level = 0.80) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilcox.test results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_wilcox.test") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_stats_wilcox_test_onesample(trial, variables = age, conf.int = TRUE, conf.level = 0.80) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilson
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wilson", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "wilsoncc", conf.level = 0.80) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check wilson.no.correct
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_wilson.no.correct", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "wilson", conf.level = 0.80) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check exact
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_exact", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "clopper-pearson", conf.level = 0.80) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check agresti.coull
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_agresti.coull", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "agresti-coull", conf.level = 0.80) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )

  # check jeffreys
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_jeffreys", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(trial, variables = grade, value = ~"I", method = "jeffreys", conf.level = 0.80) |>
      dplyr::mutate(
        fmt_fun = map2(
          fmt_fun, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fun() |>
      cards::get_ard_statistics(.column = "stat_fmt") |>
      glue_data("{conf.low}, {conf.high}")
  )
})

test_that("add_ci(pattern)", {
  expect_snapshot(
    tbl_summary(
      trial,
      include = c(age, grade),
      missing = "no",
      statistic = list(age = "{mean}")
    ) |>
      add_ci(
        include = age,
        pattern = "{stat} [{ci}]"
      ) |>
      as.data.frame()
  )
})


test_that("add_ci(pattern) messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = age,
      missing = "no",
      statistic = list(age = "{mean}")
    ) |>
      add_ci(pattern = "{not_a_stat} [{ci}]")
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = age,
      missing = "no",
      statistic = list(age = "{mean}")
    ) |>
      add_ci(pattern = "{ci}")
  )
})

test_that("add_ci(method) messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = age,
      missing = "no",
      statistic = list(age = "{mean}")
    ) |>
      add_ci(method = list(age = "wilson"))
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = grade
    ) |>
      add_ci(method = list(grade = "t.test"))
  )
})

test_that("add_ci() correctly handles dichotomous variables", {
  expect_silent(
    tbl <- tbl_summary(
      trial,
      include = c(response, grade),
      value = list(response = 0, grade = "III"),
      missing = "no"
    ) |>
      add_ci()
  )
  expect_snapshot(as.data.frame(tbl))

  expect_equal(
    tbl$inputs$value,
    tbl$cards$add_ci[c("variable", "variable_level")] |> unique() |> deframe()
  )
})

test_that("add_ci() correctly handles tbl_summary(percent='row')", {
  expect_silent(
    df <-
      trial |>
      tbl_summary(include = c(response, grade), percent='row', statistic = ~"{n} / {N} ({p}%)") |>
      add_ci(
        statistic = ~"{conf.low} {conf.high}",
        style_fun = ~label_style_number(scale = 100, digits = 1)
      ) |>
      as_tibble(col_labels = FALSE)
  )

  # check the CIs are correct for response
  expect_equal(
    df[1, "ci_stat_0"] |>
      dplyr::pull("ci_stat_0"),
    cardx::proportion_ci_wilson(
      x = rep_len(TRUE, table(trial$response) |> as.data.frame() |> dplyr::filter(Var1 == 1) |> dplyr::pull(Freq)),
      correct = TRUE
    )[c("conf.low", "conf.high")] |>
      map_chr(label_style_number(scale = 100, digits = 1)) |>
      unlist() |>
      paste(collapse = " ")
  )

  # check the CIs are correct for grade II
  expect_equal(
    df |>
      dplyr::filter(label == "II") |>
      dplyr::pull("ci_stat_0"),
    cardx::proportion_ci_wilson(
      x = rep_len(TRUE, table(trial$grade) |> as.data.frame() |> dplyr::filter(Var1 == "II") |> dplyr::pull(Freq)),
      correct = TRUE
    )[c("conf.low", "conf.high")] |>
      map_chr(label_style_number(scale = 100, digits = 1)) |>
      unlist() |>
      paste(collapse = " ")
  )

  # now with a by variable
  expect_equal(
    trial |>
      tbl_summary(by = trt, include = response, percent='row', statistic = ~"{n} / {N} ({p}%)", missing = "no") |>
      add_ci(
        statistic = ~"{conf.low} {conf.high}",
        style_fun = ~label_style_number(scale = 100, digits = 1)
      ) |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull("ci_stat_1"),
    cardx::proportion_ci_wilson(
      x = (trial$trt == "Drug A")[trial$response == 1],
      correct = TRUE
    )[c("conf.low", "conf.high")] |>
      map_chr(label_style_number(scale = 100, digits = 1)) |>
      unlist() |>
      paste(collapse = " ")
  )
})


test_that("add_ci() correctly handles tbl_summary(percent='cell')", {
  expect_silent(
    df <-
      trial |>
      tbl_summary(include = c(response, grade), percent='cell', statistic = ~"{n} / {N} ({p}%)") |>
      add_ci(
        statistic = ~"{conf.low} {conf.high}",
        style_fun = ~label_style_number(scale = 100, digits = 1)
      ) |>
      as_tibble(col_labels = FALSE)
  )

  # check the CIs are correct for response
  expect_equal(
    df[1, "ci_stat_0"] |>
      dplyr::pull("ci_stat_0"),
    cardx::proportion_ci_wilson(
      x = trial$response == 1,
      correct = TRUE
    )[c("conf.low", "conf.high")] |>
      map_chr(label_style_number(scale = 100, digits = 1)) |>
      unlist() |>
      paste(collapse = " ")
  )

  # check the CIs are correct for grade II
  expect_equal(
    df |>
      dplyr::filter(label == "II") |>
      dplyr::pull("ci_stat_0"),
    cardx::proportion_ci_wilson(
      x = trial$grade == "II",
      correct = TRUE
    )[c("conf.low", "conf.high")] |>
      map_chr(label_style_number(scale = 100, digits = 1)) |>
      unlist() |>
      paste(collapse = " ")
  )

  # now with a by variable
  expect_equal(
    trial |>
      tbl_summary(by = trt, include = response, percent='cell', statistic = ~"{n} / {N} ({p}%)", missing = "no") |>
      add_ci(
        statistic = ~"{conf.low} {conf.high}",
        style_fun = ~label_style_number(scale = 100, digits = 1)
      ) |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull("ci_stat_1"),
    cardx::proportion_ci_wilson(
      x =
        with(
          tidyr::drop_na(trial, c(trt, response)),
          trt == "Drug A" & response == 1
        ),
      correct = TRUE
    )[c("conf.low", "conf.high")] |>
      map_chr(label_style_number(scale = 100, digits = 1)) |>
      unlist() |>
      paste(collapse = " ")
  )
})

# Addresses Issue #2139
test_that("add_ci() correctly handles columns of all NAs", {
  expect_equal(
    trial |>
      mutate(death = ifelse(trt == "Drug B", NA, death)) |>
      tbl_summary(
        by = trt,
        missing = "no",
        include = death
      ) |>
      add_ci() |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull("ci_stat_2"),
    "NA%, NA%"
  )
})
