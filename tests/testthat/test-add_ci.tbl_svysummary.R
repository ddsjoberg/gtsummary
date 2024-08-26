skip_on_cran()
skip_if_not(is_pkg_installed(c("cardx", "survey"), reference_pkg = "gtsummary") && is_pkg_installed("broom", reference_pkg = "cardx"))
svy_trial <- survey::svydesign(~1, data = trial, weights = ~1)

test_that("add_ci(method) with no `by`", {
  svy_trial2 <- svy_trial
  svy_trial2$variables <-
    svy_trial2$variables |>
    dplyr::mutate(
      age_svymean = age,
      age_svymedian.mean = age,
      age_svymedian.beta = age,
      age_svymedian.xlogit = age,
      age_svymedian.asin = age,
      age_svymedian.score = age,

      response_svyprop.logit = response,
      grade_svyprop.logit = grade,
      grade_svyprop.likelihood = grade,
      grade_svyprop.asin = grade,
      grade_svyprop.beta = grade,
      grade_svyprop.mean = grade,
      grade_svyprop.xlogit = grade
    )

  # create a tbl with all the methods present
  expect_silent(
    tbl <-
      svy_trial2 %>%
      tbl_svysummary(
        include = c(starts_with("response_"), starts_with("grade_"), starts_with("age_")),
        missing = "no",
        label = imap(svy_trial2$variables, ~.y),
        statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
      ) |>
      add_ci(
        method = list(
          ends_with("svyprop.logit") ~ "svyprop.logit",
          ends_with("svyprop.likelihood") ~ "svyprop.likelihood",
          ends_with("svyprop.asin") ~ "svyprop.asin",
          ends_with("svyprop.beta") ~ "svyprop.beta",
          ends_with("svyprop.mean") ~ "svyprop.mean",
          ends_with("svyprop.xlogit") ~ "svyprop.xlogit",
          ends_with("svymean") ~ "svymean",
          ends_with("svymedian.mean") ~ "svymedian.mean",
          ends_with("svymedian.beta") ~ "svymedian.beta",
          ends_with("svymedian.xlogit") ~ "svymedian.xlogit",
          ends_with("svymedian.asin") ~ "svymedian.asin",
          ends_with("svymedian.score") ~ "svymedian.score"
        ),
        style_fun =
          list(all_continuous() ~ label_style_sigfig(digits = 4),
               all_categorical() ~ label_style_sigfig(digits = 4, scale = 100))
      )
  )

  # check svymean results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymean") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_continuous_ci(svy_trial, variables = age, method = "svymean") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svymedian.mean results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.mean") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_continuous_ci(svy_trial, variables = age, method = "svymedian.mean") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )


  # check svymedian.beta results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.beta") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_continuous_ci(svy_trial, variables = age, method = "svymedian.beta") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svymedian.xlogit results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.xlogit") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_continuous_ci(svy_trial, variables = age, method = "svymedian.xlogit") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svymedian.asin results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.asin") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_continuous_ci(svy_trial, variables = age, method = "svymedian.asin") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svymedian.score results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.score") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_continuous_ci(svy_trial, variables = age, method = "svymedian.score") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.logit
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "response_svyprop.logit") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(svy_trial, variables = response, value = ~1, method = "logit") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.logit
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.logit", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(svy_trial, variables = grade, value = ~"I", method = "logit") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.likelihood
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.likelihood", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(svy_trial, variables = grade, value = ~"I", method = "likelihood") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.asin
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.asin", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(svy_trial, variables = grade, value = ~"I", method = "asin") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.beta
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.beta", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(svy_trial, variables = grade, value = ~"I", method = "beta") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.mean
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.mean", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(svy_trial, variables = grade, value = ~"I", method = "mean") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.xlogit
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.xlogit", label == "I") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_categorical_ci(svy_trial, variables = grade, value = ~"I", method = "xlogit") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )
})

test_that("add_ci(method) with `by`", {
  svy_trial2 <- svy_trial
  svy_trial2$variables <-
    svy_trial2$variables |>
    dplyr::mutate(
      age_svymean = age,
      age_svymedian.mean = age,
      age_svymedian.beta = age,
      age_svymedian.xlogit = age,
      age_svymedian.asin = age,
      age_svymedian.score = age,

      response_svyprop.logit = response,
      grade_svyprop.logit = grade,
      grade_svyprop.likelihood = grade,
      grade_svyprop.asin = grade,
      grade_svyprop.beta = grade,
      grade_svyprop.mean = grade,
      grade_svyprop.xlogit = grade
    )

  # create a tbl with all the methods present
  expect_silent(
    tbl <-
      svy_trial2 %>%
      tbl_svysummary(
        by = trt,
        include = c(starts_with("response_"), starts_with("grade_"), starts_with("age_")),
        missing = "no",
        label = imap(svy_trial2$variables, ~.y),
        statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
      ) |>
      add_ci(
        method = list(
          ends_with("svyprop.logit") ~ "svyprop.logit",
          ends_with("svyprop.likelihood") ~ "svyprop.likelihood",
          ends_with("svyprop.asin") ~ "svyprop.asin",
          ends_with("svyprop.beta") ~ "svyprop.beta",
          ends_with("svyprop.mean") ~ "svyprop.mean",
          ends_with("svyprop.xlogit") ~ "svyprop.xlogit",
          ends_with("svymean") ~ "svymean",
          ends_with("svymedian.mean") ~ "svymedian.mean",
          ends_with("svymedian.beta") ~ "svymedian.beta",
          ends_with("svymedian.xlogit") ~ "svymedian.xlogit",
          ends_with("svymedian.asin") ~ "svymedian.asin",
          ends_with("svymedian.score") ~ "svymedian.score"
        ),
        style_fun =
          list(all_continuous() ~ label_style_sigfig(digits = 4),
               all_categorical() ~ label_style_sigfig(digits = 4, scale = 100))
      )
  )

  # check svymean results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymean") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_continuous_ci(svy_trial, variables = age, by = trt, method = "svymean") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svymedian.mean results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.mean") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_continuous_ci(svy_trial, variables = age, by = trt, method = "svymedian.mean") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )


  # check svymedian.beta results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.beta") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_continuous_ci(svy_trial, variables = age, by = trt, method = "svymedian.beta") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svymedian.xlogit results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.xlogit") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_continuous_ci(svy_trial, variables = age, by = trt, method = "svymedian.xlogit") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svymedian.asin results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.asin") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_continuous_ci(svy_trial, variables = age, by = trt, method = "svymedian.asin") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svymedian.score results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svymedian.score") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_continuous_ci(svy_trial, variables = age, by = trt, method = "svymedian.score") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.logit
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "response_svyprop.logit") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(svy_trial, variables = response, by = trt, value = ~1, method = "logit") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.logit
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.logit", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(svy_trial, variables = grade, by = trt, value = ~"I", method = "logit") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.likelihood
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.likelihood", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(svy_trial, variables = grade, by = trt, value = ~"I", method = "likelihood") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.asin
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.asin", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(svy_trial, variables = grade, by = trt, value = ~"I", method = "asin") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.beta
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.beta", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(svy_trial, variables = grade, by = trt, value = ~"I", method = "beta") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.mean
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.mean", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(svy_trial, variables = grade, by = trt, value = ~"I", method = "mean") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )

  # check svyprop.xlogit
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svyprop.xlogit", label == "I") |>
      dplyr::pull(ci_stat_1),
    cardx::ard_categorical_ci(svy_trial, variables = grade, by = trt, value = ~"I", method = "xlogit") |>
      dplyr::filter(group1_level %in% "Drug A") |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), \(x) style_sigfig(x, digits = 4, scale = 100) %>% paste0("%"), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )
})

test_that("add_ci(method) after `add_overall()`", {
  # create a tbl with all the methods present
  expect_snapshot(
    svy_trial |>
      tbl_svysummary(
        by = trt,
        include = c(age, grade),
        missing = "no",
        statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
      ) |>
      add_overall() |>
      add_ci() |>
      as.data.frame(col_label = FALSE) |>
      dplyr::select(-all_stat_cols())
  )
})



test_that("add_ci(include)", {
  expect_snapshot(
    tbl_svysummary(
      svy_trial,
      include = c(age, grade),
      by = trt,
      missing = "no"
    ) |>
      add_overall() |>
      add_ci(include = age)  |>
      as.data.frame(col_label = FALSE) |>
      dplyr::select(-all_stat_cols())
  )
})

test_that("add_ci(statistic)", {
  expect_snapshot(
    tbl_svysummary(
      svy_trial,
      include = c(age, grade),
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
  # create a tbl with all the methods present
  expect_silent(
    tbl <-
      svy_trial %>%
      tbl_svysummary(
        include = age,
        missing = "no",
        statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
      ) |>
      add_ci(conf.level = 0.80, style_fun = ~label_style_sigfig(digits = 4))
  )

  # check svymean results
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age") |>
      dplyr::pull(ci_stat_0),
    cardx::ard_continuous_ci(svy_trial, variables = age, method = "svymean", conf.level = 0.80) |>
      dplyr::mutate(
        fmt_fn = map2(
          fmt_fn, stat_name,
          ~ ifelse(.y %in% c("conf.low", "conf.high"), label_style_sigfig(digits = 4), .x)
        )
      ) |>
      cards::apply_fmt_fn() |>
      cards::get_ard_statistics(.column = "stat_fmt") %>%
      {glue("{conf.low}, {conf.high}", .envir = .)} # styler: off
  )
})

test_that("add_ci(pattern)", {
  expect_snapshot(
    tbl_svysummary(
      svy_trial,
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
    tbl_svysummary(
      svy_trial,
      include = age,
      missing = "no",
      statistic = list(age = "{mean}")
    ) |>
      add_ci(pattern = "{not_a_stat} [{ci}]")
  )

  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
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
    tbl_svysummary(
      svy_trial,
      include = age,
      missing = "no",
      statistic = list(age = "{mean}")
    ) |>
      add_ci(method = list(age = "xxxxxxxxxx"))
  )

  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
      include = grade
    ) |>
      add_ci(method = list(grade = "svymean"))
  )
})

test_that("add_ci() correctly handles dichotomous variables", {
  expect_silent(
    tbl <- tbl_svysummary(
      svy_trial,
      include = c(response, grade),
      value = list(response = 0, grade = "III"),
      missing = "no"
    ) |>
      add_ci()
  )
  expect_snapshot(as.data.frame(tbl))

  expect_equal(
    tbl$inputs$value |> lapply(as.character),
    tbl$cards$add_ci[c("variable", "variable_level")] |> unique() |> deframe()
  )
})

test_that("add_ci() messaging for tbl_svysummary(percent)", {
  expect_message({
    data(api, package = "survey")
    survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
      tbl_svysummary(
        by = "both",
        include = stype,
        percent = "row"
      ) |>
      add_ci()},
    "function is meant to work with"
  )
})
