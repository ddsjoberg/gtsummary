skip_on_cran()
skip_if_not(is_pkg_installed(c("survey", "cardx")))

svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
svy_trial <- survey::svydesign(~1, data = trial, weights = ~1)

test_that("add_difference.tbl_svysummary() snapshots of common outputs", {
  expect_snapshot(
    tbl_svysummary(svy_trial, by = trt) |>
      add_difference() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )

  expect_snapshot(
    tbl_svysummary(svy_titanic, by = Survived) |>
      add_difference() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )
})

test_that("add_difference.tbl_svysummary(x) messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_trial, include = age) |>
      add_difference()
  )

  expect_snapshot(
    tbl <- tbl_svysummary(svy_trial, by = trt, percent = "row", include = grade) |>
      add_difference()
  )
})

test_that("add_difference.tbl_svysummary(test)", {
  svy_trial2 <- svy_trial
  svy_trial2$variables$age_svy.t.test <- svy_trial2$variables$age
  svy_trial2$variables$grade_smd <- svy_trial2$variables$grade
  svy_trial2$variables$age_emmeans <- svy_trial2$variables$age
  svy_trial2$variables$response_emmeans <- svy_trial2$variables$response

  expect_error(
    tbl <-
      tbl_svysummary(
        svy_trial2,
        by = trt,
        include = c(starts_with("age_"), starts_with("grade_"), response_emmeans),
        label = svy_trial2$variables |> imap(~.y),
        missing = "no"
      ) |>
      add_difference(
        pvalue_fun = label_style_pvalue(digits = 3),
        test = list(
          contains("svy.t.test") ~ survey::svyttest,
          contains("smd") ~ "smd",
          contains("emmeans") ~ "emmeans"
        )
      ),
    NA
  )

  # check the differences are correct
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svy.t.test", row_type == "label") |>
      dplyr::select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))) |>
      dplyr::mutate_all(unname) |>
      unlist() |>
      as.list() |>
      discard(is.na) %>%
      {.[order(names(.))]}, # styler: off
    cardx::ard_survey_svyttest(svy_trial2,
                               variables = age_svy.t.test,
                               by = trt) |>
      cards::get_ard_statistics(
        stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")
      ) %>%
      {.[order(names(.))]}, # styler: off
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_smd", row_type == "label") |>
      dplyr::select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))) |>
      unlist() |>
      as.list() |>
      discard(is.na),
    cardx::ard_smd_smd(svy_trial2,
                       variables = grade_smd,
                       by = trt) |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_emmeans", row_type == "label") |>
      dplyr::select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))) |>
      unlist() |>
      as.list(),
    cardx::ard_emmeans_mean_difference(svy_trial2,
                                       method = survey::svyglm,
                                       formula = age_emmeans ~ trt,
                                       response_type = "continuous") |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "response_emmeans", row_type == "label") |>
      dplyr::select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))) |>
      unlist() |>
      as.list(),
    cardx::ard_emmeans_mean_difference(svy_trial2,
                                       method = survey::svyglm,
                                       method.args = list(family = binomial),
                                       formula = response_emmeans ~ trt,
                                       response_type = "dichotomous") |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")),
    ignore_attr = TRUE
  )

  # test we can pass custom functions
  expect_equal(
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_difference(test = age ~ cardx::ard_survey_svyttest, pvalue_fun = label_style_sigfig(digits = 4)) |>
      as.data.frame(),
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_difference(test = age ~ "svy.t.test", pvalue_fun = label_style_sigfig(digits = 4)) |>
      as.data.frame()
  )
})

test_that("add_difference.tbl_svysummary() + add_p.tbl_svysummary()", {
  expect_snapshot(
    svy_trial |>
      tbl_svysummary(by = trt, include = c(age, response, grade), missing = "no") |>
      add_difference(test = ~"smd") |>
      add_p() |>
      as.data.frame(col_label = FALSE) |>
      dplyr::select(-all_stat_cols())
  )
})

test_that("add_difference.tbl_svysummary(test) messaging", {
  # bad test value
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_difference(test = age ~ letters)
  )
})

test_that("add_difference.tbl_svysummary(pvalue_fun)", {
  expect_equal(
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_difference(pvalue_fun = label_style_pvalue(digits = 3)) |>
      getElement("table_body") |>
      getElement("p.value") |>
      getElement(1L) |>
      style_pvalue(digits = 3),
    "0.834"
  )

  expect_error(
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_difference(pvalue_fun = letters),
    "Can't convert `pvalue_fun`, a character vector, to a function"
  )
})

test_that("add_difference.tbl_svysummary(include)", {
  # should only have the one p-value
  expect_length(
    tbl_svysummary(svy_trial, by = trt, include = c(age, grade)) |>
      add_difference(include = age) |>
      getElement("table_body") |>
      getElement("estimate") |>
      na.omit(),
    1L
  )
})

