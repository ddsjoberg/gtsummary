skip_on_cran()
skip_if_not(is_pkg_installed(c("survey", "cardx")))

svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
svy_trial <- survey::svydesign(~1, data = trial, weights = ~1)

test_that("add_p.tbl_svysummary() snapshots of common outputs", {
  expect_snapshot(
    tbl_svysummary(svy_trial, by = grade) |>
      add_p() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )

  expect_snapshot(
    tbl_svysummary(svy_titanic, by = Survived) |>
      add_p() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )
})

test_that("add_p.tbl_svysummary(x) messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_trial) |>
      add_p()
  )
})

test_that("add_p.tbl_svysummary(test)", {
  svy_trial2 <- svy_trial
  svy_trial2$variables$age_svy.t.test <- svy_trial2$variables$age
  svy_trial2$variables$age_svy.wilcox.test <- svy_trial2$variables$age
  svy_trial2$variables$age_svy.kruskal.test <- svy_trial2$variables$age
  svy_trial2$variables$age_svy.vanderwaerden.test <- svy_trial2$variables$age
  svy_trial2$variables$age_svy.median.test <- svy_trial2$variables$age
  svy_trial2$variables$grade_svy.chisq.test <- svy_trial2$variables$grade
  svy_trial2$variables$grade_svy.adj.chisq.test <- svy_trial2$variables$grade
  svy_trial2$variables$grade_svy.wald.test <- svy_trial2$variables$grade
  svy_trial2$variables$grade_svy.adj.wald.test <- svy_trial2$variables$grade
  svy_trial2$variables$grade_svy.lincom.test <- svy_trial2$variables$grade
  svy_trial2$variables$grade_svy.saddlepoint.test <- svy_trial2$variables$grade
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
      add_p(
        pvalue_fun = label_style_pvalue(digits = 3),
        test = list(
          contains("svy.t.test") ~ survey::svyttest,
          contains("svy.wilcox.test") ~ "svy.wilcox.test",
          contains("svy.kruskal.test") ~ "svy.kruskal.test",
          contains("svy.vanderwaerden.test") ~ "svy.vanderwaerden.test",
          contains("svy.median.test") ~ "svy.median.test",
          contains("svy.chisq.test") ~ survey::svychisq,
          contains("svy.adj.chisq.test") ~ "svy.adj.chisq.test",
          contains("svy.wald.test") ~ "svy.wald.test",
          contains("svy.adj.wald.test") ~ "svy.adj.wald.test",
          contains("svy.lincom.test") ~ "svy.lincom.test",
          contains("svy.saddlepoint.test") ~ "svy.saddlepoint.test",
          contains("emmeans") ~ "emmeans"
        )
      ),
    NA
  )

  # check the p-values are correct
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "response_emmeans", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_emmeans_mean_difference(svy_trial2,
                                       method = survey::svyglm,
                                       method.args = list(family = binomial),
                                       formula = response_emmeans ~ trt,
                                       response_type = "dichotomous") |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_emmeans", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_emmeans_mean_difference(svy_trial2,
                                       method = survey::svyglm,
                                       formula = age_emmeans ~ trt,
                                       response_type = "continuous") |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svy.saddlepoint.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svychisq(svy_trial2, variables = "grade_svy.saddlepoint.test",
                               by = trt, statistic = 'saddlepoint') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svy.saddlepoint.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svychisq(svy_trial2, variables = "grade_svy.saddlepoint.test",
                               by = trt, statistic = 'saddlepoint') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svy.lincom.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svychisq(svy_trial2, variables = "grade_svy.lincom.test",
                               by = trt, statistic = 'lincom') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svy.adj.wald.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svychisq(svy_trial2, variables = "grade_svy.adj.wald.test",
                               by = trt, statistic = 'adjWald') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svy.wald.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svychisq(svy_trial2, variables = "grade_svy.wald.test",
                               by = trt, statistic = 'Wald') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svy.adj.chisq.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svychisq(svy_trial2, variables = "grade_svy.adj.chisq.test",
                               by = trt, statistic = 'Chisq') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svy.chisq.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svychisq(svy_trial2, variables = "grade_svy.chisq.test",
                               by = trt, statistic = 'F') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "grade_svy.chisq.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svychisq(svy_trial2, variables = "grade_svy.chisq.test",
                               by = trt, statistic = 'F') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svy.median.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svyranktest(svy_trial2, variables = "age_svy.median.test",
                                  by = trt, test = 'median') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svy.vanderwaerden.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svyranktest(svy_trial2, variables = "age_svy.vanderwaerden.test",
                                  by = trt, test = 'vanderWaerden') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svy.kruskal.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svyranktest(svy_trial2, variables = "age_svy.kruskal.test",
                                  by = trt, test = 'KruskalWallis') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svy.wilcox.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svyranktest(svy_trial2, variables = "age_svy.wilcox.test",
                                  by = trt, test = 'wilcoxon') |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age_svy.t.test", row_type == "label") |>
      dplyr::pull(p.value),
    cardx::ard_survey_svyttest(svy_trial2, variables = age_svy.t.test, by = trt) |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull("stat") |>
      unlist(),
    ignore_attr = TRUE
  )

  # test we can pass custom functions
  expect_equal(
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_p(test = age ~ cardx::ard_survey_svyttest, pvalue_fun = label_style_sigfig(digits = 4)) |>
      as.data.frame(),
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_p(test = age ~ "svy.t.test", pvalue_fun = label_style_sigfig(digits = 4)) |>
      as.data.frame()
  )
})

test_that("add_p.tbl_svysummary(test) messaging", {
  # bad test value
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_p(test = age ~ letters)
  )
})

test_that("add_p.tbl_svysummary(pvalue_fun)", {
  expect_equal(
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_p(pvalue_fun = label_style_pvalue(digits = 3)) |>
      getElement("table_body") |>
      getElement("p.value") |>
      getElement(1L) |>
      style_pvalue(digits = 3),
    "0.718"
  )

  expect_error(
    tbl_svysummary(svy_trial, by = trt, include = age) |>
      add_p(pvalue_fun = letters),
    "Can't convert `pvalue_fun`, a character vector, to a function"
  )
})

test_that("add_p.tbl_svysummary(include)", {
  # should only have the one p-value
  expect_length(
    tbl_svysummary(svy_trial, by = trt, include = c(age, grade)) |>
      add_p(include = age) |>
      getElement("table_body") |>
      getElement("p.value") |>
      na.omit(),
    1L
  )
})

