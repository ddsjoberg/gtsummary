skip_on_cran()
skip_if_pkg_not_installed("survey")

svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
svy_trial <- survey::svydesign(~1, data = trial, weights = ~1)

test_that("add_difference.tbl_svysummary() snapshots of common outputs", {
  skip_if_pkg_not_installed("smd")
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
  skip_if_pkg_not_installed("smd")
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
  skip_if_pkg_not_installed("smd")
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
    cardx::ard_emmeans_contrast(svy_trial2,
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
    cardx::ard_emmeans_contrast(svy_trial2,
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
  skip_if_pkg_not_installed("smd")
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

test_that("add_difference.tbl_svysummary(test = 'emmeans') dichotomous sign matches displayed value (#2399)", {
  skip_if_pkg_not_installed("emmeans", ref = "cardx")

  # Titanic `Age` has levels c("Child", "Adult"); the displayed `value`
  # ("Child") is the FIRST factor level. Before the fix, emmeans modeled
  # P(last level = "Adult"), flipping the sign of the displayed P(Child).
  df_titanic <- as.data.frame(Titanic)
  df_titanic <- df_titanic[rep(seq_len(nrow(df_titanic)), df_titanic$Freq), ]
  prop_tbl <- prop.table(table(df_titanic$Age, df_titanic$Survived), margin = 2)
  expected_diff <- unname(prop_tbl["Child", "No"] - prop_tbl["Child", "Yes"])

  est_child <-
    suppressWarnings(
      tbl_svysummary(svy_titanic, by = Survived, value = list(Age = "Child"), include = Age) |>
        add_difference(test = list(Age ~ "emmeans"))
    )$table_body |>
    dplyr::filter(.data$variable == "Age", .data$row_type == "label") |>
    dplyr::pull("estimate")

  # estimate must reflect P(Child | group1) - P(Child | group2) (correct sign)
  expect_equal(est_child, expected_diff, tolerance = 1e-6)
})

test_that("add_difference.tbl_svysummary(levels) selects two groups when by has 3+ levels", {
  skip_if_pkg_not_installed("broom", ref = "cardx")

  # selecting two groups from a 3-level `by` runs without error and keeps all stat cols
  expect_error(
    tbl_diff <-
      svy_trial |>
      tbl_svysummary(by = grade, include = c(age, marker), missing = "no") |>
      add_difference(levels = c("I", "III")),
    NA
  )
  # all 3 original stat columns are retained
  expect_equal(
    sum(grepl("^stat_\\d+$", tbl_diff$table_styling$header$column)),
    3L
  )
  # difference result columns are added (estimate, CI, p-value)
  expect_true(all(c("estimate", "conf.low", "conf.high", "p.value") %in% names(tbl_diff$table_body)))

  # the returned object retains the original full design (not the subset)
  expect_true(inherits(tbl_diff$inputs$data, "survey.design"))
  expect_equal(nrow(tbl_diff$inputs$data$variables), nrow(trial))
  expect_setequal(
    as.character(unique(tbl_diff$inputs$data$variables$grade)),
    as.character(unique(trial$grade))
  )

  # estimate equals levels[1] - levels[2] (I - III)
  est_I_III <-
    tbl_diff$cards$add_difference$age |>
    dplyr::filter(stat_name == "estimate") |>
    dplyr::pull("stat") |>
    unlist()
  expected <-
    svy_trial |>
    subset(grade %in% c("I", "III")) |>
    (\(d) {
      d$variables$grade <- factor(as.character(d$variables$grade), levels = c("I", "III"))
      survey::svyttest(age ~ grade, design = d)
    })() |>
    getElement("estimate") |>
    unname()
  expect_equal(est_I_III, expected, ignore_attr = TRUE)
})

test_that("add_difference.tbl_svysummary(levels) flips sign when levels are reversed", {
  skip_if_pkg_not_installed("broom", ref = "cardx")

  est_fun <- function(lvls) {
    svy_trial |>
      tbl_svysummary(by = grade, include = age, missing = "no") |>
      add_difference(levels = lvls) |>
      getElement("cards") |>
      getElement("add_difference") |>
      getElement("age") |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::pull("stat") |>
      unlist()
  }
  expect_equal(est_fun(c("I", "III")), -est_fun(c("III", "I")))
})

test_that("add_difference.tbl_svysummary(levels) works for two-level by (flip direction)", {
  skip_if_pkg_not_installed("broom", ref = "cardx")

  est_fun <- function(...) {
    svy_trial |>
      tbl_svysummary(by = trt, include = age, missing = "no") |>
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

test_that("add_difference.tbl_svysummary(levels) validation errors", {
  # 3+ levels and no `levels` -> informative error pointing to `levels`
  expect_error(
    svy_trial |>
      tbl_svysummary(by = grade, include = age, missing = "no") |>
      add_difference(),
    "levels"
  )
  # wrong length
  expect_error(
    svy_trial |>
      tbl_svysummary(by = grade, include = age, missing = "no") |>
      add_difference(levels = "I"),
    "length-two"
  )
  # non-existent level
  expect_error(
    svy_trial |>
      tbl_svysummary(by = grade, include = age, missing = "no") |>
      add_difference(levels = c("I", "X")),
    "not present|one of"
  )
  # duplicated level
  expect_error(
    svy_trial |>
      tbl_svysummary(by = grade, include = age, missing = "no") |>
      add_difference(levels = c("I", "I")),
    "distinct"
  )
})

test_that("add_difference.tbl_svysummary(levels) adds footnote naming compared pair", {
  skip_if_pkg_not_installed("broom", ref = "cardx")

  tbl_diff <-
    svy_trial |>
    tbl_svysummary(by = grade, include = age, missing = "no") |>
    add_difference(levels = c("I", "III"))
  footnotes <- tbl_diff$table_styling$footnote_header$footnote
  expect_true(any(grepl("I - III", footnotes)))
})

