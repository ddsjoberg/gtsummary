skip_on_cran()

test_that("setting themes", {
  expect_error(
    theme_gtsummary_journal("jama"),
    NA)

  expect_error(
    set_gtsummary_theme(theme_gtsummary_journal("jama")),
    NA)

  expect_error(
    set_gtsummary_theme(theme_gtsummary_journal("nejm")),
    NA)

  expect_error(
    set_gtsummary_theme(theme_gtsummary_journal("lancet")),
    NA)

  expect_error(
    theme_gtsummary_continuous2(),
    NA)

  expect_error(
    theme_gtsummary_printer(),
    NA)

  expect_error(
    theme_gtsummary_compact(),
    NA)

  expect_error(
    set_gtsummary_theme(theme_gtsummary_language("fr", big.mark = " ", decimal.mark = ",")),
    NA)

  expect_error(
    tbl <- tbl_summary(trial, by = trt) %>% add_p() %>% add_stat_label(),
    NA)

  expect_equal(
    tbl$table_styling$footnote %>% dplyr::filter(column == "p.value") %>% purrr::pluck("footnote"),
    "test de Wilcoxon-Mann-Whitney; test du khi-deux d'indépendance",
    ignore_attr = TRUE
  )

  expect_error(
    theme_gtsummary_mean_sd(),
    NA)

  expect_error(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      as_gt(),
    NA)

  expect_error(
    reset_gtsummary_theme(),
    NA)

  expect_error(
    set_gtsummary_theme(list("not_a_theme_element" = 0)),
    "*")

  expect_error(
    get_theme_element("not_a_theme_element"),
    "*")

  # setting a continuous2 default stat
  list(
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-str:continuous_stat" = c("{median} ({p25} - {p75})", "{mean} ({sd})"),
    "tbl_summary-str:categorical_stat" = "{n} / {N} ({p}%)"
  ) %>%
    set_gtsummary_theme()

  expect_error(tbl_theme <- tbl_summary(trial[c("trt", "age")]), NA)
  expect_equal(tbl_theme$meta_data$stat_display,
               list("{n} / {N} ({p}%)", c("{median} ({p25} - {p75})", "{mean} ({sd})")))
  reset_gtsummary_theme()
})


