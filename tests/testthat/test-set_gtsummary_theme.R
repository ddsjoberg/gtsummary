skip_on_cran()

test_that("setting themes", {
  expect_error(
    theme_gtsummary_journal("jama"),
    NA
  )

  expect_error(
    set_gtsummary_theme(theme_gtsummary_journal("jama")),
    NA
  )

  expect_error(
    set_gtsummary_theme(theme_gtsummary_journal("nejm")),
    NA
  )

  expect_error(
    set_gtsummary_theme(theme_gtsummary_journal("lancet")),
    NA
  )

  expect_error(
    theme_gtsummary_continuous2(),
    NA
  )

  expect_error(
    theme_gtsummary_printer(),
    NA
  )

  expect_error(
    theme_gtsummary_compact(),
    NA
  )

  expect_error(
    set_gtsummary_theme(theme_gtsummary_language("fr", big.mark = " ", decimal.mark = ",")),
    NA
  )

  expect_error(
    tbl <- tbl_summary(trial, by = trt) %>% add_p() %>% add_stat_label(),
    NA
  )

  expect_equal(
    tbl$table_styling$footnote %>% dplyr::filter(column == "p.value") %>% purrr::pluck("footnote"),
    "test de Wilcoxon-Mann-Whitney; test du khi-deux d'indépendance",
    ignore_attr = TRUE
  )

  expect_error(
    theme_gtsummary_mean_sd(),
    NA
  )

  expect_error(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      as_gt(),
    NA
  )

  expect_error(
    reset_gtsummary_theme(),
    NA
  )

  expect_error(
    set_gtsummary_theme(list("not_a_theme_element" = 0)),
    "*"
  )

  expect_error(
    get_theme_element("not_a_theme_element"),
    "*"
  )

  # setting a continuous2 default stat
  list(
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-str:continuous_stat" = c("{median} ({p25} - {p75})", "{mean} ({sd})"),
    "tbl_summary-str:categorical_stat" = "{n} / {N} ({p}%)"
  ) %>%
    set_gtsummary_theme()

  expect_error(tbl_theme <- tbl_summary(trial[c("trt", "age")]), NA)
  expect_equal(
    tbl_theme$meta_data$stat_display,
    list("{n} / {N} ({p}%)", c("{median} ({p25} - {p75})", "{mean} ({sd})"))
  )


  reset_gtsummary_theme()
  theme_gtsummary_journal("jama")

  expect_equal(
    lm(age ~ grade, trial) %>%
      tbl_regression() %>%
      as_tibble(col_labels = FALSE) %>%
      purrr::pluck("estimate"),
    c(NA, NA, "1.4 (-3.6 to 6.4)", "2.0 (-3.1 to 7.0)")
  )

  tbl1 <-
    trial %>%
    select(age, response, trt) %>%
    tbl_summary(by = trt, missing = "no") %>%
    add_difference() %>%
    as_tibble(col_labels = FALSE)
  expect_equal(
    tbl1 %>%
      purrr::pluck("estimate"),
    c("-0.44 (-4.6 to 3.7)", "-4.2% (-18% to 9.9%)")
  )
  expect_equal(
    tbl1 %>%
      purrr::pluck("estimate"),
    c("-0.44 (-4.6 to 3.7)", "-4.2% (-18% to 9.9%)")
  )

  reset_gtsummary_theme()
  list(
    "tbl_regression-str:coef_header" =
      rlang::expr(ifelse(exponentiate == TRUE, "exp(coef)", "coef"))
  ) %>%
    set_gtsummary_theme()

  expect_equal(
    lm(age ~ grade, trial) %>%
      tbl_regression() %>%
      as_tibble(col_labels = TRUE) %>%
      names() %>%
      purrr::pluck(2),
    "**coef**"
  )
  reset_gtsummary_theme()


  theme_gtsummary_journal("qjecon")
  expect_error(
    tbl_qjecon1 <-
      lm(age ~ grade, trial) %>%
      tbl_regression() %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    tbl_qjecon1 %>%
      pull(estimate),
    c(NA, NA, "1.4\n(2.54)", "2.0\n(2.55)")
  )
  expect_false("ci" %in% names(tbl_qjecon1))

  expect_error(
    tbl_qjecon2 <-
      mtcars %>%
      tbl_uvregression(method = lm, y = hp) %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_false("ci" %in% names(tbl_qjecon2))

  reset_gtsummary_theme()
  theme_gtsummary_eda()
  expect_error(
    tbl <-
      trial %>%
      select(age) %>%
      tbl_summary() %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(tbl$label, c("Age", "Median (IQR)", "Mean (SD)", "Range", "Unknown"))
  expect_equal(tbl$stat_0, c(NA, "47 (38, 57)", "47 (14)", "6, 83", "11"))

  reset_gtsummary_theme()

  expect_equal({
    theme_gtsummary_journal("lancet")
    with_gtsummary_theme(
      x = list("style_number-arg:decimal.mark" = "."),
      expr = tbl_summary(trial, include = marker, missing = "no") %>% as_tibble(col_labels = FALSE)
    ) %>%
      dplyr::pull(stat_0)},
    "0.64 (0.22 – 1.39)"
  )

  expect_equal({
    theme_gtsummary_journal("lancet")
    with_gtsummary_theme(
      x = list("style_number-arg:decimal.mark" = "."),
      expr = tbl_summary(trial, include = marker, missing = "no") %>% as_tibble(col_labels = FALSE)
    )
    tbl_summary(trial, include = marker, missing = "no") %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull(stat_0)},
    "0·64 (0·22 – 1·39)"
  )

  reset_gtsummary_theme()

})

reset_gtsummary_theme()
