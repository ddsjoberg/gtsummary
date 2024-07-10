# TODO: add theme_gtsummary_journal('qjecon')
# TODO: add theme_gtsummary_printer()

# TODO: After inline_text() is added: theme_gtsummary_journal(c('jama', 'nejm')) with preprend_p examples

test_that("theme_gtsummary_compact() works", {
  expect_error(
    with_gtsummary_theme(
      theme_gtsummary_compact(),
      expr = trial |> tbl_summary(include = c(age, grade)) |> as_gt()
    ),
    NA
  )

  expect_error(
    with_gtsummary_theme(
      theme_gtsummary_compact(),
      expr = trial |> tbl_summary(include = c(age, grade)) |> as_hux_table()
    ),
    NA
  )

  expect_error(
    with_gtsummary_theme(
      theme_gtsummary_compact(),
      expr = trial |> tbl_summary(include = c(age, grade)) |> as_flex_table()
    ),
    NA
  )

  expect_error(
    with_gtsummary_theme(
      theme_gtsummary_compact(),
      expr = trial |> tbl_summary(include = c(age, grade)) |> as_kable_extra()
    ),
    NA
  )
})

test_that("theme_gtsummary_eda() works", {
  expect_snapshot(
    with_gtsummary_theme(
      theme_gtsummary_eda(),
      expr = trial |> tbl_summary(include = c(age, grade))
    ) |>
      as.data.frame()
  )
})

test_that("theme_gtsummary_mean_sd() works", {
  expect_equal(
    with_gtsummary_theme(
      theme_gtsummary_mean_sd(),
      expr = trial |> tbl_summary(include = age)
    ) |>
      getElement("inputs") |>
      getElement("statistic") |>
      getElement("age"),
    "{mean} ({sd})",
    ignore_attr = TRUE
  )

  expect_equal(
    with_gtsummary_theme(
      theme_gtsummary_mean_sd(),
      expr = trial |> tbl_summary(by = trt, include = age) |> add_p()
    ) |>
      getElement("table_body") |>
      getElement("test_name") |>
      dplyr::last(),
      "t.test",
    ignore_attr = TRUE
  )
})

test_that("theme_gtsummary_continuous2() works", {
  expect_equal(
    with_gtsummary_theme(
      theme_gtsummary_continuous2(),
      expr = trial |> tbl_summary(include = age)
    ) |>
      getElement("table_body") |>
      getElement("var_type") |>
      dplyr::last(),
    "continuous2"
  )
})

test_that("theme_gtsummary_language() works", {
  expect_equal(
    with_gtsummary_theme(
      theme_gtsummary_language("es"),
      expr = trial |> tbl_summary(include = age)
    ) |>
      getElement("table_body") |>
      getElement("label") |>
      dplyr::last(),
    "Desconocido"
  )

  expect_equal(
    with_gtsummary_theme(
      theme_gtsummary_language("es", big.mark = " ", decimal.mark = ",", iqr.sep = " - "),
      expr = trial |> tbl_summary(include = marker)
    ) |>
      getElement("table_body") |>
      getElement("stat_0") |>
      head(n = 1L),
    "0,64 (0,22 - 1,41)"
  )
})

test_that("theme_gtsummary_journal('lancet') works", {
  # R 4.1 wasn't working with the UTF8 midpoint character, but is working on release (4.4 as of May 2024)
  skip_if(.Platform$OS.type == "windows" && startsWith(R.version$version.string, "R version 4.1"))

  # check that we get
  #  - IQR separated with emdash in table
  #  - pvalues are rounded to 2 places
  #  - CI seperator is " to "
  expect_snapshot(
    with_gtsummary_theme(
      theme_gtsummary_journal("lancet"),
      expr = trial |>
        tbl_summary(by = trt, include = marker, label = marker ~ "marker", missing = "no") |>
        add_difference() |>
        modify_column_hide(c("stat_2")) |>
        as.data.frame()
    )
  )
  # check the footnote updated to IQR from Q1 - Q3
  expect_equal(
    with_gtsummary_theme(
      theme_gtsummary_journal("lancet"),
      expr = trial |>
        tbl_summary(by = trt, include = marker, label = marker ~ "marker", missing = "no") |>
        getElement("table_styling") |>
        getElement("footnote") |>
        dplyr::filter(.by = "column", dplyr::n() == dplyr::row_number(), column %in% c("stat_1", "stat_2")) |>
        dplyr::pull("footnote") |>
        unique()
    ),
    "Median (IQR)"
  )

  # check the prepend p-value function is used
  expect_true(
    with_gtsummary_theme(
      theme_gtsummary_journal("lancet"),
      lm(age ~ marker, trial) |>
        tbl_regression() |>
        inline_text(variable = marker)
    ) %>%
      {str_detect(., "p=") && endsWith(., "97)")}
  )
})

test_that("theme_gtsummary_journal('nejm') works", {
  # check that we get
  #  - IQR separated with emdash in table, and no percent symbol
  #  - pvalues are rounded to 2 places
  #  - CI separator is " to "
  expect_snapshot(
    with_gtsummary_theme(
      theme_gtsummary_journal("nejm"),
      expr = trial |>
        tbl_summary(by = trt, include = age, label = age ~ "Age", missing = "no") |>
        add_difference() |>
        modify_column_hide(c("stat_2")) |>
        as.data.frame()
    )
  )
  # check the footnote has IQR in it
  expect_equal(
    with_gtsummary_theme(
      theme_gtsummary_journal("nejm"),
      expr = trial |>
        tbl_summary(by = trt, include = age, label = age ~ "Age", missing = "no") |>
        getElement("table_styling") |>
        getElement("footnote") |>
        getElement("footnote") |>
        dplyr::last()
    ),
    "Median (IQR)"
  )
})

test_that("theme_gtsummary_journal('jama') works", {
  # check that we get
  #  - IQR separated with emdash in table
  #  - pvalues are rounded to 2 places
  #  - CI separator is " to "
  #  - estimate and CI are in the same cell with appropriate header
  expect_snapshot(
    with_gtsummary_theme(
      theme_gtsummary_journal("jama"),
      expr = trial |>
        tbl_summary(by = trt, include = age, label = age ~ "Age", missing = "no") |>
        add_difference() |>
        modify_column_hide(c("stat_2")) |>
        as.data.frame()
    )
  )

  # check that we get
  #  - pvalues are rounded to 2 places
  #  - CI separator is " to "
  #  - estimate and CI are in the same cell with approriate header
  expect_snapshot(
    with_gtsummary_theme(
      theme_gtsummary_journal("jama"),
      expr = lm(hp ~ am, mtcars) |>
        tbl_regression() |>
        as.data.frame()
    )
  )
})
