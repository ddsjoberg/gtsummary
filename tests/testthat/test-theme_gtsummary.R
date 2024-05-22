# TODO: add theme_gtsummary_journal('qjecon')
# TODO: add theme_gtsummary_compact()
# TODO: add theme_gtsummary_printer()
# TODO: add theme_gtsummary_language()
# TODO: add theme_gtsummary_continuous2()
# TODO: add theme_gtsummary_mean_sd()
# TODO: add theme_gtsummary_eda()

# TODO: After inline_text() is added: theme_gtsummary_journal(c('jama', 'nejm', 'lancet')) with preprend_p examples

test_that("theme_gtsummary_journal('lancet') works", {
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
