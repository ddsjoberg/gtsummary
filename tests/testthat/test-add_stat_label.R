skip_on_cran()

test_that("add_stat_label(location='row') standard use", {
  tbl <- trial |> tbl_summary(by = trt)

  expect_snapshot(
    tbl |>
      add_stat_label(location='row') |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )
})

test_that("add_stat_label(location='column') standard use", {
  tbl <- trial |> tbl_summary(by = trt)

  expect_snapshot(
    tbl |>
      add_stat_label(location='column') |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )

  expect_snapshot(
    tbl |>
      add_stat_label(location = "column", label = all_categorical() ~ "no. (%)") |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )
})

test_that("add_stat_label(label) standard use", {
  expect_snapshot(
    trial |>
      tbl_summary(
        include = c(age, grade, trt),
        by = trt,
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}"),
      ) |>
      add_stat_label(label = age ~ c("Median (IQR)", "Range")) |>
      as.data.frame()
  )

  # passing NA wont add labels to those variables
  expect_snapshot(
    trial |>
      tbl_summary(type = age ~ 'continuous2', include = c(age, response), missing = "no") |>
      add_stat_label(
        label = list(all_continuous() ~ 'Median (IQR)', all_categorical() ~ NA_character_),
        location = "row"
      ) |>
      as.data.frame()
  )
})

test_that("add_stat_label(label) messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(
        include = c(age, trt),
        by = trt,
      ) |>
      add_stat_label(label = age ~ letters)
  )

 expect_snapshot(
   error = TRUE,
   trial |>
      tbl_summary(
        include = c(age, grade, trt),
        by = trt,
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}"),
      ) |>
      add_stat_label(label = age ~ c("Median (IQR)", "Range", "TOO LONG!"))
  )

  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(
        include = c(age, grade, trt),
        by = trt,
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}"),
      ) |>
      add_stat_label(label = age ~ c("{mean}" = "Mean"))
  )
})

test_that("add_stat_label(location) messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(include = age) |>
      add_stat_label(location = "colum")
  )
})

test_that("add_stat_label(label) with named continuous2 labels", {
  tbl <-
    trial |>
    tbl_summary(
      include = c(age, marker),
      type = all_continuous() ~ "continuous2",
      statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min} - {max}"),
      missing = "no"
    )

  expect_equal(
    tbl |>
      add_stat_label(label = age ~ c("{min} - {max}" = "Range", "{mean} ({sd})" = "Mean (SD)")) |>
      getElement("table_body") |>
      dplyr::filter(row_type == "level") |>
      dplyr::pull(label),
    c("Mean (SD)", "Median (Q1, Q3)", "Range", "Mean (SD)", "Median (Q1, Q3)", "Min - Max")
  )

  # names may also be the labels currently shown in the table, or a mix of both
  expect_equal(
    tbl |>
      add_stat_label(label = age ~ c("Min - Max" = "Range", "{mean} ({sd})" = "Mean (SD)")) |>
      getElement("table_body") |>
      dplyr::filter(row_type == "level") |>
      dplyr::pull(label),
    c("Mean (SD)", "Median (Q1, Q3)", "Range", "Mean (SD)", "Median (Q1, Q3)", "Min - Max")
  )

  # labels are matched against the current labels, not the updated ones
  expect_equal(
    tbl |>
      add_stat_label(label = age ~ c("Min - Max" = "Median (Q1, Q3)", "Median (Q1, Q3)" = "IQR")) |>
      getElement("table_body") |>
      dplyr::filter(row_type == "level", variable == "age") |>
      dplyr::pull(label),
    c("Mean (SD)", "IQR", "Median (Q1, Q3)")
  )

  # translated labels are matched
  expect_equal(
    with_gtsummary_theme(
      theme_gtsummary_language("fr"),
      tbl_summary(trial, include = age, type = age ~ "continuous2", statistic = age ~ c("{mean}", "{sd}"), missing = "no") |>
        add_stat_label(label = age ~ c("ET" = "Écart type"))
    ) |>
      getElement("table_body") |>
      dplyr::filter(row_type == "level") |>
      dplyr::pull(label),
    c("Moyenne", "Écart type")
  )
})

test_that("add_stat_label(label) with non-statistic names falls back to positional matching", {
  tbl <-
    trial |>
    tbl_summary(
      include = age,
      type = age ~ "continuous2",
      statistic = age ~ c("{N_nonmiss}", "{mean}", "{sd}"),
      missing = "no"
    )

  # names that are not statistics are ignored for full-length vectors (pre-v2.7.0 behavior)
  lifecycle::expect_deprecated(
    tbl_labelled <- add_stat_label(tbl, label = age ~ c(n = "n", mean = "Mean", sd = "SD"))
  )
  expect_equal(
    tbl_labelled$table_body |>
      dplyr::filter(row_type == "level") |>
      dplyr::pull(label),
    c("n", "Mean", "SD")
  )

  # partially matching names also fall back to positional matching
  lifecycle::expect_deprecated(
    tbl_labelled <- add_stat_label(tbl, label = age ~ c("{sd}" = "SD", "{mean}" = "Mean", x = "N"))
  )
  expect_equal(
    tbl_labelled$table_body |>
      dplyr::filter(row_type == "level") |>
      dplyr::pull(label),
    c("SD", "Mean", "N")
  )

  # names that are not statistics with a vector that is not full length errors
  expect_snapshot(
    error = TRUE,
    add_stat_label(tbl, label = age ~ c(n = "n", mean = "Mean"))
  )
})

test_that("add_stat_label(location='column') missing row label", {
  expect_equal(
    trial |>
      tbl_summary(include = age, missing_stat = "{N_miss} ({p_miss}%)") |>
      add_stat_label(location = "column") |>
      getElement("table_body") |>
      dplyr::filter(row_type == "missing") |>
      dplyr::pull(stat_label),
    "n (%)"
  )
})

test_that("add_stat_label() messaging", {
  expect_snapshot(
    trial |>
      tbl_summary(
        include = c(age, trt),
      ) |>
      add_stat_label() |>
      add_stat_label() |>
      invisible()
  )
})

test_that("add_stat_label() with tbl_merge()", {
  tbl0 <-
    trial |>
    select(age, response, trt) |>
    tbl_summary(by = trt, missing = "no") |>
    add_stat_label()

  expect_error(
    tbl1 <- tbl_merge(list(tbl0, tbl0)),
    NA
  )
  expect_snapshot(tbl1 |> as.data.frame())

  expect_equal(
    as_tibble(tbl1, col_labels = FALSE) %>% dplyr::pull(label),
    c("Age, Median (Q1, Q3)", "Tumor Response, n (%)")
  )
})
