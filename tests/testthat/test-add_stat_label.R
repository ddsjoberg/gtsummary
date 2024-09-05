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
