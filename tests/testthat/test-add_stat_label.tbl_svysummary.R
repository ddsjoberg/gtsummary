skip_if_not(is_pkg_installed("survey", reference_pkg = "gtsummary"))

svy_trial <- survey::svydesign(~1, data = trial, weights = ~1)

test_that("add_stat_label(location='row') standard use", {
  tbl <- svy_trial |> tbl_svysummary(by = trt)

  expect_snapshot(
    tbl |>
      add_stat_label(location='row') |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )
})

test_that("add_stat_label(location='column') standard use", {
  tbl <- svy_trial |> tbl_svysummary(by = trt)

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
    svy_trial |>
      tbl_svysummary(
        include = c(age, grade, trt),
        by = trt,
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}"),
      ) |>
      add_stat_label(label = age ~ c("Median (IQR)", "Range")) |>
      as.data.frame()
  )
})

test_that("add_stat_label(label) messaging", {
  expect_snapshot(
    error = TRUE,
    svy_trial |>
      tbl_svysummary(
        include = c(age, trt),
        by = trt,
      ) |>
      add_stat_label(label = age ~ letters)
  )

  expect_snapshot(
    error = TRUE,
    svy_trial |>
      tbl_svysummary(
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
    svy_trial |>
      tbl_svysummary(
        include = c(age, trt),
      ) |>
      add_stat_label() |>
      add_stat_label() |>
      invisible()
  )
})
