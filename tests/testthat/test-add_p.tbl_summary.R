skip_if_not(is_pkg_installed("broom", reference_pkg = "cardx"))

test_that("add_p.tbl_summary() snapshots of common outputs", {
  expect_snapshot(
    tbl_summary(trial, by = grade) |>
      add_p() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )

  expect_snapshot(
    tbl_summary(mtcars, by = am) |>
      add_p() |>
      as.data.frame()
  )

  expect_snapshot(
    trial |>
      tbl_summary(by = trt) |>
      add_p() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )
})

test_that("add_p.tbl_summary() & lme4", {
  skip_if_not(is_pkg_installed("lme4", reference_pkg = "cardx"))

  # errors with expected use
  expect_error(
    tbl_summary(trial, by = trt) |>
      add_p(test = everything() ~ "lme4", group = response),
    NA
  )

  # we see appropriate messaging when using incorrectly (no group variable)
  expect_snapshot(
    tbl_summary(trial, by = trt) |>
      add_p(test = everything() ~ "lme4") |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )
})

test_that("add_p.tbl_summary() creates output without error/warning for continuous2", {
  expect_snapshot(
    tbl_summary(trial, by = grade, include = c(age, marker, response), type = all_continuous() ~ "continuous2") |>
      add_p() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )
})
