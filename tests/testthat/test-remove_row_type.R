skip_on_cran()
skip_if_not(is_pkg_installed(c("cardx", "broom.helpers"), reference_pkg = "gtsummary"))

test_that("remove_row_type(type) works", {
  tbl1 <- tbl_summary(trial, include = c(response, age, grade))
  tbl2 <- lm(age ~ grade + response, trial) |> tbl_regression()

  expect_equal(
    remove_row_type(tbl1, type = "header", variables = c("grade", "age")) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull("label"),
    c("Tumor Response", "Unknown", "Age", "Unknown", "I", "II", "III")
  )

  expect_equal(
    remove_row_type(tbl1, type = "missing", variables = c("grade", "age")) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull("label"),
    c("Tumor Response", "Unknown", "Age", "Grade", "I", "II", "III")
  )

  expect_equal(
    remove_row_type(tbl1, type = "level", variables = c("grade", "age"), level_value = "III") |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull("label"),
    c("Tumor Response", "Unknown", "Age", "Unknown", "Grade", "I", "II")
  )

  expect_equal(
    remove_row_type(tbl1, type = "all", variables = c("grade", "age")) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull("label"),
    c("Tumor Response", "Unknown")
  )

  expect_equal(
    remove_row_type(tbl2, type = "reference", variables = c("grade", "response")) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull("label"),
    c("Grade", "II", "III", "Tumor Response")
  )
})

test_that("remove_row_type(type) messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = c(response, age, grade)) |>
      remove_row_type(type = "reference")
  )
})
