skip_on_cran()
skip_if_not(is_pkg_installed(c("cardx", "broom"), reference_pkg = "gtsummary"))

test_that("sort_p() works", {
  # check the variables sort to the correct order
  expect_equal(
    tbl_summary(trial, by = trt, include = c(age, response, grade)) |>
      add_p(include = -response) |>
      sort_p() |>
      getElement("table_body") |>
      dplyr::pull(variable) |>
      unique(),
    c("age", "grade", "response")
  )
})


test_that("filter_p() works", {
  # check the variables sort to the correct order
  expect_equal(
    tbl_summary(trial, by = trt, include = c(age, response, grade)) |>
      add_p(include = -response) |>
      filter_p(t = 0.8) |>
      getElement("table_body") |>
      dplyr::pull(variable) |>
      unique(),
    "age"
  )
})

test_that("sort_p() messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(by = trt, include = c(response, marker, trt), missing = "no") |>
      sort_p()
  )
})

test_that("filter_p() messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(by = trt, include = c(response, marker, trt), missing = "no") |>
      filter_p()
  )
})
