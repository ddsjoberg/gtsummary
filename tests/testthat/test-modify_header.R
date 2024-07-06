
# first, testing deprecation
test_that("modify_header(update,quiet) are deprecated", {
  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_header(update = list(label = "Variable"))
  )
  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_header(list(label = "Variable"))
  )
  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_header(quiet = FALSE)
  )
})

test_that("modify_header(update) deprecated argument still works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_header(update = list(label = "Variable")) %>%
      {.[c("table_body", "table_styling")]},
    tbl_summary(trial, include = marker) |>
      modify_header(label = "Variable") %>%
      {.[c("table_body", "table_styling")]}
  )
})


test_that("modify_header(...) works", {
  tbl <- tbl_summary(trial, include = "marker")

  # typical use
  expect_equal(
    tbl |>
      modify_header(label = "Variable") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% "label") |>
      dplyr::pull("label"),
    "Variable"
  )

  expect_equal(
    tbl |>
      modify_header(label = "Variable", stat_0 = "Overall") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% c("label", "stat_0")) |>
      dplyr::pull("label"),
    c("Variable", "Overall")
  )
})

# TODO: Add dynamic headers work with `tbl_svysummary()`
# TODO: Add dynamic headers work with `tbl_continuous()`
# TODO: Add dynamic headers work with `tbl_cross()`
# TODO: Add dynamic headers work with `tbl_regression()`
# TODO: Add dynamic headers work with `tbl_uvregression()`
test_that("modify_header(...) dynamic headers work with `tbl_summary()`", {
  tbl <- tbl_summary(trial, include = "marker")

  # test dynamic dots
  expect_equal(
    tbl |>
      modify_header(!!!list(label = "Variable", stat_0 = "Overall")) |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% c("label", "stat_0")) |>
      dplyr::pull("label"),
    c("Variable", "Overall")
  )

  # testing dynamic entries in header
  expect_equal(
    tbl |>
      modify_header(stat_0 = "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% "stat_0") |>
      dplyr::pull("label"),
    "Overall | N = 200 | n = 200 | p = 100%"
  )

  expect_equal(
    tbl_summary(trial, by = trt, include = marker) |>
      modify_header(all_stat_cols() ~ "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull("label"),
    c("Drug A | N = 200 | n = 98 | p = 49%",
      "Drug B | N = 200 | n = 102 | p = 51%")
  )

  expect_equal(
    tbl_summary(trial, by = trt, include = marker) |>
      add_overall() |>
      modify_header(all_stat_cols() ~ "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull("label"),
    c("Overall | N = 200 | n = 200 | p = 100%",
      "Drug A | N = 200 | n = 98 | p = 49%",
      "Drug B | N = 200 | n = 102 | p = 51%")
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      modify_header(label = "This is not a valid {element}.")
  )
})

test_that("modify_header(text_interpret) works", {
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_header(label = "Variable", text_interpret = "html") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% "label") |>
      dplyr::pull(interpret_label),
    "gt::html"
  )
})

