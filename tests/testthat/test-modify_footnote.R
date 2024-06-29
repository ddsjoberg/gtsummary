
# first, testing deprecation
test_that("modify_footnote(update,quiet) are deprecated", {
  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_footnote(update = list(label = "Variable"))
  )
  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_footnote(quiet = FALSE)
  )
})

test_that("modify_footnote(update) deprecated argument still works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_footnote(update = list(label = "Variable")) %>%
      {.[c("table_body", "table_styling")]},
    tbl_summary(trial, include = marker) |>
      modify_footnote(label = "Variable") %>%
      {.[c("table_body", "table_styling")]}
  )
})


test_that("modify_footnote(...) works", {
  tbl <- tbl_summary(trial, include = "marker")

  # typical use
  expect_equal(
    tbl |>
      modify_footnote(label = "Variable") |>
      getElement("table_styling") |>
      getElement("footnote") |>
      dplyr::filter(column %in% "label") |>
      dplyr::pull("footnote"),
    "Variable",
    ignore_attr = TRUE
  )

  expect_equal(
    tbl |>
      modify_footnote(label = "Variable", stat_0 = "Overall") |>
      getElement("table_styling") |>
      getElement("footnote") |>
      dplyr::slice_tail(by = "column", n = 1) |>
      dplyr::filter(column %in% c("label", "stat_0")) |>
      dplyr::pull("footnote"),
    c("Overall", "Variable"),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl |>
      modify_footnote(label = "Variable", stat_0 = "Overall", abbreviation = TRUE) |>
      getElement("table_styling") |>
      getElement("footnote_abbrev") |>
      dplyr::slice_tail(by = "column", n = 1) |>
      dplyr::filter(column %in% c("label", "stat_0")) |>
      dplyr::pull("footnote"),
    c("Variable", "Overall"),
    ignore_attr = TRUE
  )
})

# TODO: Add dynamic headers work with `tbl_svysummary()`
# TODO: Add dynamic headers work with `tbl_continuous()`
# TODO: Add dynamic headers work with `tbl_cross()`
# TODO: Add dynamic headers work with `tbl_regression()`
# TODO: Add dynamic headers work with `tbl_uvregression()`
test_that("modify_footnote(...) dynamic headers work with `tbl_summary()`", {
  tbl <- tbl_summary(trial, include = "marker")

  # test dynamic dots
  expect_equal(
    tbl |>
      modify_footnote(!!!list(label = "Variable", stat_0 = "Overall")) |>
      getElement("table_styling") |>
      getElement("footnote") |>
      dplyr::slice_tail(by = "column", n = 1) |>
      dplyr::filter(column %in% c("label", "stat_0")) |>
      dplyr::pull("footnote"),
    c("Overall", "Variable"),
    ignore_attr = TRUE
  )

  # testing dynamic entries in header
  expect_equal(
    tbl |>
      modify_footnote(stat_0 = "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("footnote") |>
      dplyr::slice_tail(by = "column", n = 1) |>
      dplyr::filter(column %in% "stat_0") |>
      dplyr::pull("footnote"),
    "Overall | N = 200 | n = 200 | p = 100%",
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_summary(trial, by = trt, include = marker) |>
      modify_footnote(all_stat_cols() ~ "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("footnote") |>
      dplyr::slice_tail(by = "column", n = 1) |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull("footnote"),
    c("Drug A | N = 200 | n = 98 | p = 49%",
      "Drug B | N = 200 | n = 102 | p = 51%"),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_summary(trial, by = trt, include = marker) |>
      add_overall() |>
      modify_footnote(all_stat_cols() ~ "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("footnote") |>
      dplyr::slice_tail(by = "column", n = 1) |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull("footnote"),
    c("Drug A | N = 200 | n = 98 | p = 49%",
      "Drug B | N = 200 | n = 102 | p = 51%",
      "Overall | N = 200 | n = 200 | p = 100%"),
    ignore_attr = TRUE
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      modify_footnote(label = "This is not a valid {element}.")
  )
})

test_that("modify_footnote(text_interpret) works", {
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_footnote(label = "Variable", text_interpret = "html") |>
      getElement("table_styling") |>
      getElement("footnote") |>
      dplyr::slice_tail(by = "column", n = 1) |>
      dplyr::filter(column %in% "label") |>
      dplyr::pull(text_interpret),
    "gt::html"
  )
})

