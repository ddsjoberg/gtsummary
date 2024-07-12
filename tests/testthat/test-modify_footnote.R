
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

test_that("modify_footnote() with tbl_svysummary()", {
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

test_that("modify_footnote() works with tbl_svysummary()", {
  skip_if_not(is_pkg_installed(c("survey", "cardx"), reference_pkg = "gtsummary"))

  expect_equal(
    survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
      tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age))|>
      add_overall() |>
      modify_footnote(all_stat_cols() ~ "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("footnote") |>
      dplyr::slice_tail(by = "column", n = 1) |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull("footnote"),
    c("No | N = 2201 | n = 1490 | p = 68%",
      "Yes | N = 2201 | n = 711 | p = 32%",
      "Overall | N = 2201 | n = 2201 | p = 100%"),
    ignore_attr = TRUE)
})

test_that("modify_footnote() works with tbl_continuous()", {
  expect_equal(tbl_continuous(data = trial, variable = age, by = trt, include = grade)|>
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
               ignore_attr = TRUE)
})


test_that("modify_footnote() works with tbl_cross()", {
  expect_equal(tbl_cross(data = trial, row = trt, col = response) |>
                 modify_footnote(stat_0 = "Total Response") |>
                 getElement("table_styling") |>
                 getElement("footnote") |>
                 dplyr::slice_tail(by = "column", n = 1) |>
                 dplyr::filter(column == "stat_0") |>
                 dplyr::pull("footnote"),
               c("Total Response"), ignore_attr = TRUE
  )
})

test_that("modify_footnote() works with tbl_regression()", {
  skip_if_not(is_pkg_installed("broom.helpers", reference_pkg = "gtsummary"))

  expect_equal(glm(response ~ age + grade, trial, family = binomial()) |>
                 tbl_regression(exponentiate = TRUE) |>
                 modify_footnote(estimate = "Estimate") |>
                 getElement("table_styling") |>
                 getElement("footnote") |>
                 dplyr::slice_tail(by = "column", n = 1) |>
                 dplyr::filter(column == "estimate") |>
                 dplyr::pull("footnote"),
               c("Estimate"), ignore_attr = TRUE
  )
})

test_that("modify_footnote() works with tbl_uvregression()", {
  expect_equal(tbl_uvregression(trial, method = glm, y = response, method.args = list(family = binomial),
                                exponentiate = TRUE, include = c("age", "grade")) |>
                 modify_footnote(estimate = "Estimate") |>
                 getElement("table_styling") |>
                 getElement("footnote") |>
                 dplyr::slice_tail(by = "column", n = 1) |>
                 dplyr::filter(column == "estimate") |>
                 dplyr::pull("footnote"),
               c("Estimate"), ignore_attr = TRUE
  )
})
