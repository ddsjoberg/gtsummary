
# first, testing deprecation
test_that("modify_spanning_header(update,quiet) are deprecated", {
  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_spanning_header(update = list(label = "Variable"))
  )
  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_spanning_header(quiet = FALSE)
  )
})

test_that("modify_spanning_header(update) deprecated argument still works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_spanning_header(update = list(label = "Variable")) %>%
      {.[c("table_body", "table_styling")]},
    tbl_summary(trial, include = marker) |>
      modify_spanning_header(label = "Variable") %>%
      {.[c("table_body", "table_styling")]}
  )
})

test_that("modify_spanning_header(...) works", {
  tbl <- tbl_summary(trial, include = "marker")

  # typical use
  expect_equal(
    tbl |>
      modify_spanning_header(label = "Variable") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% "label") |>
      dplyr::pull("spanning_header"),
    "Variable"
  )

  expect_equal(
    tbl |>
      modify_spanning_header(label = "Variable", stat_0 = "Overall") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% c("label", "stat_0")) |>
      dplyr::pull("spanning_header"),
    c("Variable", "Overall")
  )

  expect_equal(
    tbl |>
      modify_spanning_header(c(label, stat_0) ~ "Variable") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% c("label", "stat_0")) |>
      dplyr::pull("spanning_header"),
    c("Variable", "Variable")
  )
})

test_that("modify_spanning_header(...) dynamic headers work with `tbl_summary()`", {
  tbl <- tbl_summary(trial, include = "marker")

  # test dynamic dots
  expect_equal(
    tbl |>
      modify_spanning_header(!!!list(label = "Variable", stat_0 = "Overall")) |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% c("label", "stat_0")) |>
      dplyr::pull("spanning_header"),
    c("Variable", "Overall")
  )

  # testing dynamic entries in header
  expect_equal(
    tbl |>
      modify_spanning_header(stat_0 = "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% "stat_0") |>
      dplyr::pull("spanning_header"),
    "Overall | N = 200 | n = 200 | p = 100%"
  )

  expect_equal(
    tbl_summary(trial, by = trt, include = marker) |>
      modify_spanning_header(all_stat_cols() ~ "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull("spanning_header"),
    c("Drug A | N = 200 | n = 98 | p = 49%",
      "Drug B | N = 200 | n = 102 | p = 51%")
  )

  expect_equal(
    tbl_summary(trial, by = trt, include = marker) |>
      add_overall() |>
      modify_spanning_header(all_stat_cols() ~ "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull("spanning_header"),
    c("Overall | N = 200 | n = 200 | p = 100%",
      "Drug A | N = 200 | n = 98 | p = 49%",
      "Drug B | N = 200 | n = 102 | p = 51%")
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      modify_spanning_header(label = "This is not a valid {element}.")
  )
})

test_that("modify_spanning_header(text_interpret) works", {
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_spanning_header(label = "Variable", text_interpret = "html") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% "label") |>
      dplyr::pull(interpret_spanning_header),
    "gt::html"
  )
})

test_that("modify_spanning_header() works with tbl_svysummary()", {
  skip_if_not(is_pkg_installed(c("survey", "cardx"), reference_pkg = "gtsummary"))

  expect_equal(
    survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
      tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age))|>
      add_overall() |>
      modify_spanning_header(label = "Variable") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column == "label") |>
      dplyr::pull("spanning_header"),
    "Variable"
  )
})

test_that("modify_spanning_header() works with tbl_continuous()", {
  expect_equal(tbl_continuous(data = trial, variable = age, by = trt, include = grade)|>
                 add_overall() |>
                 modify_spanning_header(all_stat_cols() ~ "Statistics") |>
                 getElement("table_styling") |>
                 getElement("header") |>
                 dplyr::filter(startsWith(column, "stat_")) |>
                 dplyr::pull("spanning_header"),
               c("Statistics", "Statistics", "Statistics")
  )
})


test_that("modify_spanning_header() works with tbl_cross()", {
  expect_equal(tbl_cross(data = trial, row = trt, col = response) |>
                 modify_spanning_header(stat_0 = "Total Response") |>
                 getElement("table_styling") |>
                 getElement("header") |>
                 dplyr::filter(column == "stat_0") |>
                 dplyr::pull("spanning_header"),
               c("Total Response")
  )
})

test_that("modify_spanning_header() works with tbl_regression()", {
  skip_if_not(is_pkg_installed("broom.helpers", reference_pkg = "gtsummary"))

  expect_equal(glm(response ~ age + grade, trial, family = binomial()) |>
                 tbl_regression(exponentiate = TRUE) |>
                 modify_spanning_header(estimate = "Estimate") |>
                 getElement("table_styling") |>
                 getElement("header") |>
                 dplyr::filter(column == "estimate") |>
                 dplyr::pull("spanning_header"),
               c("Estimate")
  )
})

test_that("modify_spanning_header() works with tbl_uvregression()", {
  expect_equal(tbl_uvregression(trial, method = glm, y = response, method.args = list(family = binomial),
                                exponentiate = TRUE, include = c("age", "grade")) |>
                 modify_spanning_header(estimate = "Estimate") |>
                 getElement("table_styling") |>
                 getElement("header") |>
                 dplyr::filter(column == "estimate") |>
                 dplyr::pull("spanning_header"),
               c("Estimate")
  )
})
