
# first, testing deprecation
test_that("modify_header(update,quiet) are deprecated", {
  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_header(list(label = "Variable"))
  )

  # THIS ONE FAILS LOCALLY UNLESS I RUN ALL THE TESTS WITH THE 'Test' BUTTON
  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_header(update = list(label = "Variable"))
  )

  lifecycle::expect_deprecated(
    tbl_summary(trial, include = marker) |>
      modify_header(quiet = FALSE)
  )
})


test_that("modify_header(update) are deprecated and still work", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_header(list(label = "Variable")) |>
      getElement("table_styling") |>
      getElement("header"),
    tbl_summary(trial, include = marker) |>
      modify_header(label = "Variable") |>
      getElement("table_styling") |>
      getElement("header")
  )

  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_header(update = list(label = "Variable")) |>
      getElement("table_styling") |>
      getElement("header"),
    tbl_summary(trial, include = marker) |>
      modify_header(label = "Variable") |>
      getElement("table_styling") |>
      getElement("header")
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

test_that("modify_header() works with tbl_svysummary()", {
  skip_if_not(is_pkg_installed(c("survey", "cardx"), reference_pkg = "gtsummary"))

  expect_equal(
    survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
      tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age))|>
      add_overall() |>
      modify_header(all_stat_cols() ~ "{level} | N = {N} | n = {n} | p = {style_percent(p)}%") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull("label"),
    c("Overall | N = 2201 | n = 2201 | p = 100%",
      "No | N = 2201 | n = 1490 | p = 68%",
      "Yes | N = 2201 | n = 711 | p = 32%")
  )
})

test_that("modify_header() works with tbl_continuous()", {
  expect_equal(tbl_continuous(data = trial, variable = age, by = trt, include = grade)|>
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
})

test_that("modify_header() works with tbl_cross()", {
  expect_equal(tbl_cross(data = trial, row = trt, col = response) |>
                 modify_header(all_stat_cols() ~ "N = {N} | n = {n} | p = {style_percent(p)}%") |>
                 getElement("table_styling") |>
                 getElement("header") |>
                 dplyr::filter(startsWith(column, "stat_")) |>
                 dplyr::pull("label"),
               c("N = 200 | n = 132 | p = 66%",
                 "N = 200 | n = 61 | p = 31%",
                 "N = 200 | n = 7 | p = 3.5%",
                 "N = 200 | n = 200 | p = 100%")
  )
})

test_that("modify_header() works with tbl_regression()", {
  skip_if_not(is_pkg_installed("broom.helpers", reference_pkg = "gtsummary"))

  expect_equal(glm(response ~ age + grade, trial, family = binomial()) |>
                 tbl_regression(exponentiate = TRUE) |>
                 modify_header(p.value = "P-Value", conf.low = "Confidence Interval") |>
                 getElement("table_styling") |>
                 getElement("header") |>
                 dplyr::filter(hide == "FALSE") |>
                 dplyr::pull("label"),
               c("**Characteristic**",
                 "**OR**",
                 "Confidence Interval",
                 "P-Value")
  )
})

test_that("modify_header() works with tbl_uvregression()", {
  expect_equal(tbl_uvregression(trial, method = glm, y = response, method.args = list(family = binomial),
                                exponentiate = TRUE, include = c("age", "grade")) |>
                 modify_header(p.value = "P-Value", conf.low = "Confidence Interval") |>
                 getElement("table_styling") |>
                 getElement("header") |>
                 dplyr::filter(hide == "FALSE") |>
                 dplyr::pull("label"),
               c("**Characteristic**",
                 "**N**",
                 "**OR**",
                 "Confidence Interval",
                 "P-Value")
  )
})
