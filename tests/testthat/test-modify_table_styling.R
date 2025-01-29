skip_on_cran()

# starting with checks of the deprecation
test_that("modify_table_styling(undo_text_format=logical()) deprecation", {
  withr::local_options(lifecycle_verbosity = "quiet")
  lifecycle::expect_deprecated(
    mtcars |>
      tbl_summary(include = mpg) |>
      modify_table_styling(
        columns = "label",
        text_format = "indent",
        undo_text_format = TRUE
      )
  )

  expect_equal(
    mtcars |>
      tbl_summary(include = mpg) |>
      modify_table_styling(
        columns = "label",
        text_format = "indent",
        undo_text_format = TRUE
      ) |>
      getElement("table_styling") |>
      getElement("indent"),
    mtcars |>
      tbl_summary(include = mpg) |>
      modify_table_styling(
        columns = "label",
        indent = 0L
      ) |>
      getElement("table_styling") |>
      getElement("indent")
  )
})

test_that("modify_table_styling(text_format=c('indent', 'indent2')) deprecation", {
  withr::local_options(lifecycle_verbosity = "quiet")
  lifecycle::expect_deprecated(
    mtcars |>
      tbl_summary(include = mpg) |>
      modify_table_styling(
        columns = "label",
        text_format = "indent"
      )
  )

  expect_equal(
    mtcars |>
      tbl_summary(include = mpg) |>
      modify_table_styling(
        columns = "label",
        text_format = "indent"
      ) |>
      getElement("table_styling") |>
      getElement("indent"),
    mtcars |>
      tbl_summary(include = mpg) |>
      modify_table_styling(
        columns = "label",
        indent = 4L
      ) |>
      getElement("table_styling") |>
      getElement("indent")
  )
})

test_that("modify_table_styling(rows)", {
  # works with objects defined outside of the expression
  footnote_variable <- "age"
  expect_error(
    tbl_summary(trial[c("trt", "age")]) %>%
      modify_table_styling(
        columns = label,
        footnote = "test footnote",
        rows = variable == footnote_variable
      ),
    NA
  )
})



test_that("modify_table_styling(label)", {
  expect_equal(
    mtcars |>
      tbl_summary(include = mpg) |>
      modify_table_styling(
        columns = stat_0,
        label = "**Overall**"
      ) |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column == "stat_0") |>
      dplyr::pull(label),
    "**Overall**"
  )

  expect_equal(
    mtcars |>
      tbl_summary(include = mpg) |>
      modify_table_styling(
        columns = c(label, stat_0),
        label = c("**Variable**", "**Overall**")
      ) |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column %in% c("label", "stat_0")) |>
      dplyr::pull(label),
    c("**Variable**", "**Overall**")
  )
})

test_that("modify_table_styling(spanning_header)", {
  expect_equal(
    trial |>
      tbl_summary(include = age, by = trt) |>
      modify_table_styling(
        columns = all_stat_cols(),
        spanning_header = "**Treatment Assignment**"
      ) |>
      getElement("table_styling") |>
      getElement("spanning_header") |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull(spanning_header),
    c("**Treatment Assignment**", "**Treatment Assignment**")
  )
})

test_that("modify_table_styling(hide)", {
  expect_equal(
    trial |>
      tbl_summary(include = age, missing = "no") |>
      modify_table_styling(
        columns = variable,
        hide = FALSE
      ) |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column == "variable") |>
      dplyr::pull(hide),
    FALSE
  )
})

test_that("modify_table_styling(footnote)", {
  expect_equal(
    trial |>
      tbl_summary(include = age, missing = "no") |>
      modify_table_styling(
        columns = all_stat_cols(),
        footnote = "testing footnote"
      ) |>
      getElement("table_styling") |>
      getElement("footnote_header") |>
      dplyr::filter(column == "stat_0") |>
      dplyr::slice_tail(n = 1L) |>
      dplyr::pull(footnote),
      "testing footnote"
  )
})

test_that("modify_table_styling(footnote_abbrev)", {
  expect_equal(
    trial |>
      tbl_summary(include = age, missing = "no") |>
      modify_table_styling(
        columns = all_stat_cols(),
        footnote_abbrev = "testing footnote_abbrev"
      ) |>
      getElement("table_styling") |>
      getElement("abbreviation") |>
      dplyr::pull(abbreviation),
    "testing footnote_abbrev"
  )
})

test_that("modify_table_styling(align)", {
  expect_equal(
    lapply(
      c("left", "right", "center"),
      function(x) {
        trial |>
          tbl_summary(include = age, missing = "no") |>
          modify_table_styling(
            columns = label,
            align = x
          ) |>
          getElement("table_styling") |>
          getElement("header") |>
          dplyr::filter(column == "label") |>
          dplyr::pull(align)
      }
    ) |>
      unlist(),
    c("left", "right", "center")
  )
})

test_that("modify_table_styling(missing_symbol)", {
  expect_equal(
    trial |>
      tbl_summary(include = age, missing = "no") |>
      modify_table_styling(
        columns = all_stat_cols(),
        missing_symbol = "THIS IS NA"
      ) |>
      getElement("table_styling") |>
      getElement("fmt_missing") |>
      dplyr::filter(column == "stat_0") |>
      dplyr::slice_tail(n = 1L) |>
      dplyr::pull(symbol),
    "THIS IS NA"
  )
})

test_that("modify_table_styling(fmt_fun)", {
  expect_equal(
    lm(mpg ~ am, mtcars) |>
      tbl_regression() |>
      modify_table_styling(
        columns = c("estimate", "conf.low", "conf.high", "p.value"),
        fmt_fun = list(label_style_sigfig(digits = 1), label_style_sigfig(digits = 2), label_style_sigfig(digits = 4), label_style_pvalue(digits = 3))
      ) |>
      getElement("table_styling") |>
      getElement("fmt_fun") |>
      dplyr::filter(.by = column, dplyr::n() == dplyr::row_number()) |>
      dplyr::filter(column %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      dplyr::pull(fmt_fun),
    list(label_style_sigfig(digits = 1), label_style_sigfig(digits = 2), label_style_sigfig(digits = 4), label_style_pvalue(digits = 3))
  )
})

test_that("modify_table_styling(text_format)", {
  expect_equal(
    trial |>
      tbl_summary(include = grade) |>
      modify_table_styling(
        columns = "label",
        rows = row_type %in% "label",
        text_format = "bold"
      ) |>
      getElement("table_styling") |>
      getElement("text_format") |>
      dplyr::filter(column == "label", undo_text_format == FALSE) |>
      dplyr::pull(format_type),
    "bold"
  )
})

test_that("modify_table_styling(undo_text_format)", {
  expect_equal(
    trial |>
      tbl_summary(include = grade) |>
      modify_table_styling(
        columns = "label",
        rows = row_type %in% "label",
        undo_text_format = "bold"
      ) |>
      getElement("table_styling") |>
      getElement("text_format") |>
      dplyr::filter(column == "label", undo_text_format == TRUE) |>
      dplyr::pull(format_type),
    "bold"
  )
})

test_that("modify_table_styling(indent)", {
  expect_equal(
    trial |>
      tbl_summary(include = grade) |>
      modify_table_styling(
        columns = "label",
        rows = !row_type %in% "label",
        indent = 8L
      ) |>
      getElement("table_styling") |>
      getElement("indent") |>
      dplyr::filter(.by = "column", column == "label", dplyr::n() == dplyr::row_number()) |>
      dplyr::pull(n_spaces),
    8L
  )
})

test_that("modify_table_styling(text_interpret)", {
  expect_equal(
    trial |>
      tbl_summary(include = grade) |>
      modify_table_styling(
        columns = "label",
        label = "header",
        text_interpret = "html"
      ) |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column == "label") |>
      dplyr::pull(interpret_label),
    "gt::html"
  )

  expect_equal(
    trial |>
      tbl_summary(include = grade, by = trt) |>
      modify_table_styling(
        columns = all_stat_cols(),
        spanning_header = "my big header",
        text_interpret = "html"
      ) |>
      getElement("table_styling") |>
      getElement("spanning_header") |>
      dplyr::filter(startsWith(column, "stat_")) |>
      dplyr::pull(spanning_header),
    c("my big header", "my big header")
  )
})

test_that("modify_table_styling(cols_merge_pattern)", {
  expect_equal(
    lm(mpg ~ factor(am), mtcars) |>
      tbl_regression() |>
      modify_table_styling(
        columns = "conf.low",
        rows = !is.na(conf.low),
        cols_merge_pattern = "{conf.low}:::{conf.high}"
      ) |>
      getElement("table_styling") |>
      getElement("cols_merge") |>
      dplyr::filter(.by = "column", column == "conf.low", dplyr::n() == dplyr::row_number()) |>
      dplyr::pull(pattern),
    "{conf.low}:::{conf.high}"
  )

  # test we can undo merging
  expect_equal(
    lm(mpg ~ factor(am), mtcars) |>
      tbl_regression() |>
      modify_table_styling(
        columns = conf.low,
        cols_merge_pattern = NA_character_
      ) |>
      modify_column_unhide(c("conf.low", "conf.high")) |>
      as.data.frame(col_label = FALSE) |>
      names(),
    c("label", "estimate",  "conf.low",  "conf.high", "p.value")
  )
})

test_that("modify_table_styling(indent) messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(include = grade) |>
      modify_table_styling(
        columns = "label",
        rows = !row_type %in% "label",
        indent = "not an integer"
      )
  )
})

test_that("modify_table_styling(rows) messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(include = grade) |>
      modify_table_styling(
        columns = "label",
        rows = "not_a_predicate",
        indent = 8L
      )
  )
})
