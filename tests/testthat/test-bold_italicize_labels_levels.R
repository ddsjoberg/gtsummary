tbl <- tbl_summary(trial, include = age)

test_that("bold_labels() works", {
  expect_equal(
    bold_labels(tbl) |>
      getElement("table_styling") |>
      getElement("text_format") |>
      dplyr::filter(format_type == "bold") |>
      getElement("rows") |>
      getElement(1L) |>
      quo_get_expr(),
    expr(.data$row_type == "label")
  )
})

test_that("bold_levels() works", {
  expect_equal(
    bold_levels(tbl) |>
      getElement("table_styling") |>
      getElement("text_format") |>
      dplyr::filter(format_type == "bold") |>
      getElement("rows") |>
      getElement(1L) |>
      quo_get_expr(),
    expr(.data$row_type != "label")
  )
})

test_that("italicize_labels() works", {
  expect_equal(
    italicize_labels(tbl) |>
      getElement("table_styling") |>
      getElement("text_format") |>
      dplyr::filter(format_type == "italic") |>
      getElement("rows") |>
      getElement(1L) |>
      quo_get_expr(),
    expr(.data$row_type == "label")
  )
})

test_that("italicize_levels() works", {
  expect_equal(
    italicize_levels(tbl) |>
      getElement("table_styling") |>
      getElement("text_format") |>
      dplyr::filter(format_type == "italic") |>
      getElement("rows") |>
      getElement(1L) |>
      quo_get_expr(),
    expr(.data$row_type != "label")
  )
})

test_that("bold_italicize_labels_levels messaging", {
  expect_message(
    testing <- tbl |>
      modify_table_body(~.x |> dplyr::select(-row_type)) |>
      bold_labels(),
    "cannot be used in this context."
  )

  expect_message(
    testing <- tbl |>
      modify_table_body(~.x |> dplyr::select(-row_type)) |>
      bold_levels(),
    "cannot be used in this context."
  )

  expect_message(
    testing <- tbl |>
      modify_table_body(~.x |> dplyr::select(-row_type)) |>
      italicize_labels(),
    "cannot be used in this context."
  )

  expect_message(
    testing <- tbl |>
      modify_table_body(~.x |> dplyr::select(-row_type)) |>
      italicize_levels(),
    "cannot be used in this context."
  )
})

# tbl_cross styling ------------------------------------------------------------

test_that("bold_labels.tbl_cross()", {
  tbl <-
    tbl_cross(trial, grade, stage) |>
    bold_labels()

  expect_equal(
    tbl |>
      as.data.frame() |>
      getElement(1L),
    c("__Grade__", "I", "II", "III", "__Total__")
  )
  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column == "stat_0") |>
      dplyr::pull(label),
    "**Total**"
  )
  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(startsWith(column, "stat_"), column != "stat_0") |>
      dplyr::pull(spanning_header) |>
      unique(),
    "**T Stage**"
  )
})

test_that("bold_levels.tbl_cross()", {
  tbl <-
    tbl_cross(trial, grade, stage) |>
    bold_levels()

  expect_equal(
    tbl |>
      as.data.frame() |>
      getElement(1L),
    c("Grade", "__I__", "__II__", "__III__", "Total")
  )
  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(startsWith(column, "stat_"), column != "stat_0") |>
      dplyr::pull(label),
    c("**T1**", "**T2**", "**T3**", "**T4**")
  )
})

test_that("italicize_labels.tbl_cross()", {
  tbl <-
    tbl_cross(trial, grade, stage) |>
    italicize_labels()

  expect_equal(
    tbl |>
      as.data.frame() |>
      getElement(1L),
    c("_Grade_", "I", "II", "III", "_Total_")
  )
  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(column == "stat_0") |>
      dplyr::pull(label),
    "*Total*"
  )
  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(startsWith(column, "stat_"), column != "stat_0") |>
      dplyr::pull(spanning_header) |>
      unique(),
    "*T Stage*"
  )
})

test_that("italicize_levels.tbl_cross()", {
  tbl <-
    tbl_cross(trial, grade, stage) |>
    italicize_levels()

  expect_equal(
    tbl |>
      as.data.frame() |>
      getElement(1L),
    c("Grade", "_I_", "_II_", "_III_", "Total")
  )
  expect_equal(
    tbl |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(startsWith(column, "stat_"), column != "stat_0") |>
      dplyr::pull(label),
    c("*T1*", "*T2*", "*T3*", "*T4*")
  )
})
