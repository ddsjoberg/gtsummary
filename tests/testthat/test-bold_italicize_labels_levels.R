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
