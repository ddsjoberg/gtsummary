test_that("add_variable_group_header() works", {
  expect_silent(
    tbl <- trial |>
      tbl_summary(include = c(age, response, death, marker), missing = "no") |>
      add_variable_group_header(header = "the header row", variables = c(response, death))
  )

  # header row is in correct location
  expect_equal(
    as_tibble(tbl, col_labels = FALSE)$label[2],
    "the header row"
  )

  # grouped variables are indented.
  expect_equal(
    .table_styling_expr_to_row_number(tbl)$table_styling$indent$row_numbers[[1]],
    c(3L, 4L)
  )
})
