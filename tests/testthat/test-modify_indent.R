skip_on_cran()

test_that("modify_indent() works", {
  # remove indentation from `tbl_summary()`
  expect_equal(
    trial |>
      tbl_summary(include = grade) |>
      modify_indent(columns = label, indent = 0L) |>
      getElement("table_styling") |>
      getElement("indent") |>
      dplyr::slice_tail(n = 1, by = "column") |>
      dplyr::pull(n_spaces),
    0L
  )


  # increase indentation in `tbl_summary`
  expect_equal(
    trial |>
      tbl_summary(include = grade) |>
      modify_indent(columns = label, rows = !row_type %in% 'label', indent = 8L) |>
      getElement("table_styling") |>
      getElement("indent") |>
      dplyr::slice_tail(n = 1, by = "column") |>
      dplyr::pull(n_spaces),
    8L
  )
})

test_that("modify_indent() messaging", {
  expect_error(
    trial |>
      tbl_summary(include = grade) |>
      modify_indent(columns = label, indent = -4L),
    "must be a non-negative scalar integer"
  )
})
