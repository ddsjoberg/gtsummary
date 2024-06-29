# begin with deprecation messaging
test_that("modify_column_indent() deprecations", {
  lifecycle::expect_deprecated(
    trial |>
      tbl_summary(include = grade) |>
      modify_column_indent(columns = "label", undo = TRUE)
  )
  lifecycle::expect_deprecated(
    trial |>
      tbl_summary(include = grade) |>
      modify_column_indent(columns = "label", undo = FALSE)
  )
  lifecycle::expect_deprecated(
    trial |>
      tbl_summary(include = grade) |>
      modify_column_indent(columns = "label", double_indent = TRUE)
  )
  lifecycle::expect_deprecated(
    trial |>
      tbl_summary(include = grade) |>
      modify_column_indent(columns = "label", double_indent = FALSE)
  )

  # now check that the results are the same with and without deprecated args
  withr::local_options(lifecycle_verbosity = "quiet")
  tbl <- trial |> tbl_summary(include = grade)

  expect_equal(
    modify_column_indent(tbl, columns = "label", undo = TRUE)[c("table_body", "table_styling")],
    modify_column_indent(tbl, columns = "label", indent = 0L)[c("table_body", "table_styling")]
  )
  expect_equal(
    modify_column_indent(tbl, columns = "label", double_indent = TRUE)[c("table_body", "table_styling")],
    modify_column_indent(tbl, columns = "label", indent = 8L)[c("table_body", "table_styling")]
  )
})

test_that("modify_column_indent() works", {
  # remove indentation from `tbl_summary()`
  expect_equal(
    trial |>
      tbl_summary(include = grade) |>
      modify_column_indent(columns = label, indent = 0L) |>
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
      modify_column_indent(columns = label, rows = !row_type %in% 'label', indent = 8L) |>
      getElement("table_styling") |>
      getElement("indent") |>
      dplyr::slice_tail(n = 1, by = "column") |>
      dplyr::pull(n_spaces),
    8L
  )
})

test_that("modify_column_indent() messaging", {
  expect_error(
    trial |>
      tbl_summary(include = grade) |>
      modify_column_indent(columns = label, indent = -4L),
    "must be a non-negative scalar integer"
  )
})
