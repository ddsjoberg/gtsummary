test_that("modify_bold/italic()/remove_bold/italic()", {
  expect_silent(
    tbl1 <- trial |>
      tbl_summary(include = grade) |>
      modify_bold(columns = label, rows = row_type == "label") |>
      modify_italic(columns = label, rows = row_type == "level")
  )
  expect_equal(
    tbl1$table_styling$text_format,
    dplyr::tribble(
      ~column,                ~rows, ~format_type, ~undo_text_format,
      "label", ~row_type == "label",       "bold",             FALSE,
      "label", ~row_type == "level",     "italic",             FALSE
    ),
    ignore_formula_env = TRUE,
    ignore_attr = TRUE
  )


  expect_silent(
    tbl2 <- tbl1 |>
      remove_bold(columns = label, rows = row_type == "label") |>
      remove_italic(columns = label, rows = row_type == "level")
  )
  expect_equal(
    tbl2$table_styling$text_format,
    dplyr::tribble(
      ~column,                ~rows, ~format_type, ~undo_text_format,
      "label", ~row_type == "label",       "bold",             FALSE,
      "label", ~row_type == "level",     "italic",             FALSE,
      "label", ~row_type == "label",       "bold",              TRUE,
      "label", ~row_type == "level",     "italic",              TRUE
    ),
    ignore_formula_env = TRUE,
    ignore_attr = TRUE
  )
})
