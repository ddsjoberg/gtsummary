test_that("modify_column_indent() works", {
  expect_error(
    tbl <-
      trial %>%
      select(grade) %>%
      tbl_summary() %>%
      modify_column_indent(columns = label, undo = TRUE),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())

  expect_equal(
    tbl$table_styling$text_format$undo_text_format,
    c(FALSE, TRUE)
  )
})
