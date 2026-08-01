# `.create_gtsummary_object()` uses a fast path in place of a literal
# `modify_column_hide(columns = -any_of("label"))` call. These tests pin that
# equivalence so a future change to `modify_column_hide()`/`as_gtsummary()` that
# breaks the assumption (or the hard-coded recorded call) is caught.

test_that(".create_gtsummary_object() matches the modify_column_hide() path", {
  for (tb in list(head(mtcars), trial[1:5, c("age", "grade")])) {
    fast <- .create_gtsummary_object(table_body = tb)
    canonical <-
      as_gtsummary(tb) |>
      modify_column_hide(columns = -any_of("label"))

    # everything except the recorded call_list should be identical
    fast$call_list <- NULL
    canonical$call_list <- NULL
    expect_equal(fast, canonical)
  }
})

test_that(".create_gtsummary_object() records the documented hide + call_list", {
  # a table without a "label" column: every column is hidden
  x <- .create_gtsummary_object(table_body = head(mtcars))
  expect_identical(x$table_styling$header$hide, rep(TRUE, ncol(mtcars)))

  # a table with a "label" column: only "label" is left unhidden
  y <- .create_gtsummary_object(
    table_body = data.frame(label = c("a", "b"), stat_0 = c("1", "2"))
  )
  expect_identical(y$table_styling$header$hide, c(FALSE, TRUE))

  # the recorded call is hard-coded (the fast path replaces the real
  # modify_column_hide() call) -- pin it so a future refactor is flagged
  expect_identical(
    x$call_list$modify_column_hide,
    quote(modify_column_hide(x = as_gtsummary(table_body, ...), columns = -any_of("label")))
  )
})
