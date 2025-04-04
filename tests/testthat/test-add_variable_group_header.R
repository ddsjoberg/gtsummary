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

  # check function works on tbl_svysummary tables
  expect_silent(
    tbl <-
      survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
      tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age)) |>
      add_variable_group_header(header = "the header row", variables = c(Class, Age))
  )
  # header row is in correct location
  expect_equal(
    as_tibble(tbl, col_labels = FALSE)$label[1],
    "the header row"
  )
  # grouped variables are indented.
  expect_equal(
    .table_styling_expr_to_row_number(tbl)$table_styling$indent$row_numbers[[1]],
    2:9
  )

  # check function works on tbl_regression tables
  expect_silent(
    tbl <-
      glm(response ~ age + grade, trial, family = binomial()) |>
      tbl_regression(exponentiate = TRUE) |>
      add_variable_group_header(header = "the header row", variables = age)
  )
  # header row is in correct location
  expect_equal(
    as_tibble(tbl, col_labels = FALSE)$label[1],
    "the header row",
    ignore_attr = TRUE
  )
  # grouped variables are indented.
  expect_true(
    2L %in% .table_styling_expr_to_row_number(tbl)$table_styling$indent$row_numbers[[1]]
  )
})

test_that("add_variable_group_header() messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(include = c(age, response, death, marker), missing = "no") |>
      modify_table_body(~dplyr::relocate(.x, label, .after = "stat_0")) |>
      add_variable_group_header(header = "the header row", variables = c(response, death))
  )
})
