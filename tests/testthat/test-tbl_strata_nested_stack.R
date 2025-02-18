test_that("tbl_strata_nested_stack() works", {
  expect_silent(
    tbl <-
      tbl_strata_nested_stack(
        trial,
        strata = trt,
        ~ .x |>
          tbl_summary(include = c(age, grade), missing = "no") |>
          modify_header(all_stat_cols() ~ "**Summary Statistics**")
      )
  )

  # check indenting of first row, which should not be indented
  expect_true({
    indent_last_row <- tbl$table_styling$indent |>
      dplyr::filter(column == "label") |>
      dplyr::filter(dplyr::n() == dplyr::row_number())

    indent_last_row$n_spaces == 0L &&
      indent_last_row$rows[[1]] |> rlang::quo_squash() |> rlang::expr_deparse() == ".data$tbl_indent_id1 == 1L"
  })

  # check the 2nd row is indented
  expect_equal(
    tbl |>
      .table_styling_expr_to_row_number() |>
      getElement("table_styling") |>
      getElement("indent") |>
      tidyr::unnest(row_numbers) |>
      dplyr::filter(column == "label", row_numbers == 2) |>
      dplyr::pull(n_spaces),
    4L
  )
  # check the levels are indented for grade
  expect_equal(
    tbl |>
      .table_styling_expr_to_row_number() |>
      getElement("table_styling") |>
      getElement("indent") |>
      tidyr::unnest(row_numbers) |>
      dplyr::filter(column == "label", row_numbers == 4) |>
      dplyr::pull(n_spaces),
    8L
  )

  # function works with variables whose names end in _strata
  expect_silent(
    tbl_strata_nested_stack(
      trial |> dplyr::rename(trt_strata = trt),
      strata = trt_strata,
      ~ .x |>
        tbl_summary(include = c(age, grade), missing = "no") |>
        modify_header(all_stat_cols() ~ "**Summary Statistics**")
    )
  )

  # works with multiple strata variables
  expect_silent(
    tbl <-
      tbl_strata_nested_stack(
        trial,
        strata = c(trt, grade),
        ~ .x |>
          tbl_summary(include = age, missing = "no") |>
          modify_header(all_stat_cols() ~ "**Summary Statistics**")
      )
  )
  # check correct indentation
  expect_equal(
    tbl |>
      .table_styling_expr_to_row_number() |>
      getElement("table_styling") |>
      getElement("indent") |>
      tidyr::unnest(row_numbers) |>
      dplyr::filter(column == "label", row_numbers %in% 2:3) |>
      dplyr::arrange(row_numbers) |>
      dplyr::pull(n_spaces),
    c(4L, 8L)
  )
})

test_that("tbl_strata_nested_stack() works with unobserved factor levels", {
  expect_silent(
    tbl <-
      tbl_strata_nested_stack(
        trial |> dplyr::mutate(trt = factor(trt, levels = c("Drug A", "Drug B", "Drug C"))),
        strata = trt,
        ~ .x |>
          tbl_summary(include = c(age, grade), missing = "no") |>
          modify_header(all_stat_cols() ~ "**Summary Statistics**")
      )
  )

  # check there are only two rows that are not indented (i.e. the two observed levels of trt)
  expect_equal(
    tbl |>
      .table_styling_expr_to_row_number() |>
      getElement("table_styling") |>
      getElement("indent") |>
      tidyr::unnest(row_numbers) |>
      dplyr::filter(column == "label") |>
      dplyr::pull(row_numbers) |>
      setdiff(x = seq_len(nrow(tbl$table_body)), y = _),
    c(1L, 7L)
  )
})

test_that("tbl_strata_nested_stack() messaging", {
  # check for protected names are not allowed in strata argument
  expect_snapshot(
    error = TRUE,
    tbl_strata_nested_stack(
      trial |> dplyr::rename(strata = trt),
      strata = strata,
      ~ .x |>
        tbl_summary(include = c(age, grade), missing = "no") |>
        modify_header(all_stat_cols() ~ "**Summary Statistics**")
    )
  )

  # the first column in the table must be a character
  expect_snapshot(
    error = TRUE,
    tbl_strata_nested_stack(
      mtcars,
      strata = am,
      ~ as_gtsummary(.x)
    )
  )

  # check messaging when running the function twice
  expect_snapshot(
    error = TRUE,
    tbl_strata_nested_stack(
      trial,
      strata = trt,
      \(.x) {
        tbl_strata_nested_stack(
          .x,
          strata = grade,
          \(.y) {
            .y |>
              tbl_summary(include = age, missing = "no") |>
              modify_header(all_stat_cols() ~ "**Summary Statistics**")
          }
        )
      }
    )
  )
})
