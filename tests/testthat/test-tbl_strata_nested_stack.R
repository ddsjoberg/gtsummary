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

  # check the tbl_ids have been applied correctly
  expect_equal(
    names(tbl$tbls),
    unique(trial$trt) |>
      sort() |>
      cli::cli_format() %>%
      paste("trt", ., sep = "=")
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

  # check the tbl_ids
  expect_equal(
    names(tbl$tbls),
    c('trt=\"Drug A\",grade=\"I\"', 'trt=\"Drug A\",grade=\"II\"', 'trt=\"Drug A\",grade=\"III\"',
      'trt=\"Drug B\",grade=\"I\"', 'trt=\"Drug B\",grade=\"II\"', 'trt=\"Drug B\",grade=\"III\"')
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

  # check indentation is correct when only one level present in stratum
  expect_true({
    df_indent <- trial |>
      dplyr::filter(trt == "Drug A") |>
      tbl_strata_nested_stack(
        strata = "trt",
        ~ tbl_summary(.x, include = "age", missing = "no")
      ) |>
      getElement("table_styling") |>
      getElement("indent") |>
      dplyr::filter(n_spaces == 0L)

    (df_indent$column == "label") &&
      (df_indent$rows[[1]] |> rlang::quo_squash() |> rlang::expr_deparse() == ".data$tbl_indent_id1 == 1L")
  })

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

# addressing issue #2179
test_that("tbl_strata_nested_stack() unobserved combinations", {
  # no error when there are unobserved combinations of the strata variables
  expect_silent(
    tbl_strata_nested_stack(
      cards::ADTTE,
      strata = c(SEX, RACE),
      .tbl_fun = ~ .x |>
        tbl_summary(include = AGE, type = AGE ~ "continuous"),
      quiet = TRUE
    )
  )
})

test_that("tbl_strata_nested_stack() works with tables without prior indentation in first column", {
  expect_silent(
    tbl <-
      tbl_strata_nested_stack(
        trial,
        strata = trt,
        ~ .x |>
          tbl_summary(include = c(age, grade), missing = "no") |>
          modify_header(all_stat_cols() ~ "**Summary Statistics**") |>
          # add column without prior indentation
          modify_column_unhide("var_label")
      )
  )

  # no indentation in second column labels
  expect_true(
    tbl |>
      .table_styling_expr_to_row_number() |>
      getElement("table_styling") |>
      getElement("indent") |>
      tidyr::unnest(row_numbers) |>
      dplyr::filter(column == "label", row_numbers %in% 2:3) |>
      dplyr::pull(n_spaces) |>
      is_empty()
  )
  # correct indentation in first column labels
  expect_equal(
    tbl |>
      .table_styling_expr_to_row_number() |>
      getElement("table_styling") |>
      getElement("indent") |>
      tidyr::unnest(row_numbers) |>
      dplyr::filter(column == "var_label", row_numbers %in% 2:3) |>
      dplyr::pull(n_spaces),
    c(4L, 4L)
  )
})

test_that("tbl_strata_nested_stack() keeps second-level headers in all groups with 3+ strata levels (#2418)", {
  df <- tidyr::expand_grid(
    SEX     = c("Female", "Male"),
    PARAMCD = c("A", "C", "W", "Y"),
    VISIT   = c("D01", "D31", "D31/D01")
  ) |>
    dplyr::mutate(
      SEX     = factor(SEX,     c("Female", "Male")),
      PARAMCD = factor(PARAMCD, c("A", "C", "W", "Y")),
      VISIT   = factor(VISIT,   c("D01", "D31", "D31/D01"))
    ) |>
    tidyr::crossing(
      USUBJID = sprintf("S-%03d", 1:4),
      TRT     = c("G1", "G2")
    ) |>
    dplyr::mutate(AVAL = seq_len(dplyr::n()))

  expect_silent(
    tbl <-
      tbl_strata_nested_stack(
        data     = df,
        strata   = c(SEX, PARAMCD, VISIT),
        .tbl_fun = function(d) tbl_summary(d, by = TRT, include = AVAL),
        quiet    = TRUE
      )
  )

  # extract the nesting header rows (those without an associated variable)
  hdr <- tbl$table_body[is.na(tbl$table_body$variable), "label", drop = TRUE] |>
    as.character()

  # the second-level (PARAMCD) headers must appear under BOTH SEX groups, not
  # just the first one. Previously, only the first PARAMCD header rendered under
  # the second SEX group.
  expect_equal(sum(hdr == "Female"), 1L)
  expect_equal(sum(hdr == "Male"), 1L)
  expect_equal(sum(hdr == "A"), 2L)
  expect_equal(sum(hdr == "C"), 2L)
  expect_equal(sum(hdr == "W"), 2L)
  expect_equal(sum(hdr == "Y"), 2L)

  # the header structure should be symmetric across the two SEX groups: each
  # SEX section contains the same sequence of nested headers
  female_idx <- which(hdr == "Female")
  male_idx <- which(hdr == "Male")
  expect_equal(female_idx, 1L)
  female_section <- hdr[female_idx:(male_idx - 1L)]
  male_section <- hdr[male_idx:length(hdr)]
  expect_equal(female_section[-1], male_section[-1])
})
