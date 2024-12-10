skip_on_cran()

base_tbl_summary <- tbl_summary(trial, include = marker)
test_that("modify_footnote_header(footnote)", {
  # test we can easily replace an existing header footnote
  expect_equal(base_tbl_summary$table_styling$footnote_header$footnote, "Median (Q1, Q3)")
  expect_silent(
    tbl <- base_tbl_summary |>
      modify_footnote_header(
        footnote = "testing N={N}; n={n}; p={p}",
        columns = all_stat_cols()
      )
  )
  expect_equal(
    tbl$table_styling$footnote_header,
    dplyr::tribble(
      ~column,                    ~footnote, ~text_interpret, ~replace, ~remove,
      "stat_0",           "Median (Q1, Q3)",        "gt::md",     TRUE,   FALSE,
      "stat_0", "testing N=200; n=200; p=1",        "gt::md",     TRUE,   FALSE
    )
  )

  # test that two footnotes can be placed in the same header
  expect_silent(
    tbl <- base_tbl_summary |>
      modify_footnote_header(
        footnote = "testing N={N}; n={n}; p={p}",
        columns = all_stat_cols(),
        replace = FALSE
      )
  )
  expect_equal(
    tbl$table_styling$footnote_header,
    dplyr::tribble(
      ~column,                    ~footnote, ~text_interpret, ~replace, ~remove,
      "stat_0",           "Median (Q1, Q3)",        "gt::md",     TRUE,   FALSE,
      "stat_0", "testing N=200; n=200; p=1",        "gt::md",    FALSE,   FALSE
    )
  )
})

test_that("remove_footnote_header(footnote)", {
  # test we can remove footnotes from the headers
  expect_silent(
    tbl <- base_tbl_summary |>
      modify_footnote_header(
        footnote = "testing",
        columns = all_stat_cols(),
        replace = FALSE
      ) |>
      remove_footnote_header(columns = all_stat_cols())
  )
  expect_equal(
    tbl$table_styling$footnote_header,
    dplyr::tribble(
      ~column,         ~footnote, ~text_interpret, ~replace, ~remove,
      "stat_0", "Median (Q1, Q3)",        "gt::md",     TRUE,   FALSE,
      "stat_0",         "testing",        "gt::md",    FALSE,   FALSE,
      "stat_0",                NA,        "gt::md",     TRUE,    TRUE
    )
  )
})
