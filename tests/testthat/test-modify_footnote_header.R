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

  # test all footnotes are removed by default
  expect_true(
    base_tbl_summary |>
      modify_footnote_header(
        footnote = "testing",
        columns = all_stat_cols(),
        replace = FALSE
      ) |>
      remove_footnote_header() |>
      getElement("table_styling") |>
      getElement("footnote_header") |>
      dplyr::slice_tail(by = column, n = 1L) |>
      getElement("remove") |>
      unique()
  )
})

test_that("modify_footnote_header(text_interpret = 'none') stores identity and renders verbatim", {
  # accepts "none" and stores the `identity` interpret function (#1987)
  tbl <- base_tbl_summary |>
    modify_footnote_header(footnote = "*p<0.05", columns = label, text_interpret = "none")
  expect_equal(
    tbl$table_styling$footnote_header |>
      dplyr::filter(footnote == "*p<0.05") |>
      dplyr::pull("text_interpret"),
    "identity"
  )

  # "md" continues to store gt::md
  tbl_md <- base_tbl_summary |>
    modify_footnote_header(footnote = "*p<0.05", columns = label, text_interpret = "md")
  expect_equal(
    tbl_md$table_styling$footnote_header |>
      dplyr::filter(footnote == "*p<0.05") |>
      dplyr::pull("text_interpret"),
    "gt::md"
  )

  # invalid values are rejected
  expect_error(
    modify_footnote_header(base_tbl_summary, footnote = "x", columns = label, text_interpret = "latex")
  )

  skip_if_pkg_not_installed("gt")
  # "none" renders the asterisks verbatim (not as emphasis)
  html_none <- base_tbl_summary |>
    modify_footnote_header(footnote = "*p<0.05; **p<0.01", columns = label, text_interpret = "none") |>
    as_gt() |>
    gt::as_raw_html()
  expect_match(html_none, "\\*p", fixed = FALSE)
  expect_false(grepl("<em>p&lt;0.05", html_none))

  # "md" interpretation is unchanged (bold rendered, literal asterisks gone)
  html_md <- base_tbl_summary |>
    modify_footnote_header(footnote = "**bold note**", columns = label, text_interpret = "md") |>
    as_gt() |>
    gt::as_raw_html()
  expect_false(grepl("\\*\\*bold note\\*\\*", html_md))
})
