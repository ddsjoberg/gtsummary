skip_on_cran()

base_tbl_summary <-
  tbl_summary(trial, include = marker) |>
  remove_footnote_header(columns = everything())

test_that("modify_footnote_body(footnote)", {
  # test we can easily replace an existing header
  expect_silent(
    tbl <- base_tbl_summary |>
      modify_footnote_body(
        footnote = "this will not appear",
        columns = label,
        rows = row_type == "label"
      ) |>
      modify_footnote_body(
        footnote = "this _will_ appear; N = {N}",
        columns = label,
        rows = row_type == "label"
      )
  )
  expect_equal(
    tbl$table_styling$footnote_body,
    dplyr::tribble(
      ~column,                ~rows,                     ~footnote, ~text_interpret, ~replace, ~remove,
      "label", ~row_type == "label",        "this will not appear",        "gt::md",     TRUE,   FALSE,
      "label", ~row_type == "label", "this _will_ appear; N = 200",        "gt::md",     TRUE,   FALSE
    ),
    ignore_attr = TRUE
  )
})

test_that("modify_footnote_body(rows) messaging", {
  expect_snapshot(
    error = TRUE,
    base_tbl_summary |>
      modify_footnote_body(
        footnote = "this will not appear",
        columns = label,
        rows = not_a_predicate
      )
  )
})

test_that("remove_footnote_body(footnote)", {
  # test we can remove footnotes from the cells
  expect_silent(
    tbl <- base_tbl_summary |>
      modify_footnote_body(
        footnote = "this will not appear",
        columns = label,
        rows = row_type == "label"
      ) |>
      remove_footnote_body(
        columns = label,
        rows = row_type == "label"
      )
  )
  expect_equal(
    tbl$table_styling$footnote_body,
    dplyr::tribble(
      ~column,                ~rows,              ~footnote, ~text_interpret, ~replace, ~remove,
      "label", ~row_type == "label", "this will not appear",        "gt::md",     TRUE,   FALSE,
      "label", ~row_type == "label",                     NA,        "gt::md",     TRUE,    TRUE
    ),
    ignore_attr = TRUE
  )
})
