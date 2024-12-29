skip_on_cran()

base_tbl_summary <-
  tbl_summary(trial, by = trt, include = marker) |>
  modify_spanning_header(all_stat_cols() ~ "**Treatment**") |>
  modify_footnote_spanning_header(
    "Randomized Treatment",
    columns = all_stat_cols(),
    level = 1,
  )

test_that("modify_footnote_spanning_header(footnote)", {
  # test we can easily replace an existing spanning header footnote
  expect_silent(
    tbl <-
      base_tbl_summary |>
      remove_footnote_spanning_header(
        columns = all_stat_cols(),
        level = 1L
      ) |>
      modify_footnote_spanning_header(
        footnote = "Treatment Recieved",
        columns = all_stat_cols()
      )
  )
  expect_equal(
    tbl$table_styling$footnote_spanning_header,
    dplyr::tribble(
      ~column,               ~footnote, ~level, ~text_interpret, ~replace, ~remove,
      "stat_1", "Randomized Treatment",     1L,        "gt::md",     TRUE,   FALSE,
      "stat_1",                     NA,     1L,        "gt::md",     TRUE,    TRUE,
      "stat_1",   "Treatment Recieved",     1L,        "gt::md",     TRUE,   FALSE
    )
  )

  # test that two footnotes can be placed in the same spanning header
  expect_silent(
    tbl <-
      base_tbl_summary |>
      modify_footnote_spanning_header(
        footnote = "Treatment as of June",
        columns = all_stat_cols(),
        replace = FALSE
      )
  )
  expect_equal(
    tbl$table_styling$footnote_spanning_header,
    dplyr::tribble(
      ~column,               ~footnote, ~level, ~text_interpret, ~replace, ~remove,
      "stat_1", "Randomized Treatment",     1L,        "gt::md",     TRUE,   FALSE,
      "stat_1", "Treatment as of June",     1L,        "gt::md",    FALSE,   FALSE
    )
  )

  # test that we can place footnotes on multiple spanning header levels
  expect_silent(
    tbl <-
      base_tbl_summary |>
      modify_spanning_header(all_stat_cols() ~ "**Treatment 2**", level = 2) |>
      modify_footnote_spanning_header(
        footnote = "Treatment as of June",
        columns = all_stat_cols(),
        level = 2
      )
  )
  expect_equal(
    tbl$table_styling$footnote_spanning_header,
    dplyr::tribble(
      ~column,               ~footnote, ~level, ~text_interpret, ~replace, ~remove,
      "stat_1", "Randomized Treatment",     1L,        "gt::md",     TRUE,   FALSE,
      "stat_1", "Treatment as of June",     2L,        "gt::md",     TRUE,   FALSE
    )
  )
})

test_that("modify_footnote_spanning_header() messaging", {
  expect_snapshot(
    error = TRUE,
    base_tbl_summary |>
      modify_footnote_spanning_header(
        footnote = "Treatment as of June",
        columns = all_stat_cols(),
        level = 0L
      )
  )
  expect_snapshot(
    error = TRUE,
    base_tbl_summary |>
      remove_footnote_spanning_header(
        columns = all_stat_cols(),
        level = 0L
      )
  )
})
