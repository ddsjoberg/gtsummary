skip_on_cran()

test_that("modify_source_note(source_note)", {
  expect_silent(
    tbl <- tbl_summary(trial, include = trt) |>
      modify_source_note("Created June 26, 2015") |>
      modify_source_note("Created June 26, 2015")
  )

  expect_equal(
    tbl$table_styling$source_note$source_note,
    rep_len("Created June 26, 2015", length.out = 2L)
  )
})

test_that("modify_source_note(text_interpret)", {
  expect_silent(
    tbl <- tbl_summary(trial, include = trt) |> modify_source_note("Created June 26, 2015", text_interpret = "html")
  )

  expect_equal(
    tbl$table_styling$source_note$text_interpret,
    "gt::html"
  )
})

test_that("modify_source_note() messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = trt) |>
      modify_source_note(source_note = letters)
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = trt) |>
      modify_source_note(source_note = "ttt", text_interpret = letters)
  )
})

test_that("remove_source_note(source_note_id)", {
  expect_silent(
    tbl <- tbl_summary(trial, include = trt) |>
      modify_source_note("Created June 26, 2015") |>
      modify_source_note("Created June 26, 2015")
  )

  expect_true(
    tbl |>
      remove_source_note(source_note_id = 1:2) |>
      getElement("table_styling") |>
      getElement("source_note") |>
      getElement("remove") |>
      unique()
  )

  expect_true(
    tbl |>
      remove_source_note(source_note_id = NULL) |>
      getElement("table_styling") |>
      getElement("source_note") |>
      getElement("remove") |>
      unique()
  )

  # test all source notes are removed by default
  expect_true(
    tbl_summary(trial, include = trt) |>
      modify_source_note("Created June 26, 2015") |>
      modify_source_note("Created June 26, 2015") |>
      remove_source_note() |>
      getElement("table_styling") |>
      getElement("source_note") |>
      getElement("remove") |>
      unique()
  )
})

test_that("remove_source_note(source_note_id) messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = trt) |>
      modify_source_note("Created June 26, 2015") |>
      remove_source_note(source_note_id = 100)
  )
})

test_that("modify_source_note(text_interpret = 'none') stores identity and renders verbatim", {
  # accepts "none" and stores the `identity` interpret function (#1987)
  tbl <- tbl_summary(trial, include = marker) |>
    modify_source_note("*literal note*", text_interpret = "none")
  expect_equal(
    tbl$table_styling$source_note$text_interpret,
    "identity"
  )

  # invalid values are rejected
  expect_error(
    modify_source_note(tbl_summary(trial, include = marker), "x", text_interpret = "latex")
  )

  skip_if_pkg_not_installed("gt")
  html <- as_gt(tbl) |> gt::as_raw_html()
  expect_match(html, "\\*literal note\\*", fixed = FALSE)
})
