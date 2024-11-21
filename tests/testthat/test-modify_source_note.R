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
})

test_that("remove_source_note(source_note_id) messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = trt) |>
      modify_source_note("Created June 26, 2015") |>
      remove_source_note(source_note_id = 100)
  )
})
