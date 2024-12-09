skip_on_cran()

base_tbl_summary <- tbl_summary(trial, include = marker)
test_that("modify_footnote_header(footnote)", {
  expect_silent(
    tbl <- base_tbl_summary |>
      modify_footnote_header(
        footnote = "testing",
        columns = all_stat_cols()
      )
  )


})
