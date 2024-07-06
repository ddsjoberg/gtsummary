skip_on_os("windows")

test_that("show_header_names() works", {
  expect_snapshot(
    tbl_summary(trial, include = age, by = trt, missing = "no") |>
      show_header_names()
  )
})
