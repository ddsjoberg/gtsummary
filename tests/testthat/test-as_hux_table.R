test_that("as_hux_table() standard case", {
  expect_error(
    trial |>
      tbl_summary(include = age) |>
      as_hux_table(),
    NA
  )
})
