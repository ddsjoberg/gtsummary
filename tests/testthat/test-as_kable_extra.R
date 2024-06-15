test_that("as_kable_extra() standard case", {
  expect_error(
    trial |>
      tbl_summary(include = age) |>
      as_kable_extra(format = "latex"),
    NA
  )
})
