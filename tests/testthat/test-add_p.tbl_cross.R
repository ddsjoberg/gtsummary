test_that("add_p.tbl_cross() works", {
  expect_silent(
    trial |>
      tbl_cross(row = stage, col = trt) |>
      add_p()
  )
})
