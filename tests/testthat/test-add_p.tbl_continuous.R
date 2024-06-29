test_that("add_p.tbl_continuous() works", {
  expect_silent(
    trial |>
      tbl_continuous(variable = age, by = trt, include = grade) |>
      add_p()
  )
})
