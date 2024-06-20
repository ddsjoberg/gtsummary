test_that("add_p.tbl_survfit() works", {
  expect_silent(
    trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        times = 12
      ) |>
      add_p()
  )
})
