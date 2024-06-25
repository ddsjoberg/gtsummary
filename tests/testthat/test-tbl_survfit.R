test_that("tbl_survfit() works", {
  expect_error(
    trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        times = 12
      ),
    NA
  )

  expect_error(
    trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        probs = 0.5
      ),
    NA
  )
})
