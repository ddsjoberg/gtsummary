test_that("add_p.tbl_survfit() works", {
  tbl <- trial |>
    tbl_survfit(
      include = trt,
      y = "Surv(ttdeath, death)",
      times = 12
    )

  # total N added to table is accurate
  expect_error(
    res <- tbl |> add_p(),
    NA
  )
})
