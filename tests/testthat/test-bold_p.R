test_that("bold_p() works", {
  expect_equal(
    trial |>
      tbl_summary(by = trt, include = c(response, marker, trt), missing = "no") |>
      add_p() |>
      bold_p(t = 0.1) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull(p.value),
    c("0.5", "__0.085__")
  )

  expect_equal(
    trial |>
      tbl_summary(by = trt, include = c(response, marker, trt), missing = "no") |>
      add_p() |>
      add_q() |>
      bold_p(t = 0.25, q = TRUE) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull(q.value),
    c("0.5", "__0.2__")
  )
})


test_that("bold_p() messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(by = trt, include = c(response, marker, trt), missing = "no") |>
      bold_p()
  )
})
