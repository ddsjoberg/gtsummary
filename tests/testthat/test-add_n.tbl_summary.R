test_that("add_n.tbl_summary() works", {
  expect_error(
    tbl <- trial |>
      tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
      add_n(),
    NA
  )

  expect_equal(
    as.data.frame(tbl, col_labels = FALSE)$n,
    c("189", NA, "200", NA, NA, NA, "193", NA)
  )
})

test_that("add_n.tbl_summary(statistic) error messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
      add_n(statistic = "no_curlies")
  )
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
      add_n(statistic = "{not_a_stat}")
  )
})
