skip_on_cran()

test_that("head.gtsummary() works", {
  a_tbl <- trial |>
    tbl_summary(by = trt)

  expect_identical(
    a_tbl |> head(n = 4L),
    {
      a_tbl$table_body <- head(a_tbl$table_body, n = 4L)
      a_tbl
    }
  )
})


test_that("tail.gtsummary() works", {
  a_tbl <- trial |>
    tbl_summary(by = trt)

  expect_identical(
    a_tbl |> tail(n = 4L),
    {
      a_tbl$table_body <- tail(a_tbl$table_body, n = 4L)
      a_tbl
    }
  )
})
