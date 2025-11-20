skip_on_cran()

a_tbl <- trial |>
  tbl_summary(by = trt)

test_that("head.gtsummary() works", {
  expect_identical(
    a_tbl |> head(n = 4L),
    {
      a_tbl$table_body <- head(a_tbl$table_body, n = 4L)
      a_tbl
    }
  )
})


test_that("tail.gtsummary() works", {
  expect_identical(
    a_tbl |> tail(n = 4L),
    {
      a_tbl$table_body <- tail(a_tbl$table_body, n = 4L)
      a_tbl
    }
  )
})

test_that("[.gtsummary() works", {
  expect_identical(
    a_tbl[1:4,],
    a_tbl |> head(4L)
  )
})
