test_that("tbl_split()", {
  expect_silent(
    tbl <- tbl_summary(trial) |>
      tbl_split(variables = c(marker, grade))
  )
  expect_s3_class(tbl, "tbl_split")

  expect_equal(
    tbl[[1]] |> as.data.frame(),
    tbl_summary(trial, include = c(trt, age, marker)) |> as.data.frame()
  )

  expect_equal(
    tbl[[2]] |> as.data.frame(),
    tbl_summary(trial, include = c(stage, grade)) |> as.data.frame()
  )
})
