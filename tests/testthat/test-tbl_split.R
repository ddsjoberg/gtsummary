skip_on_cran()

test_that("tbl_split_by_rows()", {
  expect_silent(
    tbl <- tbl_summary(trial) |>
      tbl_split_by_rows(variables = c(marker, grade))
  )
  expect_s3_class(tbl, "tbl_split_by_rows")

  expect_equal(
    tbl[[1]] |> as.data.frame(),
    tbl_summary(trial, include = c(trt, age, marker)) |> as.data.frame()
  )

  expect_equal(
    tbl[[2]] |> as.data.frame(),
    tbl_summary(trial, include = c(stage, grade)) |> as.data.frame()
  )
})

test_that("tbl_split_by_columns()", {
  expect_silent(
    tbl <- tbl_summary(trial, by = trt) |>
      tbl_split_by_columns(groups = list("stat_1", "stat_2"))
  )
  expect_s3_class(tbl, "tbl_split_by_columns")


  expect_equal(
    tbl[[1]] |> as.data.frame(),
    tbl_summary(trial |> dplyr::filter(trt == "Drug A"), by = trt) |> as.data.frame()
  )

  expect_equal(
    tbl[[2]] |> as.data.frame(),
    tbl_summary(trial |> dplyr::filter(trt == "Drug B"), by = trt) |> as.data.frame()
  )
})

test_that("tbl_split_by_columns() warns if not all columns are selected", {
  expect_snapshot(
    tbl <- tbl_summary(trial, by = trt) |>
      tbl_split_by_columns(groups = list("stat_2"))
  )
})
