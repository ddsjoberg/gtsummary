skip_on_cran()

# tbl_hierarchical(data) ------------------------------------------------------------
test_that("tbl_hierarchical(data)", {
  # creates table when data frame is passed
  expect_snapshot(tbl_hierarchical(data = trial) |> as.data.frame())
  expect_snapshot(tbl_hierarchical(data = mtcars) |> as.data.frame())
  expect_snapshot(tbl_hierarchical(data = iris) |> as.data.frame())
})

test_that("tbl_hierarchical(data) errors properly", {
  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_hierarchical())
  expect_snapshot(error = TRUE, tbl_hierarchical(data = letters))
  expect_snapshot(error = TRUE, tbl_hierarchical(data = dplyr::tibble()))
})

# tbl_hierarchical(by) --------------------------------------------------------------
