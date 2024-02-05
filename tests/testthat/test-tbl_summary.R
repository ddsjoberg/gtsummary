test_that("standard tbl_summary() creates correct", {
  expect_snapshot(tbl_summary(data = trial) |> as.data.frame())
  expect_snapshot(tbl_summary(data = mtcars) |> as.data.frame())
  expect_snapshot(tbl_summary(data = iris) |> as.data.frame())
})


test_that("tbl_summary(by) creates output without error/warning", {
  expect_snapshot(tbl_summary(data = trial, by = trt) |> as.data.frame())
  expect_snapshot(tbl_summary(data = mtcars, by = am) |> as.data.frame())
  expect_snapshot(tbl_summary(data = iris, by = Species) |> as.data.frame())
})

test_that("tbl_summary(label) allows for named list input", {
  expect_snapshot(
    tbl_summary(
      mtcars,
      by = am,
      label = list(mpg = "New mpg", cyl = "New cyl"),
      include = c(mpg, cyl)
    ) |>
      as.data.frame()
  )
})

test_that("tbl_summary(sort) works", {
  expect_equal(
    tbl_summary(mtcars, sort = all_categorical() ~ "frequency", include = cyl) |>
      getElement("table_body") |>
      dplyr::filter(row_type %in% "level") |>
      dplyr::pull(label),
    c("8", "4", "6")
  )

  # proper errors are returned
  expect_snapshot(
    error = TRUE,
    tbl_summary(mtcars, sort = list(all_categorical() ~ c("frequency", "two")))
  )
  expect_snapshot(
    error = TRUE,
    tbl_summary(mtcars, sort = list(all_categorical() ~ "freq5555uency"))
  )
})

test_that("tbl_summary(value) works", {
  expect_snapshot(
    tbl_summary(trial, value = "grade" ~ "III", include = grade) |>
      as.data.frame()
  )
})
