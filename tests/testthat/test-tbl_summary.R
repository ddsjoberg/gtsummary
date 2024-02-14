test_that("tbl_summary(data)", {
  # creates table when data frame is passed
  expect_snapshot(tbl_summary(data = trial) |> as.data.frame())
  expect_snapshot(tbl_summary(data = mtcars) |> as.data.frame())
  expect_snapshot(tbl_summary(data = iris) |> as.data.frame())
})

test_that("tbl_summary(data) errors properly", {
  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_summary())
  expect_snapshot(error = TRUE, tbl_summary(data = letters))
  expect_snapshot(error = TRUE, tbl_summary(data = dplyr::tibble()))
})


test_that("tbl_summary(by)", {
  expect_snapshot(tbl_summary(data = trial, by = trt) |> as.data.frame())
  expect_snapshot(tbl_summary(data = mtcars, by = am) |> as.data.frame())
  expect_snapshot(tbl_summary(data = iris, by = Species) |> as.data.frame())
  })

test_that("tbl_summary(by)", {
  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_summary(mtcars, by = c("mpg", "am")))
})

test_that("tbl_summary(label)", {
  expect_error(
    tbl <- tbl_summary(
      mtcars,
      by = am,
      label = list(mpg = "New mpg", cyl = "New cyl"),
      include = c(mpg, cyl)
    ),
    NA
  )
  expect_snapshot(as.data.frame(tbl))

  expect_equal(
    tbl$table_body |>
      dplyr::filter(row_type %in% "header") |>
      dplyr::pull(label),
    c("New mpg", "New cyl")
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
  # ensure grade is coerced to dichotomous and response defaults to dichotomous
  expect_error(
    tbl <- tbl_summary(trial, value = "grade" ~ "III", include = c(grade, response)),
    NA
  )
  expect_snapshot(as.data.frame(tbl))

  # check all summary types are assigned to dichotomous
  expect_equal(
    tbl$table_body$summary_type |> unique(),
    "dichotomous"
  )
})
