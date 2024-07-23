skip_on_cran()

test_that("tbl_continuous(data)", {
  # testing a typical case
  expect_silent(
    tbl <- tbl_continuous(trial, variable = marker, include = c(grade, response), by = trt)
  )
  expect_snapshot(tbl |> as.data.frame())

  # pass some that is no a data frame
  expect_error(tbl_continuous(letters))
})

test_that("tbl_continuous(variable)", {
  # not passing a variable
  expect_error(tbl_continuous(trial, include = grade))

  # passing more than one variable
  expect_error(tbl_continuous(trial, variable = c(age, marker), include = grade))
})

test_that("tbl_continuous(variable) messaging", {
  # passing a categorical variable
  expect_snapshot(
    tbl_continuous(trial, variable = grade, include = trt) |>
      as.data.frame()
  )
})


test_that("tbl_continuous(include)", {
  # using the default is the same as specifying include
  expect_equal(
    trial[c("age", "trt")] |>
      tbl_continuous(variable = age) |>
      as.data.frame(),
    trial |>
      tbl_continuous(variable = age, include = trt) |>
      as.data.frame()
  )


  # we get an error with bad inputs
  expect_error(suppressWarnings(tbl_continuous(trial, variable = age, include = letters)))
  expect_error(suppressWarnings(tbl_continuous(trial, variable = age, include = mtcars)))
})

test_that("tbl_continuous(digits)", {
  # digits recycles for all stats
  expect_equal(
    tbl_continuous(trial, variable = age, include = trt, digits = trt ~ 2) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull("stat_0") |>
      na.omit(),
    c("46.00 (37.00, 60.00)", "48.00 (39.00, 56.00)"),
    ignore_attr = TRUE
  )

  # three digits are applied to each stat
  tbl <- tbl_continuous(trial, variable = age, include = trt, digits = trt ~ c(0, 1, 2)) # ill use this a couple of times
  expect_equal(
    tbl |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull("stat_0") |>
      na.omit(),
    c("46 (37.0, 60.00)", "48 (39.0, 56.00)"),
    ignore_attr = TRUE
  )

  # check we get the same answer using a named list
  expect_equal(
    tbl_continuous(trial, variable = age, include = trt, digits = list(trt = c(0, 1, 2))) |>
      as.data.frame(col_label = FALSE),
    tbl |>
      as.data.frame(col_label = FALSE)
  )

  # check we get the same answer using a named list of list
  expect_equal(
    tbl_continuous(trial, variable = age, include = trt, digits = list(trt = list(0, 1, 2))) |>
      as.data.frame(col_label = FALSE),
    tbl |>
      as.data.frame(col_label = FALSE)
  )

  # check we get the same answer using a named list of named stats list (out of order)
  expect_equal(
    tbl_continuous(trial, variable = age, include = trt, digits = list(trt = list(p75 = 2, p25 = 1, median = 0))) |>
      as.data.frame(col_label = FALSE),
    tbl |>
      as.data.frame(col_label = FALSE)
  )

  # check we get the same answer using a named list of named stats functions
  expect_equal(
    tbl_continuous(
      trial, variable = age,
      include = trt,
      digits = list(trt = list(p75 = label_style_number(digits = 2),
                               p25 = label_style_number(digits = 1),
                               median = label_style_number(digits = 0)))
    ) |>
      as.data.frame(col_label = FALSE),
    tbl |>
      as.data.frame(col_label = FALSE)
  )

  # check that unspecified digits are filled with the default
  expect_equal(
    tbl_continuous(
      trial, variable = age,
      include = c(trt, grade),
      digits = list(grade = 2)
    ) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull(stat_0) %>%
      `[`(2:3),
    c("46 (37, 60)", "48 (39, 56)")
  )
})

test_that("tbl_continuous(by)", {
  # we get correct output with standard use
  expect_snapshot(
    tbl_continuous(trial, variable = age, include = grade, by = trt) |>
      as.data.frame()
  )

  # we get an error with bad inputs
  expect_error(
    suppressWarnings(tbl_continuous(trial, variable = age, include = grade, by = pi))
  )
})

test_that("tbl_continuous(by) messaging", {
  # we get a helpful message about `tbl_strata()` when more than one var specified
  expect_snapshot(
    error = TRUE,
    tbl_continuous(trial, variable = age, include = grade, by = c(stage, trt))
  )
})


test_that("tbl_continuous(statistic)", {
  # works with standard use
  expect_equal(
    tbl_continuous(trial, variable = age, include = grade, statistic = ~ "{mean}")  |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull(stat_0) |>
      na.omit(),
    c("46", "48", "48"),
    ignore_attr = TRUE
  )

  # can specify different stats for different variables
  expect_snapshot(
    tbl_continuous(
      trial,
      variable = age,
      include = c(trt, grade),
      statistic = list(trt = "{var}", grade = "{sd}")
    ) |>
      as.data.frame()
  )
})

test_that("tbl_continuous(statistic) messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_continuous(trial, variable = age, include = grade, statistic = ~ letters)
  )

  expect_snapshot(
    error = TRUE,
    tbl_continuous(trial, variable = age, include = grade, statistic = ~ "mean")
  )
})


test_that("tbl_continuous(label)", {
  # we are able to change the labels
  expect_equal(
    tbl_continuous(trial, variable = age, include = c(trt, grade), label = list(trt = "TRT")) |>
      getElement("table_body") |>
      getElement("var_label") |>
      unique(),
    c("TRT", "Grade")
  )
})

test_that("tbl_continuous(label) messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_continuous(trial, variable = age, include = c(trt, grade), label = list(trt = mean))
  )
})
