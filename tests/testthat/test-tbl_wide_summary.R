skip_on_cran()

test_that("tbl_wide_summary(data)", {
  # works with standard use
  expect_snapshot(
    trial |>
      tbl_wide_summary(include = c(response, grade)) |>
      as.data.frame()
  )

  # error when not a data frame
  expect_error(
    tbl_wide_summary(letters, include = c(response, grade)),
    "The `data` argument must be class <data.frame>"
  )
})


test_that("tbl_wide_summary(label)", {
  # check specified labels are applied
  expect_equal(
    tbl_wide_summary(
      trial,
      include = c(response, grade),
      label = response ~ "Response of the Tumor"
    ) |>
      as_tibble(col_label = FALSE) |>
      dplyr::pull("label") |>
      getElement(1L),
    "Response of the Tumor"
  )

  expect_error(
    tbl_wide_summary(
      trial,
      include = c(response, grade),
      label = response ~ c("one", "two")
    ),
    "value must be a string"
  )
})


test_that("tbl_wide_summary(statistic)", {
  # works with standard use
  expect_snapshot(
    trial |>
      tbl_wide_summary(include = c(age, marker), statistic = c("{mean}", "{sd}")) |>
      as.data.frame()
  )

  # we get an error with incorrect values passed
  expect_error(
    trial |>
      tbl_wide_summary(include = c(age, marker), statistic = mean),
    "The `statistic` argument must be class <character>, not a function"
  )
})

test_that("tbl_wide_summary(digits)", {
  # digits value properly recycled for all stats
  expect_equal(
    trial |>
      tbl_wide_summary(include = age, statistic = c("{mean}", "{sd}"), digits = ~2) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::select(all_stat_cols()) |>
      as.list(),
    list(stat_1 = "47.24", stat_2 = "14.31")
  )

  # can change digits of a single stat
  expect_equal(
    trial |>
      tbl_wide_summary(include = age, statistic = c("{mean}", "{sd}"), digits = ~list(sd = 2)) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::select(all_stat_cols()) |>
      as.list(),
    list(stat_1 = "47", stat_2 = "14.31")
  )

  # can change digits of a single stat with a function
  expect_equal(
    trial |>
      tbl_wide_summary(include = age, statistic = c("{mean}", "{sd}"), digits = ~list(sd = label_style_number(2))) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::select(all_stat_cols()) |>
      as.list(),
    list(stat_1 = "47", stat_2 = "14.31")
  )
})

test_that("tbl_wide_summary(type)", {
  # we get an error message with incompatible types
  expect_error(
    trial |>
      tbl_wide_summary(include = c(age, grade)),
    'The summary types must all be "continuous" or all be "categorical" and "dichotomous".'
  )
})

test_that("tbl_wide_summary(sort)", {
  expect_equal(
    tbl_wide_summary(trial, include = trt, sort = ~"frequency") |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull("label"),
    c("trt", "Drug B", "Drug A")
  )
})
