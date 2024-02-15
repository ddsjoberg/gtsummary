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

test_that("tbl_summary(by) errors properly", {
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

test_that("tbl_summary(label) errors properly", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial["age"], label = list(age = letters))
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(trial["age"], label = letters)
  )
})

test_that("tbl_summary(statistic)", {
  # categorical summary
  expect_equal(
    trial |>
      tbl_summary(
        include = response,
        statistic =
          response ~ "n={n} | N={N} | p={p} | N_obs={N_obs} | N_miss={N_miss} | N_nonmiss={N_nonmiss} | p_miss={p_miss} | p_nonmiss={p_nonmiss}",
        missing = "no"
      ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0),
    "n=61 | N=193 | p=32 | N_obs=200 | N_miss=7 | N_nonmiss=193 | p_miss=3.5 | p_nonmiss=97"
  )

  # continuous summary, testing cv function and passed in formula
  expect_equal({
    cv  <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
    trial |>
      tbl_summary(
        include = age,
        statistic =
          age ~ "cv={cv} | N_obs={N_obs} | N_miss={N_miss} | N_nonmiss={N_nonmiss} | p_miss={p_miss} | p_nonmiss={p_nonmiss}",
        missing = "no"
      ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0)},
    "cv=30 | N_obs=200 | N_miss=11 | N_nonmiss=189 | p_miss=5.5 | p_nonmiss=95"
  )

  # continuous summary, testing cv function and passed in named list
  expect_equal({
    cv  <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
    trial |>
      tbl_summary(
        include = age,
        statistic =
          list(age = "cv={cv} | N_obs={N_obs} | N_miss={N_miss} | N_nonmiss={N_nonmiss} | p_miss={p_miss} | p_nonmiss={p_nonmiss}"),
        missing = "no"
      ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0)},
    "cv=30 | N_obs=200 | N_miss=11 | N_nonmiss=189 | p_miss=5.5 | p_nonmiss=95"
  )
})

test_that("tbl_summary(statistic) errors properly", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = response,
      statistic = ~"{n} ({not_a_statistic})"
    )
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = age,
      statistic = ~"({not_a_summary_statistic})"
    )
  )
})


test_that("tbl_summary(value)", {
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

  # check we can pass unobserved levels to values
  expect_equal(
    trial |>
      dplyr::mutate(
        grade = factor(grade, levels = c("I", "II", "III", "IV")),
        response = TRUE
      ) |>
      tbl_summary(
        include = c(grade, response),
        value = list(grade = "IV", response = FALSE)
      ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0) |>
      unique(),
    "0 (0%)"
  )
})

test_that("tbl_summary(value) errors properly", {
  # passing a value that does not exist
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, value = "grade" ~ "IV", include = c(grade, response))
  )
})


test_that("tbl_summary(sort)", {
  expect_equal(
    tbl_summary(mtcars, sort = all_categorical() ~ "frequency", include = cyl) |>
      getElement("table_body") |>
      dplyr::filter(row_type %in% "level") |>
      dplyr::pull(label),
    c("8", "4", "6")
  )
})

test_that("tbl_summary(sort) errors properly", {
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


