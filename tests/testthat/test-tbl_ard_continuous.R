skip_on_cran()

test_that("tbl_ard_continuous(cards)", {
  # works with correct specification
  expect_snapshot(
    cards::ard_continuous(trial, by = grade, variables = age) |>
      tbl_ard_continuous(variable = "age", include = "grade") |>
      as.data.frame()
  )
  expect_snapshot(
    cards::bind_ard(
      cards::ard_continuous(trial, by = c(trt, grade), variables = age),
      cards::ard_categorical(trial, trt)
    ) |>
      tbl_ard_continuous(variable = "age", include = "grade", by = "trt") |>
      as.data.frame()
  )

  # no error when no tabulation of the 'by' data is passed
  expect_snapshot(
    cards::ard_continuous(trial, by = c(trt, grade), variables = age) |>
      tbl_ard_continuous(by = trt, variable = age, include = grade) |>
      as.data.frame()
  )
})

test_that("tbl_ard_continuous(cards) error messaging", {
  # we get an error when non-cards object passed
  expect_error(
    tbl_ard_continuous(cards = letters, include = trt, variable = age),
    "must be class"
  )

  expect_snapshot(
    error = TRUE,
    cards::bind_ard(
      cards::ard_continuous(trial, by = c(trt, grade), variables = age)
    ) |>
      tbl_ard_continuous(variable = "trt", include = "grade", by = "age")
  )

  expect_snapshot(
    error = TRUE,
    cards::bind_ard(
      cards::ard_continuous(trial, by = c(trt, grade), variables = age)
    ) |>
      tbl_ard_continuous(variable = "age", include = "trt", by = "grade")
  )
})

test_that("tbl_ard_summary(label) argument works", {
  expect_equal(
    cards::bind_ard(
      # the primary ARD with the results
      cards::ard_continuous(trial, by = grade, variables = age),
      # add missing and attributes ARD
      cards::ard_missing(trial, by = grade, variables = age)
    ) |>
      tbl_ard_continuous(variable = "age", include = "grade", label = grade ~ "Updated GRADE!") |>
      getElement("table_body") |>
      dplyr::filter(row_type == "label") |>
      dplyr::pull(label),
    "Updated GRADE!"
  )

  expect_equal(
    cards::bind_ard(
      # the primary ARD with the results
      cards::ard_continuous(trial, by = grade, variables = age),
      # add missing and attributes ARD
      cards::ard_missing(trial, by = grade, variables = age),
      cards::ard_attributes(trial, variables = c(grade, age))
    ) |>
      tbl_ard_continuous(variable = "age", include = "grade", label = grade ~ "Updated GRADE!") |>
      getElement("table_body") |>
      dplyr::filter(row_type == "label") |>
      dplyr::pull(label),
    "Updated GRADE!"
  )
})

test_that("tbl_ard_continuous(statistic) error messaging", {
  # statistic argument is a long vector
  expect_snapshot(
    error = TRUE,
    cards::ard_continuous(trial, by = grade, variables = age) |>
      tbl_ard_continuous(variable = "age", include = "grade", statistic = everything() ~ c("{mean}", "{median}"))
  )
})

test_that("tbl_ard_continuous(value)", {
  # checking results without by variable
  expect_equal(
    cards::ard_continuous(trial, by = grade, variables = age) |>
      tbl_ard_continuous(variable = "age", include = "grade", value = grade ~ "I") |>
      as.data.frame() %>%
      `[`(1, 2),
    tbl_summary(
      trial |> dplyr::filter(grade == "I"),
      include = age,
      digits = ~1
    ) |>
      as.data.frame() %>%
      `[`(1, 2)
  )

  # checking results with by variable
  expect_equal(
    cards::ard_continuous(trial, by = c(trt, grade), variables = age) |>
      tbl_ard_continuous(by = "trt", variable = "age", include = "grade", value = grade ~ "I") |>
      as.data.frame() %>%
      `[`(1, 2),
    tbl_summary(
      trial |> dplyr::filter(grade == "I", trt == "Drug A"),
      include = age,
      digits = ~1
    ) |>
      as.data.frame() %>%
      `[`(1, 2)
  )
})


test_that("tbl_ard_continuous(value) messaging", {
  # specified a level that does not exist
  expect_snapshot(
    error = TRUE,
    cards::ard_continuous(trial, by = c(trt, grade), variables = age) |>
      tbl_ard_continuous(by = "trt", variable = "age", include = "grade", value = grade ~ "XXXXXXX")
  )

  # specified a value that is not a single unit in length
  expect_snapshot(
    error = TRUE,
    cards::ard_continuous(trial, by = c(trt, grade), variables = age) |>
      tbl_ard_continuous(by = "trt", variable = "age", include = "grade", value = grade ~ letters)
  )
})

test_that("tbl_ard_continuous() existing 'gts_column'", {
  # test there is no error when passing an ARD with an existing 'gts_column'
  tbl <-
    tbl_continuous(
      data = trial,
      variable = age,
      by = trt,
      include = grade
    )
  expect_equal(
    tbl_ard_continuous(
      cards = tbl$cards[[1]],
      variable = age,
      by = trt,
      include = grade
    ) |>
      modify_header(all_stat_cols() ~ "**{level}**  \nN = {n}") |>
      as.data.frame(),
    as.data.frame(tbl)
  )
})
