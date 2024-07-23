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


test_that("tbl_ard_continuous(statistic) error messaging", {
  # statistic argument is a long vector
  expect_snapshot(
    error = TRUE,
    cards::ard_continuous(trial, by = grade, variables = age) |>
      tbl_ard_continuous(variable = "age", include = "grade", statistic = everything() ~ c("{mean}", "{median}"))
  )
})
