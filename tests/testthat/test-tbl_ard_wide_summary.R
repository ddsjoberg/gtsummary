skip_on_cran()

test_that("tbl_ard_wide_summary(cards)", {
  # works with standard use
  expect_snapshot(
    cards::ard_stack(
      trial,
      cards::ard_dichotomous(variables = response),
      cards::ard_categorical(variables = grade),
      .missing = TRUE,
      .attributes = TRUE
    ) |>
      tbl_ard_wide_summary() |>
      as.data.frame()
  )
})

test_that("tbl_ard_wide_summary(cards) messaging", {
  # error when there are grouping variables
  expect_snapshot(
    error = TRUE,
    cards::ard_continuous(trial, by = trt, variables = age) |>
      tbl_ard_wide_summary()
  )
})


test_that("tbl_ard_wide_summary(type) messaging", {
  # error when there are grouping variables
  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      trial,
      cards::ard_continuous(variables = age),
      .missing = TRUE,
      .attributes = TRUE
    ) |>
      tbl_ard_wide_summary(type = age ~ "categorical")
  )

  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      trial,
      cards::ard_dichotomous(variables = response),
      cards::ard_continuous(variables = age),
      .missing = TRUE,
      .attributes = TRUE
    ) |>
      tbl_ard_wide_summary()
  )
})
