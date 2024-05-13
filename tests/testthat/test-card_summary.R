# adding a few basic tests here to ensure we don't break the function with other updates
test_that("card_summary() works", {
  withr::local_package(package = "cards") # TODO: We can delete this after ard_stack() works when cards not loaded

  expect_snapshot(
    ard_stack(
      data = ADSL,
      by = ARM,
      ard_categorical(variables = "AGEGR1"),
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      card_summary() |>
      as.data.frame()
  )

  expect_snapshot(
    ard_stack(
      data = ADSL,
      by = NULL,
      ard_categorical(variables = "AGEGR1"),
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      card_summary() |>
      as.data.frame()
  )
})

test_that("card_summary(cards) error messages", {
  withr::local_package(package = "cards") # TODO: We can delete this after ard_stack() works when cards not loaded

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      by = c(ARM, AGEGR1),
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      card_summary()
  )

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      by = ARM,
      ard_continuous(variables = "AGE"),
      .attributes = FALSE,
      .missing = TRUE
    ) |>
      card_summary()
  )
})

test_that("card_summary(type) error messages", {
  withr::local_package(package = "cards") # TODO: We can delete this after ard_stack() works when cards not loaded

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      by = ARM,
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      card_summary(type = list(AGE = "categorical"))
  )

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      by = ARM,
      ard_categorical(variables = "AGEGR1"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      card_summary(type = list(AGEGR1 = "continuous"))
  )
})

test_that("card_summary(statistic) error messages", {
  withr::local_package(package = "cards") # TODO: We can delete this after ard_stack() works when cards not loaded

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      by = ARM,
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      card_summary(statistic = list(AGE = "{not_a_valid_summary_statistic}"))
  )
})
