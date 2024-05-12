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
