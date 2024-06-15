# adding a few basic tests here to ensure we don't break the function with other updates
test_that("tbl_ard_summary() works", {
  withr::local_package(package = "cards") # TODO: We can delete this after ard_stack() works when cards not loaded

  expect_snapshot(
    ard_stack(
      data = ADSL,
      .by = ARM,
      ard_categorical(variables = "AGEGR1"),
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary() |>
      as.data.frame()
  )

  expect_snapshot(
    ard_stack(
      data = ADSL,
      ard_categorical(variables = "AGEGR1"),
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary() |>
      as.data.frame()
  )
})

test_that("tbl_ard_summary(statistic) argument works", {
  withr::local_package(package = "cards") # TODO: We can delete this after ard_stack() works when cards not loaded

  ard <-
    ard_stack(
      data = ADSL,
      ard_categorical(variables = "AGEGR1"),
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    )

  expect_snapshot(
    tbl_ard_summary(
      ard,
      statistic = list(all_continuous() ~ "{median}", all_categorical() ~ "{n} / {N} (Total {N_obs})")
    ) |>
      as.data.frame()
  )

  expect_snapshot(
    tbl_ard_summary(
      ard,
      type = list(all_continuous() ~ "continuous2"),
      statistic = list(all_continuous() ~ c("{median}", "{mean}"))
    ) |>
      as.data.frame()
  )
})


test_that("tbl_ard_summary(cards) error messages", {
  withr::local_package(package = "cards") # TODO: We can delete this after ard_stack() works when cards not loaded

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      .by = c(ARM, AGEGR1),
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary()
  )

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      .by = ARM,
      ard_continuous(variables = "AGE"),
      .attributes = FALSE,
      .missing = TRUE
    ) |>
      tbl_ard_summary()
  )
})

test_that("tbl_ard_summary(type) error messages", {
  withr::local_package(package = "cards") # TODO: We can delete this after ard_stack() works when cards not loaded

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      .by = ARM,
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(type = list(AGE = "categorical"))
  )

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      .by = ARM,
      ard_categorical(variables = "AGEGR1"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(type = list(AGEGR1 = "continuous"))
  )
})

test_that("tbl_ard_summary(statistic) error messages", {
  withr::local_package(package = "cards") # TODO: We can delete this after ard_stack() works when cards not loaded

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      .by = ARM,
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(statistic = list(AGE = "{not_a_valid_summary_statistic}"))
  )

  expect_snapshot(
    error = TRUE,
    ard_stack(
      data = ADSL,
      .by = ARM,
      ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(statistic = list(AGE = c("{mean}", "{median}")))
  )
})
