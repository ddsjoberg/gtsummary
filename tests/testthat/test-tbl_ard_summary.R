skip_on_cran()

# adding a few basic tests here to ensure we don't break the function with other updates
test_that("tbl_ard_summary() works", {
  expect_snapshot(
    cards::ard_stack(
      data = cards::ADSL,
      .by = ARM,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(by = ARM) |>
      as.data.frame()
  )

  expect_snapshot(
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE,
      .total_n = TRUE
    ) |>
      tbl_ard_summary() |>
      as.data.frame()
  )
})


test_that("tbl_ard_summary(cards)", {
  # when attribute labels are not provided, we default to variable names
  expect_equal(
    cards::ard_stack(
      data = cards::ADSL,
      .by = ARM,
      cards::ard_continuous(variables = "AGE"),
      cards::ard_categorical(variables = "AGEGR1"),
      .attributes = FALSE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(by = ARM) |>
      getElement("table_body") |>
      getElement("var_label") |>
      unique(),
    c("AGE", "AGEGR1")
  )

  # no error when missing are not present AND missing='no'
  expect_snapshot(
    cards::ard_stack(
      data = cards::ADSL,
      .by = ARM,
      cards::ard_continuous(variables = "AGE"),
      .attributes = FALSE,
      .missing = FALSE
    ) |>
      tbl_ard_summary(by = ARM, missing = "no") |>
      as.data.frame()
  )

  # no error when no tablulation of the 'by' data is passed
  expect_snapshot(
    cards::ard_continuous(trial, by = trt, variables = age) |>
      tbl_ard_summary(by = trt) |>
      as.data.frame()
  )
})

test_that("tbl_ard_summary(cards) error messages", {
  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      data = cards::ADSL,
      .by = c(ARM, AGEGR1),
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(by = ARM)
  )

  # we get an error when missing ARDs are not present and missing values requested
  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      data = cards::ADSL,
      .by = ARM,
      cards::ard_continuous(variables = "AGE"),
      .attributes = FALSE,
      .missing = FALSE
    ) |>
      tbl_ard_summary(by = ARM, missing = "ifany")
  )
})

test_that("tbl_ard_summary(by) messaging", {
  expect_snapshot(
    error = TRUE,
    cards::bind_ard(
      cards::ard_continuous(trial, by = trt, variables = age),
      cards::ard_continuous(trial, by = grade, variables = age)
    ) |>
      tbl_ard_summary(by = trt)
  )

    # when ARD is stratified, but `by` arg not specified
  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      data = cards::ADSL,
      .by = ARM,
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary()
  )
})

test_that("tbl_ard_summary(label) argument works", {
  expect_equal(
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE
    ) |>
      tbl_ard_summary(label = AGE ~ "Updated AGE!") |>
      getElement("table_body") |>
      dplyr::filter(row_type == "label") |>
      dplyr::pull(label),
    c("Pooled Age Group 1", "Updated AGE!")
  )

  expect_equal(
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
      .attributes = FALSE
    ) |>
      tbl_ard_summary(label = AGE ~ "Updated AGE!") |>
      getElement("table_body") |>
      dplyr::filter(row_type == "label") |>
      dplyr::pull(label),
    c("AGEGR1", "Updated AGE!")
  )
})

test_that("tbl_ard_summary(statistic) argument works", {
  ard <-
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE,
      .total_n = TRUE
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


test_that("tbl_ard_summary(type) error messages", {
  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      data = cards::ADSL,
      .by = ARM,
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(by = ARM, type = list(AGE = "categorical"))
  )

  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      data = cards::ADSL,
      .by = ARM,
      cards::ard_categorical(variables = "AGEGR1"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(by = ARM, type = list(AGEGR1 = "continuous"))
  )
})

test_that("tbl_ard_summary(statistic) error messages", {
  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      data = cards::ADSL,
      .by = ARM,
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(by = ARM, statistic = list(AGE = "{not_a_valid_summary_statistic}"))
  )

  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      data = cards::ADSL,
      .by = ARM,
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary(by = ARM, statistic = list(AGE = c("{mean}", "{median}")))
  )
})
