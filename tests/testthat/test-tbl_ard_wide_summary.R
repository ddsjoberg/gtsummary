skip_on_cran()

test_that("tbl_ard_wide_summary(cards)", {
  # works with standard use
  expect_snapshot(
    cards::ard_stack(
      trial,
      cards::ard_tabulate_value(variables = response),
      cards::ard_tabulate(variables = grade),
      .missing = TRUE,
      .attributes = TRUE
    ) |>
      tbl_ard_wide_summary() |>
      as.data.frame()
  )

  # works without missing or attributes
  expect_snapshot(
    cards::ard_stack(
      trial,
      cards::ard_tabulate_value(variables = response),
      cards::ard_tabulate(variables = grade)
    ) |>
      tbl_ard_wide_summary() |>
      as.data.frame()
  )
})

test_that("tbl_ard_wide_summary(cards) messaging", {
  # error when there are grouping variables
  expect_snapshot(
    error = TRUE,
    cards::ard_summary(trial, by = trt, variables = age) |>
      tbl_ard_wide_summary()
  )
})


test_that("tbl_ard_wide_summary(type) messaging", {
  # error when there are grouping variables
  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      trial,
      cards::ard_summary(variables = age),
      .missing = TRUE,
      .attributes = TRUE
    ) |>
      tbl_ard_wide_summary(type = age ~ "categorical")
  )

  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      trial,
      cards::ard_tabulate_value(variables = response),
      cards::ard_summary(variables = age),
      .missing = TRUE,
      .attributes = TRUE
    ) |>
      tbl_ard_wide_summary()
  )
})

test_that("tbl_ard_summary(label) argument works", {
  expect_equal(
    cards::ard_stack(
      trial,
      cards::ard_summary(variables = age),
      .missing = TRUE,
      .attributes = TRUE,
      .total_n = TRUE
    ) |>
      tbl_ard_wide_summary(label = age ~ "Updated AGE!") |>
      getElement("table_body") |>
      dplyr::filter(row_type == "label") |>
      dplyr::pull(label),
    "Updated AGE!"
  )

  expect_equal(
    cards::ard_stack(
      trial,
      cards::ard_summary(variables = age),
      .attributes = FALSE
    ) |>
      tbl_ard_wide_summary(label = age ~ "Updated AGE!") |>
      getElement("table_body") |>
      dplyr::filter(row_type == "label") |>
      dplyr::pull(label),
    "Updated AGE!"
  )
})

test_that("tbl_ard_wide_summary() existing 'gts_column'", {
  # test there is no error when passing an ARD with an existing 'gts_column'
  tbl <-
    trial |>
    tbl_wide_summary(include = c(response, grade))
  tbl_ard <-
    tbl_ard_wide_summary(
      cards = tbl$cards[[1]],
      include = c(response, grade)
    )
  expect_true("gts_column" %in% names(tbl$cards[[1]]))
  expect_equal(
    as.data.frame(tbl_ard),
    as.data.frame(tbl)
  )

  # the call is saved
  expect_equal(names(tbl_ard$call_list), "tbl_ard_wide_summary")
})

