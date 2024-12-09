skip_on_cran()

ADAE_subset <- cards::ADAE |>
  dplyr::filter(AETERM %in% unique(cards::ADAE$AETERM)[1:5])

tbl <- tbl_hierarchical(
  data = ADAE_subset,
  variables = c(SEX, RACE, AETERM),
  by = TRTA,
  denominator = cards::ADSL |> mutate(TRTA = ARM),
  id = USUBJID,
  overall_row = TRUE
)

test_that("tbl_filter.tbl_hierarchical() works", {
  withr::local_options(width = 200)

  # no errors
  expect_silent(tbl <- tbl_filter(tbl, t = 10))
  expect_snapshot(tbl |> as.data.frame())

  # .stat argument works
  expect_silent(tbl <- tbl_filter(tbl, t = 10, .stat = "p"))
})

test_that("tbl_filter.tbl_hierarchical(gt) works", {
  # gt = TRUE
  expect_silent(tbl_gt <- tbl_filter(tbl, t = 10))

  # gt = FALSE
  expect_silent(tbl_lt <- tbl_filter(tbl, t = 10, gt = FALSE))

  expect_equal(
    dplyr::inner_join(
      tbl_gt$table_body,
      tbl_lt$table_body,
      by = names(tbl_gt$table_body)
    ) |>
      dplyr::filter(variable == "AETERM") |>
      nrow(),
    0
  )

  expect_equal(
    sum(
      tbl_gt$table_body |>
        dplyr::filter(variable == "AETERM") |>
        nrow(),
      tbl_lt$table_body |>
        dplyr::filter(variable == "AETERM") |>
        nrow()
    ),
    tbl$table_body |>
      dplyr::filter(variable == "AETERM") |>
      nrow()
  )
})

test_that("tbl_filter.tbl_hierarchical(eq) works", {
  # gt = TRUE, eq = FALSE
  expect_silent(tbl_gt <- tbl_filter(tbl, t = 12))

  # gt = TRUE, eq = TRUE
  expect_silent(tbl_geq <- tbl_filter(tbl, t = 12, eq = TRUE))
  expect_gt(nrow(tbl_geq$table_body), nrow(tbl_gt$table_body))

  # gt = FALSE, eq = FALSE
  expect_silent(tbl_lt <- tbl_filter(tbl, t = 12, gt = FALSE))

  # gt = TRUE, eq = TRUE
  expect_silent(tbl_leq <- tbl_filter(tbl, t = 12, gt = FALSE, eq = TRUE))
  expect_lt(nrow(tbl_lt$table_body), nrow(tbl_leq$table_body))
})

test_that("tbl_filter.tbl_hierarchical() returns empty table when all rows filtered out", {
  expect_silent(tbl <- tbl_filter(tbl, t = 200))
  expect_equal(nrow(tbl$table_body), 0)
})

test_that("tbl_filter.tbl_hierarchical() works with only one variable in x", {
  tbl_single <- tbl_hierarchical(
    data = ADAE_subset,
    variables = AETERM,
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID,
    overall_row = TRUE
  )

  expect_silent(tbl_single <- tbl_filter(tbl_single, t = 20))
  expect_equal(nrow(tbl_single$table_body), 4)
})

test_that("tbl_filter.tbl_hierarchical() works when some variables not included in x", {
  tbl <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID,
    include = c(SEX, AETERM),
    overall_row = TRUE
  )

  expect_message(tbl_filter(tbl, t = 10))
})

test_that("tbl_filter.tbl_hierarchical() error messaging works", {
  # invalid x input
  expect_snapshot(
    tbl_filter(data.frame(), t = 10),
    error = TRUE
  )

  # invalid t input
  expect_snapshot(
    tbl_filter(tbl, t = "10"),
    error = TRUE
  )

  # invalid gt input
  expect_snapshot(
    tbl_filter(tbl, t = "10", gt = "yes"),
    error = TRUE
  )

  # invalid eq input
  expect_snapshot(
    tbl_filter(tbl, t = "10", eq = "no"),
    error = TRUE
  )

  # invalid .stat input
  expect_snapshot(
    tbl_filter(tbl, t = "10", .stat = "pct"),
    error = TRUE
  )
})
