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

test_that("filter_hierarchical() works", {
  withr::local_options(width = 200)

  # no errors
  expect_silent(tbl <- filter_hierarchical(tbl, sum(n) > 10))

  # row order is retained
  expect_snapshot(tbl |> as.data.frame())
})

test_that("filter_hierarchical(keep_empty) works", {
  tbl2 <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AEBODSYS, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID
  )

  # keep summary rows
  expect_silent(tbl_f <- filter_hierarchical(tbl2, sum(n) > 10, keep_empty = TRUE))
  expect_equal(nrow(tbl_f$table_body), 29)

  # remove summary rows
  expect_silent(tbl_f <- filter_hierarchical(tbl2, sum(n) > 10, keep_empty = FALSE))
  expect_equal(nrow(tbl_f$table_body), 16)
})

test_that("filter_hierarchical() works with various different filter conditions", {
  withr::local_options(width = 200)

  expect_silent(tbl_gt <- filter_hierarchical(tbl, sum(n) > 10))
  expect_silent(tbl_lt <- filter_hierarchical(tbl, sum(n) <= 10))
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

  expect_silent(tbl_f <- filter_hierarchical(tbl, n > 5))
  expect_equal(nrow(tbl_f$table_body), 11)

  expect_silent(tbl_f <- filter_hierarchical(tbl, p > 0.05))
  expect_equal(nrow(tbl_f$table_body), 25)

  expect_silent(tbl_f <- filter_hierarchical(tbl, n == 2 & p < 0.05))
  expect_equal(nrow(tbl_f$table_body), 6)

  expect_silent(tbl_f <- filter_hierarchical(tbl, mean(n) > 4 | n > 3))
  expect_equal(nrow(tbl_f$table_body), 12)

  expect_silent(tbl_f <- filter_hierarchical(tbl, any(n > 2 & TRTA == "Xanomeline High Dose"), keep_empty = FALSE))
  expect_snapshot(tbl_f |> as.data.frame())
})

test_that("filter_hierarchical() returns empty table when all rows filtered out", {
  tbl2 <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID
  )

  expect_silent(tbl_f <- filter_hierarchical(tbl2, sum(n) > 200, keep_empty = FALSE))
  expect_equal(nrow(tbl_f$table_body), 0)

  # overall row present
  expect_silent(tbl_f <- filter_hierarchical(tbl, sum(n) > 200, keep_empty = FALSE))
  expect_equal(nrow(tbl_f$table_body), 1)
})

test_that("filter_hierarchical() works with only one variable in x", {
  tbl_single <- tbl_hierarchical(
    data = ADAE_subset,
    variables = AETERM,
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID,
    overall_row = TRUE
  )

  expect_silent(tbl_single <- filter_hierarchical(tbl_single, sum(n) > 20))
  expect_equal(nrow(tbl_single$table_body), 4)
})

test_that("filter_hierarchical() works with no by variable", {
  tbl_noby <- tbl_hierarchical(
    data = cards::ADAE,
    denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
    variables = c(AEBODSYS, AEDECOD),
    id = "USUBJID"
  )

  expect_silent(tbl_f <- filter_hierarchical(tbl_noby, sum(n) / sum(N) > 0.05))
  expect_equal(nrow(tbl_f$table_body), 21)
})

test_that("filter_hierarchical() works when some variables not included in x", {
  tbl <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID,
    include = c(SEX, AETERM),
    overall_row = TRUE
  )

  expect_message(filter_hierarchical(tbl, sum(n) > 10))
})

test_that("filter_hierarchical() works with add_overall()", {
  tbl_f <- filter_hierarchical(tbl, n > 1)
  tbl_o <- tbl |> add_overall()

  expect_silent(tbl_o <- filter_hierarchical(tbl_o, n > 1))

  # overall col does not affect rows filtered
  expect_identical(tbl_o$table_body$label, tbl_f$table_body$label)

  # cards$add_overall is filtered correctly
  expect_equal(nrow(tbl_o$cards$add_overall), 49)
})

test_that("filter_hierarchical() error messaging works", {
  # invalid x input
  expect_snapshot(
    filter_hierarchical(data.frame(), sum(n) > 10),
    error = TRUE
  )

  # invalid filter input
  expect_snapshot(
    filter_hierarchical(tbl, 10),
    error = TRUE
  )
})
