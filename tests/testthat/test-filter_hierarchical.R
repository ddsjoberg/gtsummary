skip_on_cran()

ADAE_subset <- cards::ADAE |>
  dplyr::filter(AETERM %in% unique(cards::ADAE$AETERM)[1:5])

tbl <- tbl_hierarchical(
  data = ADAE_subset,
  variables = c(SEX, RACE, AETERM),
  by = TRTA,
  denominator = cards::ADSL,
  id = USUBJID,
  overall_row = TRUE
)

test_that("filter_hierarchical() works", {
  withr::local_options(width = 200)

  # no errors
  expect_silent(tbl <- filter_hierarchical(tbl, sum(n) > 10))

  # row order is retained
  expect_snapshot(tbl |> as.data.frame())

  # check indentation of results, lines 3,4 should be indented 4 and 8 spaces
  expect_equal(
    tbl |>
      .table_styling_expr_to_row_number() |>
      getElement("table_styling") |>
      getElement("indent") |>
      tidyr::unnest(row_numbers) |>
      dplyr::filter(column == "label", row_numbers %in% c(3, 4)) |>
      dplyr::arrange(row_numbers) |>
      dplyr::pull(n_spaces),
    c(4, 8)
  )
})

test_that("filter_hierarchical(keep_empty) works", {
  tbl2 <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AEBODSYS, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID
  )

  # keep summary rows
  expect_silent(tbl_f <- filter_hierarchical(tbl2, sum(n) > 10, keep_empty = TRUE))
  expect_equal(nrow(tbl_f$table_body), 29)

  # remove summary rows
  expect_silent(tbl_f <- filter_hierarchical(tbl2, sum(n) > 10, keep_empty = FALSE))
  expect_equal(nrow(tbl_f$table_body), 16)
})

test_that("filter_hierarchical(var) works", {
  withr::local_options(width = 200)

  tbl <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(AESOC, AEDECOD, AESEV),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID
  )

  # default uses the correct variable
  expect_identical(
    filter_hierarchical(tbl, n > 5, var = AESEV),
    filter_hierarchical(tbl, n > 5)
  )

  # works with `var` specified
  expect_silent(tbl_f <- filter_hierarchical(tbl, sum(n) > 15, var = AEDECOD))
  expect_snapshot(tbl_f$table_body$label)
  expect_equal(nrow(tbl_f$table_body), 17)

  # works with first hierarchy variable
  expect_silent(tbl_f <- filter_hierarchical(tbl, sum(n) > 50, var = AESOC))
  expect_equal(nrow(tbl_f$table_body), 9)

  # works with no `by` variable
  tbl_noby <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(AESOC, AEDECOD, AESEV),
    denominator = cards::ADSL,
    id = USUBJID
  )
  expect_silent(tbl_f <- filter_hierarchical(tbl_noby, sum(n) > 10, var = AEDECOD))
  # same rows kept in table with/without `by`
  expect_identical(
    tbl_f$table_body$label,
    filter_hierarchical(tbl, sum(n) > 10, var = AEDECOD)$table_body$label
  )
  expect_equal(nrow(tbl_f$table_body), 17)
})

test_that("filter_hierarchical() works with column-specific filters", {
  tbl_o <- tbl |> add_overall()

  # difference between n's in columns 2 and 3 > 1 (one-sided)
  expect_message(tbl_f <- filter_hierarchical(tbl, n_2 - n_3 > 1))
  expect_equal(nrow(tbl_f$table_body), 6)

  # difference between n's in columns 2 and 3 > 1 (absolute)
  expect_message(tbl_f <- filter_hierarchical(tbl, abs(n_2 - n_3) > n_1))
  expect_equal(nrow(tbl_f$table_body), 15)

  # overall prevalence across row group > 30%
  expect_silent(tbl_f <- filter_hierarchical(tbl, p_overall > 0.3))
  # p_overall calculated correctly
  expect_identical(
    tbl_f,
    tbl |> filter_hierarchical(sum(n) / sum(N) > 0.3),
  )
  # p_overall filters the same rows with overall column added
  expect_identical(
    tbl_f$table_body,
    filter_hierarchical(tbl_o, p_overall > 0.3)$table_body |>
      select(-"stat_0")
  )
  expect_equal(nrow(tbl_f$table_body), 7)

  # overall prevalence across row group > 15
  expect_silent(tbl_f <- filter_hierarchical(tbl, n_overall > 15))
  expect_equal(nrow(tbl_f$table_body), 9)

  # column-wise p statistics equal to previous derivation with column names specified (both still work)
  expect_message(tbl_f <- filter_hierarchical(tbl, p_2 > 0.15 | p_3 > 0.2))
  expect_identical(
    tbl_f,
    tbl |>
      filter_hierarchical(any(p > 0.15 & TRTA == "Xanomeline High Dose") | any(p > 0.2 & TRTA == "Xanomeline Low Dose"))
  )
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
    denominator = cards::ADSL,
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
    denominator = cards::ADSL,
    id = USUBJID,
    overall_row = TRUE
  )

  expect_silent(tbl_single <- filter_hierarchical(tbl_single, sum(n) > 20))
  expect_equal(nrow(tbl_single$table_body), 4)
})

test_that("filter_hierarchical() works with no by variable", {
  tbl_noby <- tbl_hierarchical(
    data = cards::ADAE,
    denominator = cards::ADSL,
    variables = c(AEBODSYS, AEDECOD),
    id = "USUBJID"
  )

  expect_silent(tbl_f <- filter_hierarchical(tbl_noby, sum(n) / sum(N) > 0.05))
  expect_equal(nrow(tbl_f$table_body), 21)

  # check indentation of results, line 2 should be indented 4 spaces
  expect_equal(
    tbl_noby |>
      .table_styling_expr_to_row_number() |>
      getElement("table_styling") |>
      getElement("indent") |>
      tidyr::unnest(row_numbers) |>
      dplyr::filter(column == "label", row_numbers %in% 2) |>
      dplyr::arrange(row_numbers) |>
      dplyr::pull(n_spaces),
    4
  )
})

test_that("filter_hierarchical() works when some variables not included in x", {
  tbl <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
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
