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

test_that("tbl_sort.tbl_hierarchical() works", {
  withr::local_options(width = 200)

  # no errors
  expect_silent(tbl <- tbl_sort(tbl))
  expect_snapshot(tbl |> as.data.frame())

  # .stat argument works
  expect_silent(tbl <- tbl_sort(tbl, .stat = "p"))
})

test_that("tbl_sort.tbl_hierarchical(sort = 'descending') works", {
  # descending frequency (default)
  expect_silent(tbl <- tbl_sort(tbl))
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "SEX") |>
      dplyr::pull(label),
    c("F", "M")
  )
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "RACE") |>
      dplyr::pull(label),
    c("WHITE", "BLACK OR AFRICAN AMERICAN", "WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE")
  )
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "AETERM") |>
      dplyr::pull(label),
    c(
      "APPLICATION SITE PRURITUS", "ERYTHEMA", "APPLICATION SITE ERYTHEMA", "DIARRHOEA", "APPLICATION SITE PRURITUS",
      "ERYTHEMA", "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "DIARRHOEA", "APPLICATION SITE PRURITUS",
      "APPLICATION SITE ERYTHEMA", "ERYTHEMA", "DIARRHOEA", "ATRIOVENTRICULAR BLOCK SECOND DEGREE",
      "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "ERYTHEMA"
    )
  )
})

test_that("tbl_sort.tbl_hierarchical(sort = 'alphanumeric') works", {
  expect_silent(result <- tbl_sort(tbl))

  # ascending (A to Z)
  expect_silent(result <- tbl_sort(result, sort = "alphanumeric"))

  # results are ordered correctly
  expect_equal(
    as.data.frame(result)[[1]],
    c(
      "Number of patients with event", "F", "BLACK OR AFRICAN AMERICAN", "APPLICATION SITE PRURITUS",
      "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "DIARRHOEA", "ERYTHEMA", "WHITE", "APPLICATION SITE ERYTHEMA",
      "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "M", "AMERICAN INDIAN OR ALASKA NATIVE", "ERYTHEMA",
      "BLACK OR AFRICAN AMERICAN", "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "WHITE",
      "APPLICATION SITE ERYTHEMA", "APPLICATION SITE PRURITUS", "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "DIARRHOEA",
      "ERYTHEMA"
    )
  )
})

test_that("tbl_sort.tbl_hierarchical() works when there is no overall row in x", {
  tbl_no_overall <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID,
    overall_row = FALSE
  )

  # sort = 'descending'
  expect_silent(tbl_no_overall <- tbl_sort(tbl_no_overall))
  expect_equal(
    tbl_no_overall$table_body,
    tbl_sort(tbl)$table_body[-1, ]
  )

  # sort = 'alphanumeric'
  expect_silent(tbl_no_overall <- tbl_sort(tbl_no_overall, sort = "alphanumeric"))
  expect_equal(
    tbl_no_overall$table_body,
    tbl_sort(tbl, sort = "alphanumeric")$table_body[-1, ]
  )
})

test_that("tbl_sort.tbl_hierarchical() works with only one variable in x", {
  tbl_single <- tbl_hierarchical(
    data = ADAE_subset,
    variables = AETERM,
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID,
    overall_row = TRUE
  )

  # sort = 'frequency'
  expect_silent(tbl_single <- tbl_sort(tbl_single))
  expect_equal(
    tbl_single$table_body |>
      dplyr::filter(variable == "AETERM") |>
      dplyr::pull(label),
    c(
      "APPLICATION SITE PRURITUS", "ERYTHEMA", "APPLICATION SITE ERYTHEMA", "DIARRHOEA",
      "ATRIOVENTRICULAR BLOCK SECOND DEGREE"
    )
  )

  # sort = 'alphanumeric'
  expect_silent(tbl_single <- tbl_sort(tbl_single, sort = "alphanumeric"))
  expect_equal(
    tbl_single$table_body |>
      dplyr::filter(variable == "AETERM") |>
      dplyr::pull(label),
    sort(unique(ADAE_subset$AETERM))
  )
})

test_that("tbl_sort.tbl_hierarchical() works when some variables not included in x", {
  tbl <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID,
    include = c(SEX, AETERM),
    overall_row = TRUE
  )

  expect_message(tbl_sort(tbl))
})

test_that("tbl_sort.tbl_hierarchical() works with add_overall()", {
  tbl_s <- tbl_sort(tbl)
  tbl_o <- tbl |> add_overall()

  expect_silent(tbl_o <- tbl_sort(tbl_o))

  # overall col does not affect sort order
  expect_identical(tbl_o$table_body$label, tbl_s$table_body$label)

  # cards$add_overall is sorted correctly
  expect_equal(
    tbl_o$cards$add_overall |>
      dplyr::filter(variable == "SEX", stat_name == "n") |>
      dplyr::pull(variable_level) |>
      unlist(),
    c("F", "M")
  )
  expect_equal(
    tbl_o$cards$add_overall |>
      dplyr::filter(variable == "RACE", stat_name == "n") |>
      dplyr::pull(variable_level) |>
      unlist(),
    c("WHITE", "BLACK OR AFRICAN AMERICAN", "WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE")
  )
  expect_equal(
    tbl_o$cards$add_overall |>
      dplyr::filter(variable == "AETERM", stat_name == "n") |>
      dplyr::pull(variable_level) |>
      unlist() |>
      as.character(),
    c(
      "APPLICATION SITE PRURITUS", "ERYTHEMA", "APPLICATION SITE ERYTHEMA", "DIARRHOEA", "APPLICATION SITE PRURITUS",
      "ERYTHEMA", "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "DIARRHOEA", "APPLICATION SITE PRURITUS",
      "APPLICATION SITE ERYTHEMA", "ERYTHEMA", "DIARRHOEA", "ATRIOVENTRICULAR BLOCK SECOND DEGREE",
      "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "ERYTHEMA"
    )
  )
})

test_that("tbl_sort.tbl_hierarchical() error messaging works", {
  # invalid x input
  expect_snapshot(
    tbl_sort(data.frame()),
    error = TRUE
  )

  # invalid sort input
  expect_snapshot(
    tbl_sort(tbl, 10),
    error = TRUE
  )
})
