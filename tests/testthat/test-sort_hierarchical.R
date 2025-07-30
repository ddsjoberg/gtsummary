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

test_that("sort_hierarchical() works", {
  withr::local_options(width = 250)

  # no errors
  expect_silent(tbl <- sort_hierarchical(tbl))
  expect_snapshot(tbl |> as.data.frame())
})

test_that("sort_hierarchical(sort = 'descending') works", {
  # descending frequency (default)
  expect_silent(tbl <- sort_hierarchical(tbl))
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

test_that("sort_hierarchical(sort = 'alphanumeric') works", {
  expect_silent(result <- sort_hierarchical(tbl))

  # ascending (A to Z)
  expect_silent(result <- sort_hierarchical(result, sort = "alphanumeric"))

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

test_that("sort_hierarchical(sort) works with different sorting methods for each variable", {
  expect_silent(
    tbl <- sort_hierarchical(
      tbl,
      sort = list(SEX ~ "alphanumeric", RACE = "descending", AETERM = "alphanumeric")
    )
  )

  # results are ordered correctly
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
      "APPLICATION SITE ERYTHEMA", "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "APPLICATION SITE PRURITUS",
      "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "DIARRHOEA", "ERYTHEMA", "APPLICATION SITE ERYTHEMA",
      "APPLICATION SITE PRURITUS", "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "DIARRHOEA", "ERYTHEMA",
      "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "ERYTHEMA"
    )
  )
})

test_that("sort_hierarchical() works when there is no overall row in x", {
  tbl_no_overall <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
    overall_row = FALSE
  )

  # sort = 'descending'
  expect_silent(tbl_no_overall <- sort_hierarchical(tbl_no_overall))
  expect_equal(
    tbl_no_overall$table_body,
    sort_hierarchical(tbl)$table_body[-1, ]
  )

  # sort = 'alphanumeric'
  expect_silent(tbl_no_overall <- sort_hierarchical(tbl_no_overall, sort = "alphanumeric"))
  expect_equal(
    tbl_no_overall$table_body,
    sort_hierarchical(tbl, sort = "alphanumeric")$table_body[-1, ]
  )
})

test_that("sort_hierarchical() works with only one variable in x", {
  tbl_single <- tbl_hierarchical(
    data = ADAE_subset,
    variables = AETERM,
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
    overall_row = TRUE
  )

  # sort = 'frequency'
  expect_silent(tbl_single <- sort_hierarchical(tbl_single))
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
  expect_silent(tbl_single <- sort_hierarchical(tbl_single, sort = "alphanumeric"))
  expect_equal(
    tbl_single$table_body |>
      dplyr::filter(variable == "AETERM") |>
      dplyr::pull(label),
    sort(unique(ADAE_subset$AETERM))
  )
})

test_that("sort_hierarchical() works when some variables not included in x", {
  tbl <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
    include = c(SEX, AETERM),
    overall_row = TRUE
  )

  expect_message(sort_hierarchical(tbl))
})

test_that("sort_hierarchical() works with no by variable", {
  tbl <- tbl_hierarchical(
    data = ADAE_subset,
    denominator = cards::ADSL,
    variables = c(AEBODSYS, AEDECOD),
    id = "USUBJID",
    overall_row = TRUE
  )

  expect_silent(tbl_sort <- sort_hierarchical(tbl))
  expect_equal(nrow(tbl_sort$table_body), nrow(tbl$table_body))
})

test_that("sort_hierarchical() works with add_overall()", {
  tbl_s <- sort_hierarchical(tbl)
  tbl_o <- tbl |> add_overall()

  expect_silent(tbl_o <- sort_hierarchical(tbl_o))

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

test_that("sort_hierarchical() error messaging works", {
  # invalid x input
  expect_snapshot(
    sort_hierarchical(data.frame()),
    error = TRUE
  )

  # invalid sort input
  expect_snapshot(
    sort_hierarchical(tbl, "10"),
    error = TRUE
  )
})
