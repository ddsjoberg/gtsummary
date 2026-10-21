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
  withr::local_options(width = 250)

  # 3 variables, 2 in include
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

  # 3 variables, 1 in include
  tbl <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(AESOC, AETERM, AESEV),
    include = AESEV,
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
    overall_row = TRUE
  )

  expect_snapshot(sort_hierarchical(tbl) |> as.data.frame())
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

  # overall col with different sort variables does not affect sort order
  expect_silent(
    tbl_s <- sort_hierarchical(
      tbl,
      sort = list(SEX ~ "alphanumeric", RACE = "descending", AETERM = "alphanumeric")
    )
  )
  tbl_o <- tbl |> add_overall()
  expect_silent(
    tbl_o <- sort_hierarchical(
      tbl_o,
      sort = list(SEX ~ "alphanumeric", RACE = "descending", AETERM = "alphanumeric")
    )
  )
  expect_identical(tbl_o$table_body$label, tbl_s$table_body$label)
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

test_that("sort_hierarchical(by_level=) restricts descending sort to one by level", {
  withr::local_options(width = 250)

  expect_silent(tbl_by <- sort_hierarchical(tbl, by_level = "Placebo"))
  expect_snapshot(tbl_by |> as.data.frame())

  # within each section, rows are ordered by descending count in the "Placebo" arm only
  # (e.g. under M/WHITE the Placebo counts run 6, 3, 2, 1, 1), so the order differs from the
  # all-arms default sort
  expect_equal(
    tbl_by$table_body |>
      dplyr::filter(variable == "SEX") |>
      dplyr::pull(label),
    c("F", "M")
  )
  expect_equal(
    tbl_by$table_body |>
      dplyr::filter(variable == "RACE") |>
      dplyr::pull(label),
    c("WHITE", "BLACK OR AFRICAN AMERICAN", "WHITE", "BLACK OR AFRICAN AMERICAN",
      "AMERICAN INDIAN OR ALASKA NATIVE")
  )
  expect_equal(
    tbl_by$table_body |>
      dplyr::filter(variable == "AETERM") |>
      dplyr::pull(label),
    c(
      "ERYTHEMA", "APPLICATION SITE ERYTHEMA", "APPLICATION SITE PRURITUS", "DIARRHOEA",
      "APPLICATION SITE PRURITUS", "DIARRHOEA", "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "ERYTHEMA",
      "DIARRHOEA", "ERYTHEMA", "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "APPLICATION SITE ERYTHEMA",
      "APPLICATION SITE PRURITUS", "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "ERYTHEMA"
    )
  )

  # the "Placebo"-restricted order differs from the all-arms default descending sort
  expect_false(
    identical(tbl_by$table_body$label, sort_hierarchical(tbl)$table_body$label)
  )
})

test_that("sort_hierarchical(by_level=) has no effect on alphanumeric sorting", {
  expect_equal(
    sort_hierarchical(tbl, sort = "alphanumeric", by_level = "Placebo")$table_body$label,
    sort_hierarchical(tbl, sort = "alphanumeric")$table_body$label
  )
})

test_that("sort_hierarchical(by_level=) works with variables not in include", {
  withr::local_options(width = 250)

  tbl_ni <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
    include = c(SEX, AETERM), # RACE not included -> dummy rows exercised
    overall_row = TRUE
  )

  # still emits the estimated-rates message, and does not error
  expect_message(res <- sort_hierarchical(tbl_ni, by_level = "Placebo"))
  expect_snapshot(res |> as.data.frame())
})

test_that("sort_hierarchical(by_level=) works with add_overall()", {
  tbl_o <- tbl |> add_overall()
  expect_silent(tbl_o <- sort_hierarchical(tbl_o, by_level = "Placebo"))

  # display order is driven by the Placebo-restricted main sort
  expect_identical(
    tbl_o$table_body$label,
    sort_hierarchical(tbl, by_level = "Placebo")$table_body$label
  )
})

test_that("sort_hierarchical(by_level=) error messaging works", {
  # scalar interface rejects the cards-style named list
  expect_snapshot(
    sort_hierarchical(tbl, by_level = list(TRTA = "Placebo")),
    error = TRUE
  )

  # non-scalar
  expect_snapshot(
    sort_hierarchical(tbl, by_level = c("Placebo", "Xanomeline Low Dose")),
    error = TRUE
  )

  # invalid level (delegated to cards)
  expect_snapshot(
    sort_hierarchical(tbl, by_level = "Nope"),
    error = TRUE
  )

  # no by variable
  tbl_no_by <- tbl_hierarchical(
    data = ADAE_subset,
    denominator = cards::ADSL,
    variables = c(AEBODSYS, AEDECOD),
    id = "USUBJID",
    overall_row = TRUE
  )
  expect_snapshot(
    sort_hierarchical(tbl_no_by, by_level = "Placebo"),
    error = TRUE
  )
})

test_that("sort_hierarchical() retains the internal ard_stack_hierarchical class", {
  ADAE_subset <- cards::ADAE |>
    dplyr::filter(AEBODSYS %in% c("SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                                  "EAR AND LABYRINTH DISORDERS")) |>
    dplyr::filter(.by = AEBODSYS, dplyr::row_number() < 20)
  
  tbl <- tbl_hierarchical(
    data = ADAE_subset,
    variables = c(AEBODSYS, AEDECOD),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID
  )
  
  tbl_sorted <- sort_hierarchical(tbl)
  
  # Verify the internal ARD engine kept its required cards subclass
  expect_s3_class(
    tbl_sorted$cards$tbl_hierarchical, 
    "ard_stack_hierarchical"
  )
})