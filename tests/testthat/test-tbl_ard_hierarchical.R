skip_on_cran()

ADAE_subset <- cards::ADAE |>
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:5],
    AETERM %in% unique(cards::ADAE$AETERM)[1:5]
  )

test_that("tbl_ard_hierarchical() event rates", {
  ard <-
    cards::ard_stack_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AETERM, AESEV),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID
    )

  expect_error(
    tbl <- tbl_ard_hierarchical(
      cards = ard,
      variables = c(AESOC, AETERM, AESEV),
      by = TRTA,
      label = list(AESOC = "System Organ Class",
                   AETERM = "Preferred Term",
                   AESEV = "Severity")
    ),
    NA
  )
  expect_equal(
    tbl$table_styling$header |> dplyr::filter(column == "label") |> dplyr::pull("label") |> gsub("[^[:alnum:]]", "", x = _),
    "SystemOrganClassPreferredTermSeverity"
  )

  expect_equal(
    as.data.frame(tbl, col_labels = FALSE),
    tbl_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AETERM, AESEV),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID,
      digits = ~list(p = 1L)
    ) |>
      as.data.frame(col_labels = FALSE)
  )
})

test_that("tbl_ard_hierarchical() counts", {
  ard <-
    cards::ard_stack_hierarchical_count(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL
    )

  expect_error(
    tbl <- tbl_ard_hierarchical(
      cards = ard,
      variables = c(AESOC, AETERM),
      by = TRTA,
      statistic = ~"{n}"
    ),
    NA
  )

  expect_equal(
    as.data.frame(tbl, col_labels = FALSE),
    tbl_hierarchical_count(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL
    ) |>
      as.data.frame(col_labels = FALSE)
  )
})

test_that("tbl_ard_hierarchical() preserves ARD sorting", {
  ard <-
    cards::ard_stack_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      id = USUBJID,
      denominator = cards::ADSL
    ) |>
    cards::sort_ard_hierarchical("descending")

  expect_silent(
    tbl <- tbl_ard_hierarchical(
      cards = ard,
      variables = c(AESOC, AETERM),
      by = TRTA
    )
  )

  expect_equal(
    tbl$table_body |>
      select("variable", "label"),
    ard |>
      dplyr::filter(variable != "TRTA") |>
      cards::unlist_ard_columns() |>
      select(cards::all_ard_variables()) |>
      dplyr::rename(label = variable_level) |>
      dplyr::distinct(),
    ignore_attr = TRUE
  )
})

test_that("tbl_ard_hierarchical() works correctly with non-standard hierarchical ARD input", {
  # build ARD
  ard <- cards::bind_ard(
    cards::ADSL |> cards::ard_tabulate(by = ARM, variable = SEX),
    cards::ADSL |> cards::ard_tabulate(by = ARM, strata = SEX, variable = RACE),
    cards::ADSL |> cards::ard_tabulate(variables = ARM)
  )

  expect_silent(
    tbl <- tbl_ard_hierarchical(
      cards = ard,
      by = ARM,
      variables = c(SEX, RACE)
    )
  )

  race_alphanum <- c("AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN", "WHITE")
  expect_equal(
    tbl$table_body |> dplyr::pull("label"),
    c("F", race_alphanum, "M", race_alphanum)
  )
})
