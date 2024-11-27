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
      denominator = cards::ADSL |> mutate(TRTA = ARM),
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
      denominator = cards::ADSL |> mutate(TRTA = ARM),
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
      denominator = cards::ADSL |> mutate(TRTA = ARM)
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
      denominator = cards::ADSL |> mutate(TRTA = ARM)
    ) |>
      as.data.frame(col_labels = FALSE)
  )
})
