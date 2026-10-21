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

test_that("tbl_ard_hierarchical(overall_row) works with event rates", {
  ard <-
    cards::ard_stack_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID,
      over_variables = TRUE,
      attributes = TRUE
    )

  expect_silent(
    tbl <- tbl_ard_hierarchical(
      cards = ard,
      variables = c(AESOC, AETERM),
      by = TRTA,
      overall_row = TRUE,
      label = list(..ard_hierarchical_overall.. = "Any AE")
    )
  )
  expect_equal(tbl$table_body$label[1], "Any AE")

  expect_equal(
    as.data.frame(tbl),
    tbl_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID,
      overall_row = TRUE,
      label = list(..ard_hierarchical_overall.. = "Any AE"),
      digits = ~list(p = 1L)
    ) |>
      as.data.frame()
  )
})

test_that("tbl_ard_hierarchical(overall_row) works with event counts", {
  ard <-
    cards::ard_stack_hierarchical_count(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL,
      over_variables = TRUE,
      attributes = TRUE
    )

  expect_silent(
    tbl <- tbl_ard_hierarchical(
      cards = ard,
      variables = c(AESOC, AETERM),
      by = TRTA,
      statistic = ~"{n}",
      overall_row = TRUE
    )
  )
  expect_equal(tbl$table_body$label[1], "Total number of events")

  expect_equal(
    as.data.frame(tbl),
    tbl_hierarchical_count(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL,
      overall_row = TRUE
    ) |>
      as.data.frame()
  )
})

test_that("tbl_ard_hierarchical(overall_row) errors when overall results are missing", {
  ard <-
    cards::ard_stack_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID
    )

  expect_snapshot(
    tbl_ard_hierarchical(cards = ard, variables = c(AESOC, AETERM), by = TRTA, overall_row = TRUE),
    error = TRUE
  )
})

test_that("tbl_ard_hierarchical() default labels come from the ARD attributes", {
  ard <-
    cards::ard_stack_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID,
      attributes = TRUE
    )

  expect_equal(
    tbl_ard_hierarchical(cards = ard, variables = c(AESOC, AETERM), by = TRTA)$table_styling$header |>
      dplyr::filter(column == "label") |>
      dplyr::pull("label") |>
      gsub("[^[:alnum:]]", "", x = _),
    "PrimarySystemOrganClassReportedTermfortheAdverseEvent"
  )

  # user-specified labels override the attributes, and formula syntax is accepted
  expect_equal(
    tbl_ard_hierarchical(cards = ard, variables = c(AESOC, AETERM), by = TRTA, label = AESOC ~ "SOC")$table_styling$header |>
      dplyr::filter(column == "label") |>
      dplyr::pull("label") |>
      gsub("[^[:alnum:]]", "", x = _),
    "SOCReportedTermfortheAdverseEvent"
  )
})

test_that("tbl_ard_hierarchical() checks the structure of the ARD", {
  ard <-
    cards::ard_stack_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID
    )

  # `by` not specified for a stratified ARD
  expect_snapshot(
    tbl_ard_hierarchical(cards = ard, variables = c(AESOC, AETERM)),
    error = TRUE
  )

  # hierarchy variables in the wrong order
  expect_snapshot(
    tbl_ard_hierarchical(cards = ard, variables = c(AETERM, AESOC), by = TRTA),
    error = TRUE
  )

  # summary statistics are not present for a variable in `include`
  expect_snapshot(
    tbl_ard_hierarchical(
      cards = cards::ard_stack_hierarchical(
        data = ADAE_subset,
        variables = c(AESOC, AETERM),
        by = TRTA,
        denominator = cards::ADSL,
        id = USUBJID,
        include = AETERM
      ),
      variables = c(AESOC, AETERM),
      by = TRTA
    ),
    error = TRUE
  )

  # the overall variable may not be passed as a hierarchy variable
  expect_snapshot(
    tbl_ard_hierarchical(
      cards = cards::ard_stack_hierarchical(
        data = ADAE_subset,
        variables = c(AESOC, AETERM),
        by = TRTA,
        denominator = cards::ADSL,
        id = USUBJID,
        over_variables = TRUE
      ),
      variables = c(..ard_hierarchical_overall.., AETERM),
      by = TRTA
    ),
    error = TRUE
  )

  # statistic not available in the ARD
  expect_snapshot(
    tbl_ard_hierarchical(
      cards = cards::ard_stack_hierarchical_count(
        data = ADAE_subset,
        variables = c(AESOC, AETERM),
        by = TRTA
      ),
      variables = c(AESOC, AETERM),
      by = TRTA
    ),
    error = TRUE
  )
})
