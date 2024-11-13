skip_on_cran()
skip_if_not(is_pkg_installed("withr"))

ADAE_subset <- cards::ADAE |>
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:5],
    AETERM %in% unique(cards::ADAE$AETERM)[1:5]
  )

test_that("add_overall.tbl_hierarchical() works", {
  withr::local_options(list(width = 220))

  expect_silent(
    tbl <-
      tbl_hierarchical(
        data = ADAE_subset,
        variables = c(AESOC, AETERM),
        by = TRTA,
        denominator = cards::ADSL |> mutate(TRTA = ARM),
        id = USUBJID,
        digits = everything() ~ list(p = 1),
        overall_row = TRUE,
        label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
      )
  )

  # vanilla output looks as expected
  expect_snapshot(
    add_overall(tbl) |>
      as.data.frame()
  )

  # the overall column will go to the end
  expect_equal(
    add_overall(tbl, last = TRUE) |>
      as.data.frame(col_labels = FALSE) |>
      names() |>
      dplyr::last(),
    "stat_0"
  )

  # the new column label can change
  expect_equal(
    add_overall(tbl, last = TRUE, col_label = "That's All") |>
      as.data.frame(col_labels = TRUE) |>
      names() |>
      dplyr::last(),
    "That's All"
  )

  # we can modify the statistic and rounding
  expect_equal(
    add_overall(tbl, statistic = ~"{p}%", digits = everything() ~ list(p = 2)) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull("stat_0") |>
      getElement(1L),
    "42.52%"
  )
})

test_that("add_overall.tbl_hierarchical_count() works", {
  withr::local_options(list(width = 220))

  expect_silent(
    tbl <-
      tbl_hierarchical_count(
        data = ADAE_subset,
        variables = c(AESOC, AETERM, AESEV),
        by = TRTA,
        overall_row = TRUE,
        label = list(..ard_hierarchical_overall.. = "Total Number of AEs")
      )
  )

  # vanilla output looks as expected
  expect_snapshot(
    add_overall(tbl) |>
      as.data.frame()
  )

  # the overall column will go to the end
  expect_equal(
    add_overall(tbl, last = TRUE) |>
      as.data.frame(col_labels = FALSE) |>
      names() |>
      dplyr::last(),
    "stat_0"
  )

  # the new column label can change
  expect_equal(
    add_overall(tbl, last = TRUE, col_label = "That's All") |>
      as.data.frame(col_labels = TRUE) |>
      names() |>
      dplyr::last(),
    "That's All"
  )

  # we can modify the statistic and rounding
  expect_equal(
    add_overall(tbl, statistic = ~"({n})", digits = everything() ~ list(n = 2)) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull("stat_0") |>
      getElement(1L),
    "(210.00)"
  )
})
