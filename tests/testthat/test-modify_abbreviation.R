skip_on_cran()
skip_if_not(is_pkg_installed(c("cardx", "broom", "broom.helpers")))

test_that("modify_abbreviation()", {
  expect_silent(
    tbl <-
      tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile") |>
      modify_abbreviation("Q3 = Third Quartile")
  )
  expect_equal(
    tbl$table_styling$abbreviation,
    dplyr::tribble(
      ~column,               ~abbreviation, ~text_interpret,
      NA_character_, "Q1 = First Quartile",        "gt::md",
      NA_character_, "Q3 = Third Quartile",        "gt::md"
    )
  )
})


test_that("remove_abbreviation()", {
  expect_silent(
    tbl <-
      tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile") |>
      modify_abbreviation("Q3 = Third Quartile") |>
      remove_abbreviation("Q3 = Third Quartile")
  )
  expect_equal(
    tbl$table_styling$abbreviation,
    dplyr::tribble(
      ~column,               ~abbreviation, ~text_interpret,
      NA_character_, "Q1 = First Quartile",        "gt::md",
    )
  )
})

test_that("remove_abbreviation() messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      remove_abbreviation("Q3 = Third Quartile")
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile") |>
      remove_abbreviation("Q3 = Third Quartile")
  )
})
