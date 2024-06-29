test_that("tbl_strata() works", {
  expect_error(
    trial |>
      tbl_strata(
        strata = grade,
        ~ tbl_summary(.x, include = age)
      ),
    NA
  )
})
