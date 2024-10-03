skip_on_cran()

test_that("tbl_ard_hierarchical() works", {
  ard <-
    cards::ard_stack_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AETERM, AESEV),
      by = TRTA,
      denominator = cards::ADSL |> mutate(TRTA = ARM),
      id = USUBJID
    )

  expect_error(
    tbl_ard_hierarchical(
      cards = ard,
      variables = c(AESOC, AETERM, AESEV),
      by = TRTA
    ),
    NA
  )
})
