test_that("tbl_ard_strata() works", {
  withr::local_options(list(width = 200))
  expect_snapshot(
    cards::ADLB |>
      dplyr::filter(
        AVISIT %in% c("Baseline", "Week 12", "Week 24"),
        PARAMCD %in% c("ALB", "BUN")
      ) |>
      cards::ard_summary(
        strata = PARAM,
        by = TRTA,
        variables = AVAL
      ) |>
      tbl_ard_strata2(
        strata = c(group2, group2_level),
        ~ .x |>
          tbl_ard_summary(by = TRTA, label = list(AVAL = .y)),
        .combine_with = "tbl_stack",
        .combine_args = list(group_header = NULL)
      ) |>
      as.data.frame()
  )
})
