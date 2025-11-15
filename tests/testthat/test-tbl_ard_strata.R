test_that("tbl_ard_strata() works", {
  withr::local_options(list(width = 200))

  # test stacking, stratifying lab PARAMs
  expect_snapshot(
    cards::ADLB |>
      dplyr::filter(
        AVISIT %in% c("Baseline"),
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

  # test merging
  expect_snapshot(
    cards::ADSL |>
      cards::ard_tabulate(
        by = TRTA,
        variables = AGEGR1
      ) |>
      tbl_ard_strata(
        strata = c(group1, group1_level),
        ~ .x |>
          tbl_ard_summary(),
        .header = "**Group: {strata}**"
      ) |>
      as.data.frame()
  )

  # check multiple by groups
  expect_snapshot(
    cards::ADSL |>
      cards::ard_tabulate(
        by = c(TRTA, SEX),
        variables = AGEGR1
      ) |>
      tbl_ard_strata2(
        strata = c(group1, group1_level, group2, group2_level),
        ~ .x |>
          tbl_ard_summary() |>
          modify_header(stat_0 = .y)
      ) |>
      as.data.frame()
  )
})

test_that("tbl_ard_strata() messaging", {
  # test inconsistent group variables
  expect_snapshot(
    error = TRUE,
    dplyr::bind_rows(
      cards::ard_tabulate(cards::ADSL, variables = AGEGR1, by = TRTA),
      cards::ard_tabulate(cards::ADSL, variables = AGEGR1, by = SEX)
    ) |>
      tbl_ard_strata2(
        strata = c(group1, group1_level),
        ~ .x |>
          tbl_ard_summary()
      )
  )

  # error when strata are not present
  expect_snapshot(
    error = TRUE,
    cards::ard_tabulate(cards::ADSL, variables = AGEGR1, by = TRTA) |>
      tbl_ard_strata2(
        strata = c(group2, group2_level),
        ~ .x |>
          tbl_ard_summary()
      )
  )

  # error when strata does not select any columns
  expect_snapshot(
    error = TRUE,
    cards::ard_tabulate(cards::ADSL, variables = AGEGR1) |>
      tbl_ard_strata2(
        strata = any_of("dddd"),
        ~ .x |>
          tbl_ard_summary()
      )
  )
})
