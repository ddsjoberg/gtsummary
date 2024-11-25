skip_on_cran()
skip_if_not(is_pkg_installed("survey"))

tbl_ard <- cards::ard_stack(
  data = cards::ADSL |> dplyr::mutate(AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80"))),
  cards::ard_categorical(variables = "AGEGR1"),
  cards::ard_continuous(variables = "AGE"),
  .attributes = TRUE,
  .missing = TRUE,
  .total_n = TRUE
) |>
  tbl_ard_summary()

test_that("add_stat_label(location='row') standard use", {
  expect_snapshot(
    tbl_ard |>
      add_stat_label(location='row') |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )
})

test_that("add_stat_label(location='column') standard use", {
  expect_snapshot(
    tbl_ard |>
      add_stat_label(location='column') |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )

  expect_snapshot(
    tbl_ard |>
      add_stat_label(location = "column", label = all_categorical() ~ "no. (%)") |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )
})

test_that("add_stat_label(label) standard use", {
  expect_snapshot(
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE,
      .total_n = TRUE
    ) |>
      tbl_ard_summary(
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}")
      ) |>
      add_stat_label(label = AGE ~ c("Median (IQR)", "Range")) |>
      as.data.frame()
  )
})

test_that("add_stat_label(label) messaging", {
  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE,
      .total_n = TRUE
    ) |>
      tbl_ard_summary(
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}")
      ) |>
      add_stat_label(label = AGE ~ letters)
  )

  expect_snapshot(
    error = TRUE,
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE,
      .total_n = TRUE
    ) |>
      tbl_ard_summary(
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}")
      ) |>
      add_stat_label(label = AGE ~ c("Median (IQR)", "Range", "TOO LONG!"))
  )
})

test_that("add_stat_label() messaging", {
  expect_snapshot(
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE,
      .total_n = TRUE
    ) |>
      tbl_ard_summary(
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}")
      ) |>
      add_stat_label() |>
      add_stat_label() |>
      invisible()
  )
})
