skip_on_cran()

test_that("add_overall.tbl_ard_summary() works", {
  # build primary table
  tbl <-
    cards::ard_stack(
      mtcars,
      .by = am,
      cards::ard_continuous(variables = mpg),
      cards::ard_categorical(variables = cyl),
      .missing = TRUE,
      .attributes = TRUE,
      .total_n = TRUE
    ) |>
    tbl_ard_summary(by = am, statistic = list(cyl = "{n}", mpg = "{mean}"))

  # create ARD with overall results
  ard_overall <-
    cards::ard_stack(
      mtcars,
      cards::ard_continuous(variables = mpg),
      cards::ard_categorical(variables = cyl),
      .missing = TRUE,
      .attributes = TRUE,
      .total_n = TRUE
    )

  # typical usage works
  expect_error(
    res <- add_overall(tbl, cards = ard_overall),
    NA
  )
  expect_equal(
    res |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::select(-all_stat_cols(), stat_0),
    ard_overall |>
      tbl_ard_summary(
        statistic = list(cyl = "{n}", mpg = "{mean}")
      ) |>
      as.data.frame(col_labels = FALSE)
  )
  # check default header is correct
  expect_equal(
    res$table_styling$header |>
      dplyr::filter(column == "stat_0") |>
      dplyr::pull(label),
    "**Overall**"
  )

  # we're able to modify the statistic arguments
  expect_error(
    res <-
      add_overall(
        tbl,
        cards = ard_overall,
        statistic = list(cyl = "{n} / {N}", mpg = "{mean}")
      ),
    NA
  )
  expect_equal(
    res |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::select(-all_stat_cols(), stat_0),
    ard_overall |>
      tbl_ard_summary(
        statistic = list(cyl = "{n} / {N}", mpg = "{mean}")
      ) |>
      as.data.frame(col_labels = FALSE)
  )

  # we can change the column header and move the overall column
  expect_error(
    res <-
      tbl |>
      add_overall(cards = ard_overall, last = TRUE, col_label = "**All**"),
    NA
  )
  # check the overall column is moved to the end
  expect_equal(
    res |>
      as.data.frame(col_label = FALSE) |>
      names() |>
      dplyr::last(),
    "stat_0"
  )
  # check header is correct
  expect_equal(
    res$table_styling$header |>
      dplyr::filter(column == "stat_0") |>
      dplyr::pull(label),
    "**All**"
  )
})


test_that("add_overall.tbl_summary() errors", {
  # no stratifying variable
  expect_snapshot(
    error = TRUE,
    ard_overall |> tbl_ard_summary() |> add_overall(cards = ard_overall)
  )
})
