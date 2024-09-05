skip_on_cran()
test_that("add_overall.tbl_continuous() works", {
  # typical usage works
  expect_error(
    res <-
      mtcars |>
      tbl_continuous(
        by = am,
        include = gear,
        variable = mpg
      ) |>
      add_overall(),
    NA
  )
  expect_equal(
    res |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::select(-all_stat_cols(), stat_0),
    mtcars |>
      tbl_continuous(
        include = gear,
        variable = mpg
      ) |>
      as.data.frame(col_labels = FALSE)
  )
  # check default header is correct
  expect_equal(
    res$table_styling$header |>
      dplyr::filter(column == "stat_0") |>
      dplyr::pull(label),
    "**Overall**  \nN = 32"
  )

  # we're able to modify the statistic and digits arguments
  expect_error(
    res <-
      mtcars |>
      tbl_continuous(
        by = am,
        include = gear,
        variable = mpg
      ) |>
      add_overall(
        statistic = list(gear = "{mean}"),
        digits = gear ~ 4
      ),
    NA
  )
  expect_equal(
    res |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::select(-all_stat_cols(), stat_0),
    mtcars |>
      tbl_continuous(
        include = gear,
        variable = mpg,
        statistic = list(gear = "{mean}"),
        digits = gear ~ 4
      ) |>
      as.data.frame(col_labels = FALSE)
  )

  # we can change the column header and move the overall column
  expect_error(
    res <-
      mtcars |>
      tbl_continuous(
        by = am,
        include = gear,
        variable = mpg
      ) |>
      add_overall(last = TRUE, col_label = "**All Transmision Types**"),
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
    "**All Transmision Types**"
  )
})


test_that("add_overall.tbl_continuous() errors", {
  # no stratifying variable
  expect_snapshot(
    error = TRUE,
    tbl_continuous(
      mtcars,
      include = gear,
      variable = mpg
    ) |>
      add_overall()
  )
})
