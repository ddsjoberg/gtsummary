skip_on_cran()

test_that("add_overall.tbl_summary() works", {
  # typical usage works
  expect_error(
    res <-
      mtcars |>
      tbl_summary(
        by = am,
        statistic = list(cyl = "{n}", mpg = "{mean}"),
        include = c(cyl, mpg, disp)
      ) |>
      add_overall(),
    NA
  )
  expect_equal(
    res |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::select(-all_stat_cols(), stat_0),
    mtcars |>
      tbl_summary(
        include = c(cyl, mpg, disp),
        statistic = list(cyl = "{n}", mpg = "{mean}")
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
      tbl_summary(
        by = am,
        include = c(cyl, mpg, disp)
      ) |>
      add_overall(
        statistic = list(cyl = "{n}", mpg = "{mean}"),
        digits = mpg ~ 4
      ),
    NA
  )
  expect_equal(
    res |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::select(-all_stat_cols(), stat_0),
    mtcars |>
      tbl_summary(
        include = c(cyl, mpg, disp),
        statistic = list(cyl = "{n}", mpg = "{mean}"),
        digits = mpg ~ 4
      ) |>
      as.data.frame(col_labels = FALSE)
  )

  # we can change the column header and move the overall column
  expect_error(
    res <-
      iris |>
      tbl_summary(by = "Species") |>
      add_overall(last = TRUE, col_label = "**All Species**"),
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
    "**All Species**"
  )
})


test_that("add_overall.tbl_summary() errors", {
  # no stratifying variable
  expect_snapshot(
    error = TRUE,
    tbl_summary(mtcars) |> add_overall()
  )

  # Run add_overall() after after `add_stat_label()`
  expect_snapshot(
    error = TRUE,
    tbl_summary(mtcars, by = am, include = "mpg", type = all_continuous() ~ "continuous2") |>
      add_stat_label(label = mpg ~ "UPDATED!") |>
      add_overall()
  )
})

# Addressing this issue: https://github.com/insightsengineering/crane/issues/41
test_that("add_overall.tbl_summary() gets all table_styling instructions", {
  expect_equal({
    tbl_styling_test <- function(...) {
      tbl <- tbl_summary(...) %>%
        modify_post_fmt_fun(
          fmt_fun = ~ifelse(. == "0 (0%)", "0", .),
          columns = all_stat_cols()
        ) %>%
        structure(.Data = ., class = c("tbl_styling_test", class(.)))

      names(tbl$call_list) <-  "tbl_styling_test"
      tbl
    }

    trial |>
      dplyr::mutate(grade = fct_expand(grade, "Other")) |>
      tbl_styling_test(
        by = trt,
        include = grade
      ) |>
      add_overall() |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull("stat_0") |>
      dplyr::last()},
    "0"
  )
})
