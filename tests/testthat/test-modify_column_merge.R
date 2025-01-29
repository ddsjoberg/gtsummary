skip_on_cran()
skip_if_not(is_pkg_installed(c("broom.helpers", "cardx")))

mod <- lm(marker ~ age + grade, trial) |> tbl_regression()

test_that("modify_column_merge() works", {
  expect_error(
    tbl <-
      mod |>
      modify_column_merge(
        pattern = "{estimate} ({conf.low}, {conf.high})",
        rows = !is.na(estimate)
      ),
    NA
  )

  expect_equal(
    as_tibble(tbl, col_labels = FALSE) |>
      dplyr::pull(estimate),
    c(
      "0.00 (-0.01, 0.01)", NA, NA,
      "-0.38 (-0.69, -0.07)", "-0.12 (-0.43, 0.19)"
    )
  )
})

test_that("modify_column_merge() messaging", {
  expect_snapshot(
    error = TRUE,
    mod |>
      modify_column_merge(
        pattern = "{not_a_column} ({conf.low}, {conf.high})",
        rows = !is.na(estimate)
      )
  )

  expect_error(
    mod |>
      modify_column_merge(
        pattern = "no columns selected",
        rows = !is.na(estimate)
      )
  )

  expect_snapshot(
    error = TRUE,
    lm(mpg ~ factor(am), mtcars) |>
      tbl_regression() |>
      modify_column_merge(
        rows = !is.na(conf.low),
        pattern = "{conf.low}:::{not_in_table}"
      )
  )
})

test_that("remove_column_merge() works", {
  expect_equal(
    lm(mpg ~ am, mtcars) |>
      tbl_regression() |>
      remove_column_merge(columns = c("conf.low", "conf.high")) |>
      modify_column_unhide(c("conf.low", "conf.high")) |>
      as_tibble(col_labels = FALSE) |>
      names() |>
      intersect(c("conf.low", "conf.high")),
    c("conf.low", "conf.high")
  )
})
