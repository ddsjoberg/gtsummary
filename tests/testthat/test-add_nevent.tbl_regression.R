skip_on_cran()
test_that("add_nevent.tbl_regression() works", {
  tbl <-
    glm(response ~ grade + age, trial, family = binomial) %>%
    tbl_regression()

  # total N added to table is accurate
  expect_error(
    res <- tbl |> add_nevent(),
    NA
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_nevent |>
      na.omit() |>
      unique(),
    tidyr::drop_na(trial, response, grade, age) |>
      dplyr::filter(response == 1) |>
      nrow() |>
      as.character()
  )

  # N added to levels is accurate
  expect_error(
    res <- tbl |> add_nevent(location = "level"),
    NA
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_nevent |>
      na.omit(),
    tidyr::drop_na(trial, response, grade, age) |>
      dplyr::filter(response == 1) |>
      with(table(grade)) |>
      as.integer() %>%
      c(sum(.)) |>
      as.character(),
    ignore_attr = TRUE
  )

  # N added to levels and labels is accurate
  expect_error(
    res <- tbl |> add_nevent(location = c("label", "level")),
    NA
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_nevent |>
      na.omit(),
    tidyr::drop_na(trial, response, grade, age) |>
      dplyr::filter(response == 1) |>
      with(table(grade)) |>
      as.integer() %>%
      {c(sum(.), ., sum(.))} |> # styler: off
      as.character(),
    ignore_attr = TRUE
  )
})
