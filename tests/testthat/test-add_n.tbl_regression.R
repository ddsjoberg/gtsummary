skip_on_cran()

test_that("add_n.tbl_regression() works", {
  tbl <-
    glm(response ~ grade + age, trial, family = binomial) %>%
    tbl_regression()

  # total N added to table is accurate
  expect_error(
    res <- tbl |> add_n(),
    NA
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_n |>
      na.omit() |>
      unique(),
    tidyr::drop_na(trial, response, grade, age) |>
      nrow() |>
      as.character()
  )

  # N added to levels is accurate
  expect_error(
    res <- tbl |> add_n(location = "level"),
    NA
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_n |>
      na.omit(),
    tidyr::drop_na(trial, response, grade, age) |>
      with(table(grade)) |>
      as.integer() %>%
      c(sum(.)) |>
      as.character(),
    ignore_attr = TRUE
  )

  # N added to levels and labels is accurate
  expect_error(
    res <- tbl |> add_n(location = c("label", "level")),
    NA
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_n |>
      na.omit(),
    tidyr::drop_na(trial, response, grade, age) |>
      with(table(grade)) |>
      as.integer() %>%
      {c(sum(.), ., sum(.))} |> # styler: off
      as.character(),
    ignore_attr = TRUE
  )
})
