skip_on_cran()
skip_if_not(is_pkg_installed(c("broom.helpers", "broom")))

test_that("add_nevent.tbl_uvregression() works", {
  expect_silent(
    tbl <-
      tbl_uvregression(
        trial,
        y = response,
        method = glm,
        method.args = list(family = binomial),
        include = c(trt),
        exponentiate = TRUE,
        hide_n = TRUE
      )
  )
  # total N added to table is accurate
  expect_silent(
    res <- tbl |> add_nevent()
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_nevent |>
      na.omit() |>
      unique(),
    tidyr::drop_na(trial, response, trt) |>
      dplyr::filter(response == 1) |>
      nrow() |>
      as.character()
  )

  # N added to levels is accurate
  expect_silent(
    res <- tbl |> add_nevent(location = "level")
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_nevent |>
      na.omit(),
    tidyr::drop_na(trial, response, trt) |>
      dplyr::filter(response == 1) |>
      with(table(trt)) |>
      as.integer() |>
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
    tidyr::drop_na(trial, response, trt) |>
      dplyr::filter(response == 1) |>
      with(table(trt)) |>
      as.integer() %>%
      {c(sum(.), .)} |> # styler: off
      as.character(),
    ignore_attr = TRUE
  )
})

test_that("add_nevent.tbl_uvregression() messaging for linear model", {
  expect_silent(
    tbl1 <- tbl_uvregression(
      trial,
      x = trt,
      include = c(marker, age),
      show_single_row = trt,
      method = lm,
      hide_n = TRUE
    )
  )
  expect_error(
    res <- tbl1 |> add_nevent(),
    "Reporting event N on label rows is not available for this model type."
  )
})
