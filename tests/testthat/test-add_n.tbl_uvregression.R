skip_if_not(is_pkg_installed(c("broom.helpers", "broom"), reference_pkg = "gtsummary"))

test_that("add_n.tbl_uvregression() works", {
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
  # total N added to table is accurate
  expect_silent(
    res <- tbl1 |> add_n()
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_n |>
      na.omit() |>
      unique(),
    c(
      tidyr::drop_na(trial, marker) |> nrow() |> as.character(),
      tidyr::drop_na(trial, age) |> nrow() |> as.character()
    )
  )

  expect_silent(
    tbl1 <-
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
  # N for levels added to table is accurate
  expect_silent(
    res <- tbl1 |> add_n(location = "level")
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_n |>
      na.omit() |>
      unique(),
    tidyr::drop_na(trial, response, trt) |>
      with(table(trt)) |>
      as.character()
  )

  # N for label added to table is accurate
  expect_error(
    res <- tbl1 |> add_n(location = c("label", "level")),
    NA
  )
  expect_equal(
    as.data.frame(res, col_label = FALSE)$stat_n |>
      na.omit() |>
      unique(),
    tidyr::drop_na(trial, response, trt) |>
      with(table(trt)) |>
      as.integer() %>%
      {c(sum(.), .)} |> # styler: off
      as.character()
  )
})
