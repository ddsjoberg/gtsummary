skip_on_cran()

mod_lm <- lm(hp ~ am, data = mtcars)
mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
mod_poisson <-
  glm(count ~ age + trt,
    trial |> dplyr::mutate(count = dplyr::row_number() %% 10),
    family = poisson
  )
mod_lm_interaction <- lm(age ~ trt * grade * response, data = trial)


test_that("stats::glm() logistic regression works", {
  expect_snapshot(tbl_regression(mod_logistic) |> as.data.frame())

  expect_equal(
    tbl_regression(mod_logistic)$table_styling$header |>
      dplyr::filter(column == "estimate") |>
      dplyr::pull(label),
    "**log(OR)**"
  )

  expect_false(
    "conf.low" %in% names(tbl_regression(mod_logistic, conf.int = FALSE) |> as_tibble(col_labels = FALSE))
  )

  expect_snapshot(
    tbl_regression(
      mod_logistic,
      exponentiate = TRUE,
      estimate_fun = label_style_ratio(digits = 1)
    ) |>
      as.data.frame()
  )

  expect_equal(
    tbl_regression(mod_logistic, exponentiate = TRUE)$table_styling$header |>
      dplyr::filter(column == "estimate") |>
      dplyr::pull(label),
    "**OR**"
  )
})

test_that("stats::glm() poisson regression works", {
  expect_snapshot(
    tbl_regression(
      mod_poisson,
      show_single_row = "trt",
      estimate_fun = label_style_ratio(digits = 1)
    ) |>
      as.data.frame()
  )

  expect_equal(
    tbl_regression(mod_poisson)$table_styling$header |>
      dplyr::filter(column == "estimate") |>
      dplyr::pull(label),
    "**log(IRR)**"
  )

  expect_snapshot(
    tbl_regression(
      mod_poisson,
      exponentiate = TRUE,
      show_single_row = "trt",
      estimate_fun = label_style_ratio(digits = 1)
    ) |>
      as.data.frame()
  )

  expect_equal(
    tbl_regression(mod_poisson, exponentiate = TRUE)$table_styling$header |>
      dplyr::filter(column == "estimate") |>
      dplyr::pull(label),
    "**IRR**"
  )
})

test_that("stats::lm() linear regression works", {
  expect_snapshot(tbl_regression(mod_lm) |> as.data.frame())
})

test_that("stats::lm() works with interactions", {
  expect_snapshot(
    tbl_regression(
      mod_lm_interaction,
      label = list(trt = "Tx")
    ) |>
      as.data.frame()
  )
})

test_that("tbl_regression(include)", {
  # only the single row of 'am' appears in table
  expect_equal(
    lm(mpg ~ cyl + am, mtcars) |>
      tbl_regression(include = am) |>
      as.data.frame() |>
      nrow(),
    1L
  )
})

test_that("tbl_regression(show_single_row)", {
  # trt variable displays on single row
  expect_equal(
    lm(age ~ trt + marker, trial) |>
      tbl_regression(show_single_row = trt) |>
      getElement("table_body") |>
      dplyr::filter(variable == "trt") |>
      nrow(),
    1L
  )
})

test_that("tbl_regression(conf.level)", {
  expect_error(
    tbl <- lm(age ~ trt + marker, trial) |>
      tbl_regression(conf.level = 0.80),
    NA
  )

  expect_equal(
    tbl$table_styling$header |>
      dplyr::filter(column %in% "conf.low") |>
      dplyr::pull(label),
    "**80% CI**"
  )
})

test_that("tbl_regression(intercept)", {
  expect_snapshot(
    lm(age ~ marker, trial) |>
      tbl_regression(intercept = TRUE) |>
      as.data.frame()
  )
})

test_that("tbl_regression(add_estimate_to_reference_rows)", {
  expect_snapshot(
    lm(age ~ marker, trial) |>
      tbl_regression(add_estimate_to_reference_rows = TRUE) |>
      as.data.frame()
  )

  expect_snapshot(
    glm(response ~ trt, trial, family = binomial()) |>
      tbl_regression(add_estimate_to_reference_rows = TRUE, exponentiate = TRUE) |>
      as.data.frame()
  )
})

test_that("tbl_regression(conf.int)", {
  # no confidence interval
  expect_equal(
    lm(age ~ marker, trial) |>
      tbl_regression(conf.int = FALSE) |>
      as.data.frame(col_labels = FALSE) |>
      names(),
    c("label", "estimate", "p.value")
  )
})
