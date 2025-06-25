skip_on_cran()
skip_if_not(is_pkg_installed(c("cardx", "broom.helpers")))

test_that("show_header_names() works with tbl_summary()", {
  expect_snapshot(
    tbl_summary(trial, include = age, by = trt, missing = "no") |>
      show_header_names()
  )
})

test_that("show_header_names(show_hidden)", {
  expect_snapshot(
    tbl_summary(trial, include = age, by = trt, missing = "no") |>
      show_header_names(show_hidden = TRUE)
  )
})

test_that("show_header_names() works with tbl_regression", {
  mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
  expect_snapshot(
    tbl_regression(mod_logistic) |>
      show_header_names()
  )
})

test_that("show_header_names() works with tbl_uvregression", {
  expect_snapshot(
    tbl_uvregression(
      trial,
      x = trt,
      include = c(marker, age),
      show_single_row = trt,
      method = lm
    ) |>
      show_header_names()
  )
})

test_that("show_header_names() works with tbl_survfit", {
  expect_snapshot(
    trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        probs = 0.5
      ) |>
      show_header_names()
  )
})

test_that("show_header_names() returns fallback value for unknown class", {
  test_table <-
    trial |>
    tbl_summary(include = age, by = trt)

  class(test_table$table_styling$header$modify_stat_N) <- "I made this class up :)"
  expect_snapshot(
    test_table |> show_header_names()
  )
})


test_that("show_header_names() returns single class value", {
  test_table <-
    trial |>
    tbl_summary(include = age, by = trt)

  class(test_table$table_styling$header$modify_stat_N) <- c("my_class", "integer")
  expect_snapshot(
    test_table |> show_header_names()
  )
})


test_that("show_header_names() has all values aligned", {
  withr::local_options(list(width = 120))
    test_table <- tbl_hierarchical(
      data = cards::ADAE,
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL |> mutate(TRTA = ARM),
      id = USUBJID
    )

    expect_snapshot(
      test_table |> show_header_names()
    )
})
