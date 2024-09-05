skip_on_cran()
skip_if_not(is_pkg_installed(c("cardx", "broom.helpers"), reference_pkg = "gtsummary"))

test_that("show_header_names() works with tbl_summary()", {
  expect_snapshot(
    tbl_summary(trial, include = age, by = trt, missing = "no") |>
      show_header_names()
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
    )|>
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
      )|>
      show_header_names()
  )
})
