skip_on_cran()
skip_if_not(is_pkg_installed(c("car", "broom.helpers", "cardx")))

test_that("add_vif(x)", {
  expect_snapshot(
    lm(age ~ marker + grade, trial) |>
      tbl_regression() |>
      add_vif() |>
      as.data.frame()
  )
})

test_that("add_vif(x) messaging", {
  expect_error(
    add_vif(letters),
    "The `x` argument must be class <tbl_regression>, not a character vector."
  )
})

test_that("add_vif(statistic)", {
  expect_snapshot(
    lm(age ~ marker + grade, trial) |>
      tbl_regression() |>
      add_vif(statistic = c("df", "aGVIF")) |>
      as.data.frame()
  )
})

test_that("add_vif(statistic) messaging", {
  expect_error(
    lm(age ~ marker + grade, trial) |>
      tbl_regression() |>
      add_vif(statistic = c("df", "xxxxxxx")),
    "`statistic` must be one of"
  )
})

test_that("add_vif(estimate_fun)", {
  expect_snapshot(
    lm(age ~ marker + response, trial) |>
      tbl_regression() |>
      add_vif(statistic = "VIF", estimate_fun = label_style_sigfig(digits = 5)) |>
      as.data.frame()
  )
})

