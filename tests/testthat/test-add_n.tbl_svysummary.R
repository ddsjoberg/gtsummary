skip_on_cran()
skip_if_not(is_pkg_installed("survey"))

svy_trial <- survey::svydesign(~1, data = trial, weights = ~1)

test_that("add_n.tbl_svysummary() works", {
  # with stratifying variable
  expect_error(
    tbl <- svy_trial |>
      tbl_svysummary(by = trt, include = c(age, grade, response)) |>
      add_n(),
    NA
  )
  expect_equal(
    as.data.frame(tbl, col_labels = FALSE)$n,
    c("189", NA, "200", NA, NA, NA, "193", NA)
  )

  # without stratifying variable
  expect_error(
    tbl <- svy_trial |>
      tbl_svysummary(include = c(age, grade, response)) |>
      add_n(),
    NA
  )
  expect_equal(
    as.data.frame(tbl, col_labels = FALSE)$n,
    c("189", NA, "200", NA, NA, NA, "193", NA)
  )

  # with multiple stats reported
  expect_snapshot(
    svy_trial |>
      tbl_svysummary(include = c(age, grade, response)) |>
      add_n(statistic = "{N_nonmiss} / {N_obs} ({N_nonmiss_unweighted})", footnote = TRUE, last = TRUE) |>
      as.data.frame()
  )
})

test_that("add_n.tbl_svysummary(statistic) error messaging", {
  expect_snapshot(
    error = TRUE,
    svy_trial |>
      tbl_svysummary(by = trt, include = c(trt, age, grade, response)) |>
      add_n(statistic = "no_curlies")
  )
  expect_snapshot(
    error = TRUE,
    svy_trial |>
      tbl_svysummary(by = trt, include = c(trt, age, grade, response)) |>
      add_n(statistic = "{not_a_stat}")
  )
})
