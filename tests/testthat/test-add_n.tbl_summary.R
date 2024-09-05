skip_on_cran()

test_that("add_n.tbl_summary() works", {
  # with stratifying variable
  expect_error(
    tbl <- trial |>
      tbl_summary(by = trt, include = c(age, grade, response)) |>
      add_n(),
    NA
  )
  expect_equal(
    as.data.frame(tbl, col_labels = FALSE)$n,
    c("189", NA, "200", NA, NA, NA, "193", NA)
  )

  # without stratifying variable
  expect_error(
    tbl <- trial |>
      tbl_summary(include = c(age, grade, response)) |>
      add_n(),
    NA
  )
  expect_equal(
    as.data.frame(tbl, col_labels = FALSE)$n,
    c("189", NA, "200", NA, NA, NA, "193", NA)
  )

  # with multiple stats reported
  expect_snapshot(
    trial |>
      tbl_summary(include = c(age, grade, response)) |>
      add_n(statistic = "{N_nonmiss} / {N_obs}", footnote = TRUE, last = TRUE) |>
      as.data.frame()
  )
})

test_that("add_n.tbl_summary(statistic) error messaging", {
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
      add_n(statistic = "no_curlies")
  )
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
      add_n(statistic = "{not_a_stat}")
  )
})
