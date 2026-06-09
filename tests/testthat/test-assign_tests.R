skip_on_cran()
skip_if_pkg_not_installed(c("broom", "broom.helpers"))

test_that("add_difference() errors when test does not return an estimate", {
  # custom test that returns only p.value, no estimate
  no_estimate_test <- function(data, variable, by, ...) {
    data.frame(p.value = 0.05)
  }

  expect_error(
    trial |>
      tbl_summary(by = trt, include = age) |>
      add_difference(test = list(age ~ no_estimate_test)),
    "did not return a difference estimate"
  )
})

test_that("add_difference() with mcnemar.test does not error", {
  trial_paired <-
    trial |>
    dplyr::select(trt, response) |>
    dplyr::mutate(.by = trt, id = dplyr::row_number()) |>
    tidyr::drop_na() |>
    dplyr::filter(.by = id, dplyr::n() == 2)

  expect_error(
    trial_paired |>
      tbl_summary(by = trt, include = response) |>
      add_difference(test = list(response ~ "mcnemar.test"), group = "id"),
    NA
  )
})
