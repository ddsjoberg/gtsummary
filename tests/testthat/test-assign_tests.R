skip_on_cran()
skip_if_pkg_not_installed(c("broom", "broom.helpers"))

test_that("add_difference() errors with clear message for incompatible tests", {
  trial_paired <-
    trial |>
    dplyr::select(trt, marker, response) |>
    dplyr::mutate(.by = trt, id = dplyr::row_number()) |>
    tidyr::drop_na() |>
    dplyr::filter(.by = id, dplyr::n() == 2)

  expect_error(
    trial_paired |>
      tbl_summary(by = trt, include = marker) |>
      add_difference(test = list(marker ~ "paired.wilcox.test"), group = "id"),
    "not available for.*add_difference"
  )

  expect_error(
    trial_paired |>
      tbl_summary(by = trt, include = response) |>
      add_difference(test = list(response ~ "mcnemar.test"), group = "id"),
    "not available for.*add_difference"
  )
})
