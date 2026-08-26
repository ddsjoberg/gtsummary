skip_on_cran()
skip_if_pkg_not_installed(c("ggstats", "broom.helpers"))

test_that("plot.tbl_regression() works", {
  plot_obj <-
    lm(age ~ grade, trial) |>
    tbl_regression() %>%
    plot()

  expect_equal(
    plot_obj$data$reference_row,
    c(TRUE, FALSE, FALSE)
  )

  plot_obj1 <- lm(age ~ grade, trial) |>
    tbl_regression() %>%
    plot(remove_reference_rows = TRUE)

  expect_equal(
    plot_obj1$data$reference_row,
    c(FALSE, FALSE)
  )

  plot_obj2 <- lm(age ~ grade, trial) |>
    tbl_regression() %>%
    plot(remove_header_rows = FALSE)

  expect_equal(
    plot_obj2$data$header_row,
    c(TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("plot.tbl_regression() passes arguments in ... to ggstats::ggcoef_plot()", {
  # arguments passed via ... should reach ggstats::ggcoef_plot() and not error (#2470)
  expect_no_error(
    lm(age ~ grade, trial) |>
      tbl_regression() %>%
      plot(point_size = 5)
  )

  # unrecognized arguments are still rejected by ggstats::ggcoef_plot()
  expect_error(
    lm(age ~ grade, trial) |>
      tbl_regression() %>%
      plot(not_an_argument = "red")
  )
})
