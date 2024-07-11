skip_if_not(is_pkg_installed(c("ggstats", "broom.helpers"), reference_pkg = "gtsummary"))

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
