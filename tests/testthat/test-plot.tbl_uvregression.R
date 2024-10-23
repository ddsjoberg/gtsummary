skip_on_cran()
skip_if_not(is_pkg_installed(c("ggstats", "broom.helpers")))

test_that("plot.tbl_uvregression() works", {
  plot_obj <- trial %>%
    tbl_uvregression(method = lm, y = marker, include = c("grade")) %>%
    plot()

  expect_equal(
    plot_obj$data$reference_row,
    c(TRUE, FALSE, FALSE)
  )

  plot_obj1 <- trial %>%
    tbl_uvregression(method = lm, y = marker, include = c("grade")) %>%
    plot(remove_reference_rows = TRUE)

  expect_equal(
    plot_obj1$data$reference_row,
    c(FALSE, FALSE)
  )

  plot_obj2 <- trial %>%
    tbl_uvregression(method = lm, y = marker, include = c("grade")) %>%
    plot(remove_header_rows = FALSE)

  expect_equal(
    plot_obj2$data$header_row,
    c(TRUE, FALSE, FALSE, FALSE)
  )
})
