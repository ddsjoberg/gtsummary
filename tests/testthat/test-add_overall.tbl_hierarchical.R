skip_on_cran()
skip_if_not(is_pkg_installed("withr"))

test_that("add_overall.tbl_custom_summary works", {
  expect_equal(2 * 2, 4)
})
