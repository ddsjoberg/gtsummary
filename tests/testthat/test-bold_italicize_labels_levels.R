context("test-tab_style")
testthat::skip_on_cran()

tbl <- trial %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  add_q()

test_that("tab_style: bold and italicize", {
  expect_error(
    tbl %>%
      bold_labels() %>%
      bold_levels() %>%
      italicize_labels() %>%
      italicize_levels() %>%
      bold_p() %>%
      bold_p(q = TRUE, t = 0.2),
    NA
  )
  expect_warning(
    tbl %>%
      bold_labels() %>%
      bold_levels() %>%
      italicize_labels() %>%
      italicize_levels() %>%
      bold_p() %>%
      bold_p(q = TRUE, t = 0.2),
    NA
  )
})


test_that("error when non-gtsummary object passed", {
  expect_error(
    mtcars %>%
      bold_labels(),
    "*"
  )
  expect_error(
    mtcars %>%
      bold_levels(),
    "*"
  )
  expect_error(
    mtcars %>%
      italicize_labels(),
    "*"
  )
  expect_error(
    mtcars %>%
      italicize_levels(),
    "*"
  )
})
