context("test-show_header_names")

test_that("show_header_names works", {
  expect_error(
    trial %>% select(age) %>% tbl_summary() %>% show_header_names(),
    NA
  )
})
