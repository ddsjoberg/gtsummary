skip_on_cran()

test_that("show_header_names works", {
  expect_error(
    trial %>% select(age) %>% tbl_summary() %>% show_header_names(quiet = TRUE),
    NA
  )
})
