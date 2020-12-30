context("test-modify_caption")
testthat::skip_on_cran()

test_that("no errors/warnings with all output types", {
  tbl <- trial %>% select(age) %>% tbl_summary() %>% modify_caption("test caption")

  expect_error(tbl %>% as_gt(), NA)
  expect_error(tbl %>% as_flex_table(), NA)
  expect_error(tbl %>% as_hux_table(), NA)
  expect_error(tbl %>% as_kable(), NA)
  expect_error(tbl %>% as_kable_extra(), NA)
  expect_error(tbl %>% as_tibble(), NA)

  tbl2 <- trial %>% select(age) %>% tbl_summary() %>%
    modify_caption("test caption", text_interpret = "html")
  expect_error(tbl2 %>% as_gt(), NA)

  tbl_reg <- lm(mpg ~ hp, mtcars) %>% tbl_regression()
  expect_error(modify_caption(trial), "*")
  expect_error(tbl_reg %>% modify_caption(letters), "*")
  expect_equal(
    tbl_reg %>% modify_caption("{N}") %>% purrr::pluck("list_output", "caption"),
    "32"
  )
})
