context("test-tab_style")

tbl <- trial %>% tbl_summary(by = "trt") %>% add_comparison() %>% add_q()

test_that("tab_style: bold and italicize", {
  expect_error(
    tbl %>%
      tab_style_bold_labels() %>%
      tab_style_bold_levels() %>%
      tab_style_italicize_labels() %>%
      tab_style_italicize_levels() %>%
      tab_style_bold_p() %>%
      tab_style_bold_p(q = TRUE, t = 0.2),
    NA)
  expect_warning(
    tbl %>%
      tab_style_bold_labels() %>%
      tab_style_bold_levels() %>%
      tab_style_italicize_labels() %>%
      tab_style_italicize_levels() %>%
      tab_style_bold_p() %>%
      tab_style_bold_p(q = TRUE, t = 0.2),
    NA)
})
