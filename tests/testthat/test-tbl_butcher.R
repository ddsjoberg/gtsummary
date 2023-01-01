test_that("tbl_butcher() works", {
  tbl_big <-
    trial %>%
    tbl_summary(include = c(marker, trt))

  expect_error(tbl_sml <- tbl_butcher(tbl_big), NA)
  expect_snapshot(tbl_sml %>% render_as_html())
  expect_equal(tbl_sml, tbl_big[c("table_body", "table_styling")], ignore_attr = TRUE)
  expect_true(inherits(tbl_sml, "gtsummary"))
})
