test_that("tbl_butcher() works", {
  tbl_big <-
    trial %>%
    tbl_summary(include = c(marker, trt))

  expect_error(tbl_sml <- tbl_butcher(tbl_big), NA)
  expect_equal(tbl_sml, tbl_big[c("table_body", "table_styling")], ignore_attr = TRUE)
})