test_that("as_hux_xlsx() works", {
  tbl <-
    trial %>%
    tbl_cross(grade, stage) %>%
    add_p(source_note = TRUE)

  expect_error(
    hux <- as_hux_xlsx(tbl, file = file.path(tempdir(), "testing.xlsx"), bold_header_rows = TRUE),
    NA
  )
  expect_error(
    hux <- as_hux_xlsx(tbl, file = file.path(tempdir(), "testing.xlsx"), bold_header_rows = FALSE),
    NA
  )
})
