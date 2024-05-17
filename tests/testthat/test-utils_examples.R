test_that("writing and reading examples work", {
  skip_if_not_installed("htmltools")
  options(gtsummary_update_examples = TRUE)
  tbl_to_write <- trial |>
    select(age, grade, response) |>
    tbl_summary()

  # Direct html
  html_tbl <- tbl_to_write |>
    as_gt() |>
    gt::tab_options() |>
    gt::as_raw_html()
  write_example_output(html_tbl)
  read_html_tbl <- read_example_output("html_tbl")
  expect_equal(html_tbl, read_html_tbl)

  # One function
  html_output2 <- write_read_example_output(html_tbl)
  expect_equal(html_tbl, html_output2)

  # Starting from tbl (common behavior)
  write_example_output(tbl_to_write)
  read_html_tbl <- read_example_output("tbl_to_write")
  html_tbl2 <- strsplit(as.character(html_tbl), "\n")[[1]]
  read_html_tbl2 <- strsplit(as.character(read_html_tbl), "\n")[[1]]
  html_tbl2[1] <- paste0('<div id=\"\" style', strsplit(html_tbl2[1], "style")[[1]][2])
  read_html_tbl2[1] <- paste0('<div id=\"\" style', strsplit(read_html_tbl2[1], "style")[[1]][2])
  expect_equal(html_tbl2, read_html_tbl2)

  # Not impossible error
  expect_error(
    write_example_output("not_a_tbl_or_html"),
    "tbl needs to be a html, gtsummary or gt table. Got something else."
  )

  # Cleaning up
  options(gtsummary_update_examples = NULL)
  folder_name <- file.path(
    sub("(^.*/gtsummary).*", "\\1", getwd()),
    "inst",
    "example_outputs"
  )
  files <- file.path(
    folder_name,
    c("example_html_tbl.txt", "example_html_tbl.rds", "example_tbl_to_write.rds")
  )
  file.remove(files[file.exists(files)])
})
