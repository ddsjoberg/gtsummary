skip_on_cran()
skip_if_pkg_not_installed(c("flextable", "officer", "broom.helpers"))

# read a part (document/header/footer) from a written .docx as a single string
read_docx_part <- function(path, part) {
  file <- switch(part,
    body = "word/document.xml",
    header = "word/header1.xml",
    footer = "word/footer1.xml"
  )
  con <- unz(path, file)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE), collapse = "")
}

tbl <-
  trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p() |>
  modify_caption("**Table 1.** Patient characteristics") |>
  modify_source_note("Data from the trial dataset") |>
  modify_abbreviation("Q1 = First quartile")

test_that("as_flex_word() writes a file and returns x invisibly", {
  path <- withr::local_tempfile(fileext = ".docx")

  expect_invisible(res <- as_flex_word(tbl, path = path))
  expect_identical(res, tbl)
  expect_true(file.exists(path))
})

test_that("as_flex_word(header = TRUE) places the caption in the Word header", {
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(tbl, path = path, header = TRUE, footer = FALSE)

  # caption text is in the header region, not the table body
  expect_match(read_docx_part(path, "header"), "Patient characteristics")
  expect_no_match(read_docx_part(path, "body"), "Patient characteristics")
})

test_that("as_flex_word(footer = TRUE) places notes in the Word footer, keeping cell symbols", {
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(tbl, path = path, header = FALSE, footer = TRUE)

  footer <- read_docx_part(path, "footer")
  body <- read_docx_part(path, "body")

  # footnote text, source note, and abbreviation appear in the footer
  expect_match(footer, "Median")
  expect_match(footer, "Wilcoxon rank sum test")
  expect_match(footer, "Data from the trial dataset")
  expect_match(footer, "First quartile")

  # footnote text is not repeated in the table body
  expect_no_match(body, "Wilcoxon rank sum test")
  expect_no_match(body, "Data from the trial dataset")

  # in-cell footnote reference symbols are retained on the table
  expect_match(body, "superscript")
})

test_that("as_flex_word(header = FALSE, footer = FALSE) keeps content in the table", {
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(tbl, path = path, header = FALSE, footer = FALSE)

  body <- read_docx_part(path, "body")
  expect_match(body, "Patient characteristics")
  expect_match(body, "Wilcoxon rank sum test")
  expect_match(body, "Data from the trial dataset")
})

test_that("as_flex_word() works when the table has no caption/footnotes", {
  path <- withr::local_tempfile(fileext = ".docx")
  plain <- trial |> tbl_summary(include = age)

  expect_invisible(as_flex_word(plain, path = path))
  expect_true(file.exists(path))
})

test_that("as_flex_word() input checks", {
  path <- withr::local_tempfile(fileext = ".docx")

  # x must be a gtsummary object
  expect_error(as_flex_word(mtcars, path = path), "gtsummary")

  # path is required and must be a string
  expect_error(as_flex_word(tbl), "path")
  expect_error(as_flex_word(tbl, path = 1L), "path")

  # header/footer must be scalar logicals
  expect_error(as_flex_word(tbl, path = path, header = "yes"))
  expect_error(as_flex_word(tbl, path = path, footer = c(TRUE, FALSE)))

  # dots must be empty
  expect_error(as_flex_word(tbl, path = path, not_an_arg = TRUE))
})

test_that("as_flex_word(page) adds live PAGE/NUMPAGES fields to the footer", {
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(tbl, path = path, page = "Page {PAGE} of {NUMPAGES}")

  footer <- read_docx_part(path, "footer")
  # live Word fields are present
  expect_match(footer, "PAGE")
  expect_match(footer, "NUMPAGES")
  # literal text is present
  expect_match(footer, "Page")
  expect_match(footer, "of")
  # default location is footer-right
  expect_match(footer, "right|end")
})

test_that("as_flex_word(page_location) honors region and alignment", {
  # header-center
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(tbl, path = path, page = "P {PAGE}", page_location = "header-center")
  expect_match(read_docx_part(path, "header"), "PAGE")
  expect_match(read_docx_part(path, "header"), "center")

  # footer-left
  path2 <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(tbl, path = path2, page = "P {PAGE}", page_location = "footer-left")
  footer <- read_docx_part(path2, "footer")
  expect_match(footer, "PAGE")
  expect_match(footer, "left|start")
})

test_that("as_flex_word(page) coexists with caption/notes as a separate line", {
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(
    tbl,
    path = path,
    header = TRUE, footer = TRUE,
    page = "Page {PAGE}", page_location = "footer-right"
  )
  # notes and page fields both live in the footer
  footer <- read_docx_part(path, "footer")
  expect_match(footer, "Data from the trial dataset")
  expect_match(footer, "PAGE")
})

test_that("as_flex_word(page) is added even when header/footer = FALSE", {
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(
    tbl,
    path = path,
    header = FALSE, footer = FALSE,
    page = "Page {PAGE} of {NUMPAGES}", page_location = "header-right"
  )
  expect_match(read_docx_part(path, "header"), "PAGE")
})

test_that("as_flex_word(page) errors on invalid placeholders and locations", {
  path <- withr::local_tempfile(fileext = ".docx")

  # invalid placeholder token
  expect_error(
    as_flex_word(tbl, path = path, page = "Page {x} of {y}"),
    "invalid placeholder"
  )
  # invalid page_location
  expect_error(
    as_flex_word(tbl, path = path, page = "Page {PAGE}", page_location = "middle"),
    class = "rlang_error"
  )
})

# tbl_split -------------------------------------------------------------------
split_tbl <-
  trial |>
  tbl_summary(by = trt, include = c(age, marker, grade)) |>
  modify_source_note("Data from the trial dataset")
split_obj <- tbl_split_by_rows(split_tbl, variables = c(age, marker))

test_that("as_flex_word() writes a tbl_split as one doc with a section per table", {
  path <- withr::local_tempfile(fileext = ".docx")

  expect_invisible(res <- as_flex_word(split_obj, path = path))
  expect_identical(res, split_obj)
  expect_true(file.exists(path))

  body <- read_docx_part(path, "body")
  n_tables <- length(split_obj)
  # one <w:tbl per split table
  expect_equal(length(gregexpr("<w:tbl ", body)[[1]]), n_tables)
  # a page break between each table (N - 1)
  expect_equal(length(gregexpr('w:type="page"', body)[[1]]), n_tables - 1L)
})

test_that("as_flex_word(tbl_split) puts each table's notes in its own section footer", {
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(split_obj, path = path, header = FALSE, footer = TRUE)

  # each table gets its own footer part carrying the source note
  parts <- unzip(path, list = TRUE)$Name
  footer_parts <- grep("word/footer[0-9]+\\.xml", parts, value = TRUE)
  expect_gte(length(footer_parts), length(split_obj))

  con <- unz(path, footer_parts[[1]])
  on.exit(close(con))
  footer1 <- paste(readLines(con, warn = FALSE), collapse = "")
  expect_match(footer1, "Data from the trial dataset")
})

test_that("as_flex_word(tbl_split) maps each table's caption to its own section header", {
  spl <- tbl_split_by_rows(split_tbl, variables = c(age, marker), caption = "all")
  spl[[1]] <- modify_caption(spl[[1]], "CAPTION ALPHA")
  spl[[2]] <- modify_caption(spl[[2]], "CAPTION BETA")

  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(spl, path = path, header = TRUE, footer = FALSE)

  read_part <- function(p, f) {
    con <- unz(p, f)
    on.exit(close(con))
    paste(readLines(con, warn = FALSE), collapse = "")
  }
  headers <- sort(grep("word/header[0-9]+\\.xml", unzip(path, list = TRUE)$Name, value = TRUE))
  expect_match(read_part(path, headers[[1]]), "CAPTION ALPHA")
  expect_match(read_part(path, headers[[2]]), "CAPTION BETA")
})

test_that("as_flex_word(tbl_split, page) adds the page line to each section", {
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(split_obj, path = path, page = "Page {PAGE} of {NUMPAGES}")

  con <- unz(path, "word/footer1.xml")
  on.exit(close(con))
  footer1 <- paste(readLines(con, warn = FALSE), collapse = "")
  expect_match(footer1, "PAGE")
  expect_match(footer1, "NUMPAGES")
})

test_that("as_flex_word(tbl_split, header/footer = FALSE) keeps content in the body", {
  path <- withr::local_tempfile(fileext = ".docx")
  as_flex_word(split_obj, path = path, header = FALSE, footer = FALSE)

  body <- read_docx_part(path, "body")
  expect_match(body, "Data from the trial dataset")
})

test_that("as_flex_word() length-1 tbl_split works", {
  spl1 <- tbl_split_by_rows(split_tbl, variables = age, row_numbers = integer(0))
  # ensure a single-element split
  spl1 <- structure(list(split_tbl), class = c("tbl_split", "list"))

  path <- withr::local_tempfile(fileext = ".docx")
  expect_invisible(as_flex_word(spl1, path = path))
  expect_true(file.exists(path))
  body <- read_docx_part(path, "body")
  expect_equal(length(gregexpr("<w:tbl ", body)[[1]]), 1L)
})

test_that("as_flex_word() errors on a bare list and an empty tbl_split", {
  path <- withr::local_tempfile(fileext = ".docx")

  # a plain list of gtsummary tables is not accepted
  expect_error(
    as_flex_word(list(split_tbl, split_tbl), path = path),
    "tbl_split"
  )
  # an empty tbl_split has nothing to write
  empty_split <- structure(list(), class = c("tbl_split", "list"))
  expect_error(as_flex_word(empty_split, path = path), "empty")
})
