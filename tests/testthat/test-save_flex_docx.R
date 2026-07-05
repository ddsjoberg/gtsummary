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

test_that("save_flex_docx() writes a file and returns x invisibly", {
  path <- withr::local_tempfile(fileext = ".docx")

  expect_invisible(res <- save_flex_docx(tbl, path = path))
  expect_identical(res, tbl)
  expect_true(file.exists(path))
})

test_that("save_flex_docx(header = TRUE) places the caption in the Word header", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, header = TRUE, footer = FALSE)

  # caption text is in the header region, not the table body
  expect_match(read_docx_part(path, "header"), "Patient characteristics")
  expect_no_match(read_docx_part(path, "body"), "Patient characteristics")
})

test_that("save_flex_docx(footer = TRUE) places notes in the Word footer, keeping cell symbols", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, header = FALSE, footer = TRUE)

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

test_that("save_flex_docx(header = FALSE, footer = FALSE) keeps content in the table", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, header = FALSE, footer = FALSE)

  body <- read_docx_part(path, "body")
  expect_match(body, "Patient characteristics")
  expect_match(body, "Wilcoxon rank sum test")
  expect_match(body, "Data from the trial dataset")
})

test_that("save_flex_docx() works when the table has no caption/footnotes", {
  path <- withr::local_tempfile(fileext = ".docx")
  plain <- trial |> tbl_summary(include = age)

  expect_invisible(save_flex_docx(plain, path = path))
  expect_true(file.exists(path))
})

test_that("save_flex_docx() input checks", {
  path <- withr::local_tempfile(fileext = ".docx")

  # x must be a gtsummary object
  expect_error(save_flex_docx(mtcars, path = path), "gtsummary")

  # path is required and must be a string
  expect_error(save_flex_docx(tbl), "path")
  expect_error(save_flex_docx(tbl, path = 1L), "path")

  # header/footer must be scalar logicals
  expect_error(save_flex_docx(tbl, path = path, header = "yes"))
  expect_error(save_flex_docx(tbl, path = path, footer = c(TRUE, FALSE)))

  # dots must be empty
  expect_error(save_flex_docx(tbl, path = path, not_an_arg = TRUE))
})

test_that("save_flex_docx(page) adds live PAGE/NUMPAGES fields to the footer", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, page = "Page {PAGE} of {NUMPAGES}")

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

test_that("save_flex_docx(page_location) honors region and alignment", {
  # header-center
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, page = "P {PAGE}", page_location = "header-center")
  expect_match(read_docx_part(path, "header"), "PAGE")
  expect_match(read_docx_part(path, "header"), "center")

  # footer-left
  path2 <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path2, page = "P {PAGE}", page_location = "footer-left")
  footer <- read_docx_part(path2, "footer")
  expect_match(footer, "PAGE")
  expect_match(footer, "left|start")
})

test_that("save_flex_docx(page) coexists with caption/notes as a separate line", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
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

test_that("save_flex_docx(page) is added even when header/footer = FALSE", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    tbl,
    path = path,
    header = FALSE, footer = FALSE,
    page = "Page {PAGE} of {NUMPAGES}", page_location = "header-right"
  )
  expect_match(read_docx_part(path, "header"), "PAGE")
})

test_that("save_flex_docx(page) errors on invalid placeholders and locations", {
  path <- withr::local_tempfile(fileext = ".docx")

  # invalid placeholder token
  expect_error(
    save_flex_docx(tbl, path = path, page = "Page {x} of {y}"),
    "invalid placeholder"
  )
  # invalid page_location
  expect_error(
    save_flex_docx(tbl, path = path, page = "Page {PAGE}", page_location = "middle"),
    class = "rlang_error"
  )
})

# tbl_split -------------------------------------------------------------------
split_tbl <-
  trial |>
  tbl_summary(by = trt, include = c(age, marker, grade)) |>
  modify_source_note("Data from the trial dataset")
split_obj <- tbl_split_by_rows(split_tbl, variables = c(age, marker))

test_that("save_flex_docx() writes a tbl_split as one doc with a section per table", {
  path <- withr::local_tempfile(fileext = ".docx")

  expect_invisible(res <- save_flex_docx(split_obj, path = path))
  expect_identical(res, split_obj)
  expect_true(file.exists(path))

  body <- read_docx_part(path, "body")
  n_tables <- length(split_obj)
  # one <w:tbl per split table
  expect_equal(length(gregexpr("<w:tbl ", body)[[1]]), n_tables)
  # one section (`<w:sectPr>`) per table: tables 1..(N-1) end with a block
  # section, and the last table becomes the document's default section. no
  # trailing empty section/paragraph is added (that rendered as a blank final
  # page).
  expect_equal(length(gregexpr("<w:sectPr", body)[[1]]), n_tables)
  expect_equal(length(gregexpr("nextPage", body)[[1]]), n_tables)
  # no explicit page breaks are added (that would insert a blank page between
  # tables).
  expect_false(grepl('w:type="page"', body))
  # the document ends immediately after the final section (no trailing blank
  # paragraph before the closing body tag).
  expect_match(body, "</w:sectPr>\\s*</w:body>")
})

test_that("save_flex_docx(tbl_split) puts each table's notes in its own section footer", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(split_obj, path = path, header = FALSE, footer = TRUE)

  # each table gets its own footer part carrying the source note
  parts <- unzip(path, list = TRUE)$Name
  footer_parts <- grep("word/footer[0-9]+\\.xml", parts, value = TRUE)
  expect_gte(length(footer_parts), length(split_obj))

  con <- unz(path, footer_parts[[1]])
  on.exit(close(con))
  footer1 <- paste(readLines(con, warn = FALSE), collapse = "")
  expect_match(footer1, "Data from the trial dataset")
})

test_that("save_flex_docx(tbl_split) shows a row-scoped footnote only in the section that has those rows", {
  tbl <-
    tbl_summary(
      trial,
      by = trt,
      include = c(age, grade),
      type = age ~ "continuous2"
    ) |>
    modify_caption("This is my title") |>
    modify_footnote_body("my label", columns = "label", rows = variable == "age")

  spl <- tbl_split_by_rows(tbl, variables = "age")

  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(spl, path = path, header = TRUE, footer = TRUE)

  read_part <- function(p, f) {
    con <- unz(p, f)
    on.exit(close(con))
    paste(readLines(con, warn = FALSE), collapse = "")
  }
  parts <- unzip(path, list = TRUE)$Name
  footer_parts <- sort(grep("word/footer[0-9]+\\.xml", parts, value = TRUE))

  # first section (age rows) carries the row-scoped footnote; the second
  # section (grade only) must not, since the referenced rows are absent there.
  expect_match(read_part(path, footer_parts[[1]]), "my label")
  expect_no_match(read_part(path, footer_parts[[2]]), "my label")
})

test_that("save_flex_docx(tbl_split) maps each table's caption to its own section header", {
  spl <- tbl_split_by_rows(split_tbl, variables = c(age, marker), caption = "all")
  spl[[1]] <- modify_caption(spl[[1]], "CAPTION ALPHA")
  spl[[2]] <- modify_caption(spl[[2]], "CAPTION BETA")

  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(spl, path = path, header = TRUE, footer = FALSE)

  read_part <- function(p, f) {
    con <- unz(p, f)
    on.exit(close(con))
    paste(readLines(con, warn = FALSE), collapse = "")
  }
  headers <- sort(grep("word/header[0-9]+\\.xml", unzip(path, list = TRUE)$Name, value = TRUE))
  expect_match(read_part(path, headers[[1]]), "CAPTION ALPHA")
  expect_match(read_part(path, headers[[2]]), "CAPTION BETA")
})

test_that("save_flex_docx(tbl_split, page) adds the page line to each section", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(split_obj, path = path, page = "Page {PAGE} of {NUMPAGES}")

  con <- unz(path, "word/footer1.xml")
  on.exit(close(con))
  footer1 <- paste(readLines(con, warn = FALSE), collapse = "")
  expect_match(footer1, "PAGE")
  expect_match(footer1, "NUMPAGES")
})

test_that("save_flex_docx(tbl_split, header/footer = FALSE) keeps content in the body", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(split_obj, path = path, header = FALSE, footer = FALSE)

  body <- read_docx_part(path, "body")
  expect_match(body, "Data from the trial dataset")
})

test_that("save_flex_docx() length-1 tbl_split works", {
  spl1 <- tbl_split_by_rows(split_tbl, variables = age, row_numbers = integer(0))
  # ensure a single-element split
  spl1 <- structure(list(split_tbl), class = c("tbl_split", "list"))

  path <- withr::local_tempfile(fileext = ".docx")
  expect_invisible(save_flex_docx(spl1, path = path))
  expect_true(file.exists(path))
  body <- read_docx_part(path, "body")
  expect_equal(length(gregexpr("<w:tbl ", body)[[1]]), 1L)
})

test_that("save_flex_docx() errors on a bare list and an empty tbl_split", {
  path <- withr::local_tempfile(fileext = ".docx")

  # a plain list of gtsummary tables is not accepted
  expect_error(
    save_flex_docx(list(split_tbl, split_tbl), path = path),
    "tbl_split"
  )
  # an empty tbl_split has nothing to write
  empty_split <- structure(list(), class = c("tbl_split", "list"))
  expect_error(save_flex_docx(empty_split, path = path), "empty")
})

test_that("save_flex_docx() header/footer text matches the flextable body font", {
  # set non-default flextable body font; the Word header/footer should follow it
  old <- flextable::set_flextable_defaults(font.family = "Times New Roman", font.size = 9)
  withr::defer(do.call(flextable::set_flextable_defaults, old))

  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, header = TRUE, footer = TRUE)

  header <- read_docx_part(path, "header")
  footer <- read_docx_part(path, "footer")

  # caption (header) and notes (footer) carry the set font family
  expect_match(header, "w:ascii=\"Times New Roman\"")
  expect_match(footer, "w:ascii=\"Times New Roman\"")

  # and the set font size (points -> half-points: 9 -> 18)
  expect_match(header, "w:sz w:val=\"18\"")
  expect_match(footer, "w:sz w:val=\"18\"")
})

test_that("save_flex_docx(page) line matches the flextable body font, fields included", {
  old <- flextable::set_flextable_defaults(font.family = "Times New Roman", font.size = 9)
  withr::defer(do.call(flextable::set_flextable_defaults, old))

  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, page = "Page {PAGE} of {NUMPAGES}")

  footer <- read_docx_part(path, "footer")

  # the page line (including the PAGE/NUMPAGES field runs) uses the body font
  expect_match(footer, "PAGE")
  expect_match(footer, "w:ascii=\"Times New Roman\"")
  expect_match(footer, "w:sz w:val=\"18\"")
})

test_that("save_flex_docx(tbl_split) header/footer matches the flextable body font", {
  old <- flextable::set_flextable_defaults(font.family = "Times New Roman", font.size = 9)
  withr::defer(do.call(flextable::set_flextable_defaults, old))

  spl <- tbl_split_by_rows(split_tbl, variables = c(age, marker), caption = "all")
  spl[[1]] <- modify_caption(spl[[1]], "CAPTION ALPHA")
  spl[[2]] <- modify_caption(spl[[2]], "CAPTION BETA")

  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(spl, path = path, header = TRUE, footer = TRUE)

  read_part <- function(p, f) {
    con <- unz(p, f)
    on.exit(close(con))
    paste(readLines(con, warn = FALSE), collapse = "")
  }
  parts <- unzip(path, list = TRUE)$Name
  header_parts <- grep("word/header[0-9]+\\.xml", parts, value = TRUE)
  footer_parts <- grep("word/footer[0-9]+\\.xml", parts, value = TRUE)

  # every section's header/footer follows the body font
  expect_true(all(vapply(
    header_parts, \(f) grepl("w:ascii=\"Times New Roman\"", read_part(path, f)), logical(1)
  )))
  expect_true(all(vapply(
    footer_parts, \(f) grepl("w:ascii=\"Times New Roman\"", read_part(path, f)), logical(1)
  )))
})

# flextable input ------------------------------------------------------------
ftbl <-
  as_flex_table(tbl) |>
  flextable::set_caption("Flextable Caption")

test_that("save_flex_docx() accepts a flextable and returns it invisibly", {
  path <- withr::local_tempfile(fileext = ".docx")

  expect_invisible(res <- save_flex_docx(ftbl, path = path))
  expect_identical(res, ftbl)
  expect_true(file.exists(path))
})

test_that("save_flex_docx(flextable) relocates the caption to the Word header", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ftbl, path = path, header = TRUE, footer = FALSE)

  # the flextable caption moves to the header region, not the table body
  expect_match(read_docx_part(path, "header"), "Flextable Caption")
  expect_no_match(read_docx_part(path, "body"), "Flextable Caption")
})

test_that("save_flex_docx(flextable) relocates footer lines to the Word footer", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ftbl, path = path, header = FALSE, footer = TRUE)

  footer <- read_docx_part(path, "footer")
  body <- read_docx_part(path, "body")

  # footnote text and source note appear in the footer, not the body
  expect_match(footer, "Wilcoxon rank sum test")
  expect_match(footer, "Data from the trial dataset")
  expect_no_match(body, "Wilcoxon rank sum test")

  # in-cell footnote reference symbols are retained on the table
  expect_match(body, "superscript")
})

test_that("save_flex_docx(flextable, header = FALSE, footer = FALSE) keeps content in the table", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ftbl, path = path, header = FALSE, footer = FALSE)

  body <- read_docx_part(path, "body")
  expect_match(body, "Flextable Caption")
  expect_match(body, "Wilcoxon rank sum test")
  expect_match(body, "Data from the trial dataset")
})

test_that("save_flex_docx() works on a flextable with no caption/footer", {
  path <- withr::local_tempfile(fileext = ".docx")
  plain <- as_flex_table(trial |> tbl_summary(include = age)) |>
    flextable::delete_part(part = "footer")

  expect_invisible(save_flex_docx(plain, path = path))
  expect_true(file.exists(path))
})

# list of flextables ----------------------------------------------------------
test_that("save_flex_docx() writes a list of flextables as one section per table", {
  path <- withr::local_tempfile(fileext = ".docx")
  lst <- list(ftbl, ftbl)

  expect_invisible(res <- save_flex_docx(lst, path = path))
  expect_identical(res, lst)
  expect_true(file.exists(path))

  body <- read_docx_part(path, "body")
  # one <w:tbl per flextable and one section per table
  expect_equal(length(gregexpr("<w:tbl ", body)[[1]]), length(lst))
  expect_equal(length(gregexpr("<w:sectPr", body)[[1]]), length(lst))
})

test_that("save_flex_docx(list) puts each flextable's caption/notes in its own section", {
  path <- withr::local_tempfile(fileext = ".docx")
  ft_a <- as_flex_table(tbl) |> flextable::set_caption("CAPTION ALPHA")
  ft_b <- as_flex_table(tbl) |> flextable::set_caption("CAPTION BETA")
  save_flex_docx(list(ft_a, ft_b), path = path, header = TRUE, footer = FALSE)

  read_part <- function(p, f) {
    con <- unz(p, f)
    on.exit(close(con))
    paste(readLines(con, warn = FALSE), collapse = "")
  }
  headers <- sort(grep("word/header[0-9]+\\.xml", unzip(path, list = TRUE)$Name, value = TRUE))
  expect_match(read_part(path, headers[[1]]), "CAPTION ALPHA")
  expect_match(read_part(path, headers[[2]]), "CAPTION BETA")
})

test_that("save_flex_docx() errors on an empty list and a non-flextable list", {
  path <- withr::local_tempfile(fileext = ".docx")

  # an empty list has nothing to write
  expect_error(save_flex_docx(list(), path = path), "flextable")

  # a list mixing flextables and other objects is rejected
  expect_error(
    save_flex_docx(list(ftbl, mtcars), path = path),
    "flextable"
  )
})

# page-number line -----------------------------------------------------------
test_that("save_flex_docx(page) line adopts the resolved style of its region", {
  # a footer fontsize set on the flextable flows through to the page line placed
  # in the footer region (8pt -> 16 half-points)
  ft <- as_flex_table(tbl) |> flextable::fontsize(size = 8, part = "footer")
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    ft,
    path = path,
    page = "Page {PAGE} of {NUMPAGES}",
    page_location = "footer-right"
  )
  footer <- read_docx_part(path, "footer")
  expect_match(footer, "PAGE")
  expect_match(footer, "w:sz w:val=\"16\"")
})

# inherit styling from the flextable parts -----------------------------------
test_that("save_flex_docx() inherits footer styling from the flextable footer part", {
  # a footer fontsize set on the flextable is reflected in the Word footer
  # (6pt -> 12 half-points) and does not affect the header
  ft <- as_flex_table(tbl) |> flextable::fontsize(size = 6, part = "footer")
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ft, path = path)
  expect_match(read_docx_part(path, "footer"), "w:sz w:val=\"12\"")
  expect_no_match(read_docx_part(path, "header"), "w:sz w:val=\"12\"")
})

test_that("save_flex_docx() inherits header styling from the flextable header part", {
  ft <-
    as_flex_table(tbl) |>
    flextable::set_caption("Caption") |>
    flextable::fontsize(size = 20, part = "header")
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ft, path = path)
  expect_match(read_docx_part(path, "header"), "w:sz w:val=\"40\"")
})

test_that("save_flex_docx() inherits non-size properties from the flextable part", {
  ft <- as_flex_table(tbl) |> flextable::bold(part = "footer")
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ft, path = path)
  expect_match(read_docx_part(path, "footer"), "<w:b/>|<w:b ")
})

test_that("save_flex_docx(list) inherits footer styling per section", {
  ft <- as_flex_table(split_tbl) |> flextable::fontsize(size = 6, part = "footer")
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(list(ft, ft), path = path)

  read_part <- function(p, f) {
    con <- unz(p, f)
    on.exit(close(con))
    paste(readLines(con, warn = FALSE), collapse = "")
  }
  parts <- unzip(path, list = TRUE)$Name
  footer_parts <- grep("word/footer[0-9]+\\.xml", parts, value = TRUE)

  expect_gt(length(footer_parts), 0L)
  expect_true(all(vapply(
    footer_parts, \(f) grepl("w:sz w:val=\"12\"", read_part(path, f)), logical(1)
  )))
})
