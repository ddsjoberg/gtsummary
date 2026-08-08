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

# read a named part (e.g. "word/footer2.xml") from a written .docx
read_docx_file <- function(path, file) {
  con <- unz(path, file)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE), collapse = "")
}

# build a Word template with page furniture and bookmarks for the caption/notes.
# `caption_bkm`/`notes_bkm` name the bookmarks; set to NULL to omit that bookmark.
make_template <- function(path,
                          caption_bkm = "gtsummary_caption",
                          notes_bkm = "gtsummary_footnotes",
                          landscape = FALSE) {
  header_blocks <-
    if (!is.null(caption_bkm)) {
      officer::block_list(officer::fpar(
        officer::run_bookmark(caption_bkm, officer::ftext("[caption]"))
      ))
    } else {
      officer::block_list(officer::fpar(officer::ftext("TEMPLATE HEADER")))
    }
  footer_pieces <- list(
    officer::fpar(
      officer::ftext("ACME | Page "),
      officer::run_word_field("PAGE"),
      officer::ftext(" of "),
      officer::run_word_field("NUMPAGES")
    )
  )
  if (!is.null(notes_bkm)) {
    footer_pieces <- c(
      footer_pieces,
      list(officer::fpar(officer::run_bookmark(notes_bkm, officer::ftext("[notes]"))))
    )
  }
  section_args <- list(
    header_default = header_blocks,
    footer_default = do.call(officer::block_list, footer_pieces)
  )
  if (isTRUE(landscape)) {
    section_args$page_size <- officer::page_size(orient = "landscape")
  }
  doc <- officer::body_add_par(officer::read_docx(), "template body", style = "Normal")
  doc <- officer::body_set_default_section(doc, do.call(officer::prop_section, section_args))
  print(doc, target = path)
  path
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

test_that("save_flex_docx(header = TRUE) places a styled caption in the Word header", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, header = TRUE, footer = FALSE)

  header <- read_docx_part(path, "header")
  # caption text is in the header region, not the table body
  expect_match(header, "Patient characteristics")
  expect_no_match(read_docx_part(path, "body"), "Patient characteristics")
  # the markdown emphasis (`**Table 1.**`) is preserved as a bold run, not stripped
  expect_match(header, "<w:b/>|<w:b ")
})

test_that("save_flex_docx(footer = TRUE) places notes in the Word footer as a flextable", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, header = FALSE, footer = TRUE)

  footer <- read_docx_part(path, "footer")
  body <- read_docx_part(path, "body")

  # the footer content is rendered as a real Word table (flextable), not plain text
  expect_match(footer, "<w:tbl")

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

  # a non-existent template is rejected
  expect_error(
    save_flex_docx(tbl, path = path, template = tempfile(fileext = ".docx")),
    "template"
  )

  # dots must be empty
  expect_error(save_flex_docx(tbl, path = path, not_an_arg = TRUE))
})

# template --------------------------------------------------------------------
test_that("save_flex_docx(template) merges notes/caption into the template regions", {
  template <- make_template(withr::local_tempfile(fileext = ".docx"))
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, header = TRUE, footer = TRUE, template = template)

  footer <- read_docx_part(path, "footer")
  header <- read_docx_part(path, "header")

  # a single footer part holds BOTH the template page furniture and the notes
  parts <- unzip(path, list = TRUE)$Name
  expect_length(grep("word/footer[0-9]+\\.xml", parts, value = TRUE), 1L)

  # template page furniture (page field + boilerplate) survives
  expect_match(footer, "PAGE")
  expect_match(footer, "ACME")
  # the notes are injected as a flextable, and the placeholder is gone
  expect_match(footer, "<w:tbl")
  expect_match(footer, "Wilcoxon rank sum test")
  expect_no_match(footer, "\\[notes\\]")

  # the styled caption is injected into the template header; placeholder gone
  expect_match(header, "Patient characteristics")
  expect_no_match(header, "\\[caption\\]")

  # the result is a valid Word document
  expect_s3_class(officer::read_docx(path), "rdocx")
})

test_that("save_flex_docx(template) errors when the notes bookmark is absent", {
  template <- make_template(withr::local_tempfile(fileext = ".docx"), notes_bkm = NULL)
  path <- withr::local_tempfile(fileext = ".docx")
  expect_error(
    save_flex_docx(tbl, path = path, footer = TRUE, template = template),
    "footer_bookmark"
  )
})

test_that("save_flex_docx(template) errors when the caption bookmark is absent", {
  template <- make_template(withr::local_tempfile(fileext = ".docx"), caption_bkm = NULL)
  path <- withr::local_tempfile(fileext = ".docx")
  expect_error(
    save_flex_docx(tbl, path = path, header = TRUE, template = template),
    "header_bookmark"
  )
})

test_that("save_flex_docx(template, *_bookmark) honors custom bookmark names", {
  template <- make_template(
    withr::local_tempfile(fileext = ".docx"),
    caption_bkm = "cap_here",
    notes_bkm = "notes_here"
  )
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    tbl,
    path = path,
    header = TRUE,
    footer = TRUE,
    template = template,
    header_bookmark = "cap_here",
    footer_bookmark = "notes_here"
  )
  expect_match(read_docx_part(path, "footer"), "<w:tbl")
  expect_match(read_docx_part(path, "header"), "Patient characteristics")
})

test_that("save_flex_docx(template) preserves the template's page geometry", {
  template <- make_template(withr::local_tempfile(fileext = ".docx"), landscape = TRUE)
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, footer = TRUE, template = template)

  body <- read_docx_part(path, "body")
  # landscape orientation from the template carries through
  expect_match(body, "w:orient=\"landscape\"")
})

test_that("save_flex_docx(template) merges per section for a tbl_split", {
  template <- make_template(withr::local_tempfile(fileext = ".docx"))
  split_tbl <-
    tbl_summary(trial, by = trt, include = c(age, grade), type = age ~ "continuous2") |>
    modify_footnote_body("age note", columns = "label", rows = variable == "age") |>
    modify_source_note("SHARED SOURCE")
  spl <- tbl_split_by_rows(split_tbl, variables = "age", caption = "all")
  spl[[1]] <- modify_caption(spl[[1]], "CAPTION ALPHA")
  spl[[2]] <- modify_caption(spl[[2]], "CAPTION BETA")

  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(spl, path = path, header = TRUE, footer = TRUE, template = template)

  parts <- unzip(path, list = TRUE)$Name
  footers <- sort(grep("word/footer[0-9]+\\.xml", parts, value = TRUE))
  headers <- sort(grep("word/header[0-9]+\\.xml", parts, value = TRUE))
  # one referenced region per section; the template's originals are pruned
  expect_length(footers, length(spl))
  expect_length(headers, length(spl))

  ftxt <- vapply(footers, \(f) read_docx_file(path, f), character(1))
  htxt <- vapply(headers, \(f) read_docx_file(path, f), character(1))

  # every section footer repeats the template furniture and holds a notes table
  expect_true(all(grepl("PAGE", ftxt)))
  expect_true(all(grepl("ACME", ftxt)))
  expect_true(all(grepl("<w:tbl", ftxt)))
  # the row-scoped footnote appears in exactly one section
  expect_equal(sum(grepl("age note", ftxt)), 1L)
  # each caption lands in a section header; furniture repeats there too
  expect_true(any(grepl("CAPTION ALPHA", htxt)))
  expect_true(any(grepl("CAPTION BETA", htxt)))
  # no leftover template placeholder text anywhere
  expect_false(any(grepl("\\[notes\\]", ftxt)))
  expect_false(any(grepl("\\[caption\\]", htxt)))

  expect_s3_class(officer::read_docx(path), "rdocx")
})

test_that("save_flex_docx(template) collection errors when the notes bookmark is absent", {
  template <- make_template(withr::local_tempfile(fileext = ".docx"), notes_bkm = NULL)
  spl <- tbl_split_by_rows(
    tbl_summary(trial, by = trt, include = c(age, marker, grade)),
    variables = c(age, marker)
  )
  path <- withr::local_tempfile(fileext = ".docx")
  expect_error(
    save_flex_docx(spl, path = path, footer = TRUE, template = template),
    "footer_bookmark"
  )
})

test_that("save_flex_docx(template) collection with header/footer = FALSE repeats furniture, keeps notes in body", {
  template <- make_template(withr::local_tempfile(fileext = ".docx"))
  spl <- tbl_split_by_rows(
    tbl_summary(trial, by = trt, include = c(age, marker, grade)) |>
      modify_source_note("SHARED SOURCE"),
    variables = c(age, marker)
  )
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(spl, path = path, header = FALSE, footer = FALSE, template = template)

  parts <- unzip(path, list = TRUE)$Name
  footers <- sort(grep("word/footer[0-9]+\\.xml", parts, value = TRUE))
  ftxt <- vapply(footers, \(f) read_docx_file(path, f), character(1))
  # furniture repeats on every section, but no notes table is merged into it
  expect_true(all(grepl("PAGE", ftxt)))
  expect_false(any(grepl("<w:tbl", ftxt)))
  # the notes stay in the table body
  expect_match(read_docx_part(path, "body"), "SHARED SOURCE")
  expect_s3_class(officer::read_docx(path), "rdocx")
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

  # each table gets its own footer part carrying the source note, rendered as a
  # flextable
  parts <- unzip(path, list = TRUE)$Name
  footer_parts <- grep("word/footer[0-9]+\\.xml", parts, value = TRUE)
  expect_gte(length(footer_parts), length(split_obj))

  footer1 <- read_docx_file(path, footer_parts[[1]])
  expect_match(footer1, "Data from the trial dataset")
  expect_match(footer1, "<w:tbl")
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

  parts <- unzip(path, list = TRUE)$Name
  footer_parts <- sort(grep("word/footer[0-9]+\\.xml", parts, value = TRUE))

  # first section (age rows) carries the row-scoped footnote; the second
  # section (grade only) must not, since the referenced rows are absent there.
  expect_match(read_docx_file(path, footer_parts[[1]]), "my label")
  expect_no_match(read_docx_file(path, footer_parts[[2]]), "my label")
})

test_that("save_flex_docx(tbl_split) maps each table's caption to its own section header", {
  spl <- tbl_split_by_rows(split_tbl, variables = c(age, marker), caption = "all")
  spl[[1]] <- modify_caption(spl[[1]], "CAPTION ALPHA")
  spl[[2]] <- modify_caption(spl[[2]], "CAPTION BETA")

  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(spl, path = path, header = TRUE, footer = FALSE)

  headers <- sort(grep("word/header[0-9]+\\.xml", unzip(path, list = TRUE)$Name, value = TRUE))
  expect_match(read_docx_file(path, headers[[1]]), "CAPTION ALPHA")
  expect_match(read_docx_file(path, headers[[2]]), "CAPTION BETA")
})

test_that("save_flex_docx(tbl_split, header/footer = FALSE) keeps content in the body", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(split_obj, path = path, header = FALSE, footer = FALSE)

  body <- read_docx_part(path, "body")
  expect_match(body, "Data from the trial dataset")
})

test_that("save_flex_docx() length-1 tbl_split works", {
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

# font --------------------------------------------------------------------
test_that("save_flex_docx() caption/notes match the flextable body font", {
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

test_that("save_flex_docx(tbl_split) header/footer matches the flextable body font", {
  old <- flextable::set_flextable_defaults(font.family = "Times New Roman", font.size = 9)
  withr::defer(do.call(flextable::set_flextable_defaults, old))

  spl <- tbl_split_by_rows(split_tbl, variables = c(age, marker), caption = "all")
  spl[[1]] <- modify_caption(spl[[1]], "CAPTION ALPHA")
  spl[[2]] <- modify_caption(spl[[2]], "CAPTION BETA")

  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(spl, path = path, header = TRUE, footer = TRUE)

  parts <- unzip(path, list = TRUE)$Name
  header_parts <- grep("word/header[0-9]+\\.xml", parts, value = TRUE)
  footer_parts <- grep("word/footer[0-9]+\\.xml", parts, value = TRUE)

  # every section's header/footer follows the body font
  expect_true(all(vapply(
    header_parts, \(f) grepl("w:ascii=\"Times New Roman\"", read_docx_file(path, f)), logical(1)
  )))
  expect_true(all(vapply(
    footer_parts, \(f) grepl("w:ascii=\"Times New Roman\"", read_docx_file(path, f)), logical(1)
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

test_that("save_flex_docx(flextable) relocates footer lines to the Word footer as a flextable", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ftbl, path = path, header = FALSE, footer = TRUE)

  footer <- read_docx_part(path, "footer")
  body <- read_docx_part(path, "body")

  # footer rendered as a flextable, footnote text and source note in the footer
  expect_match(footer, "<w:tbl")
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

  headers <- sort(grep("word/header[0-9]+\\.xml", unzip(path, list = TRUE)$Name, value = TRUE))
  expect_match(read_docx_file(path, headers[[1]]), "CAPTION ALPHA")
  expect_match(read_docx_file(path, headers[[2]]), "CAPTION BETA")
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

# preserve styling from the flextable footer part -----------------------------
test_that("save_flex_docx() preserves footer styling in the Word footer flextable", {
  # a footer fontsize set on the flextable is reflected in the Word footer
  # (6pt -> 12 half-points)
  ft <- as_flex_table(tbl) |> flextable::fontsize(size = 6, part = "footer")
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ft, path = path, footer = TRUE)
  expect_match(read_docx_part(path, "footer"), "w:sz w:val=\"12\"")
})

test_that("save_flex_docx() preserves non-size footer styling in the Word footer", {
  ft <- as_flex_table(tbl) |> flextable::bold(part = "footer")
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ft, path = path, footer = TRUE)
  expect_match(read_docx_part(path, "footer"), "<w:b/>|<w:b ")
})

test_that("save_flex_docx(list) preserves footer styling per section", {
  ft <- as_flex_table(split_tbl) |> flextable::fontsize(size = 6, part = "footer")
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(list(ft, ft), path = path, footer = TRUE)

  parts <- unzip(path, list = TRUE)$Name
  footer_parts <- grep("word/footer[0-9]+\\.xml", parts, value = TRUE)

  expect_gt(length(footer_parts), 0L)
  expect_true(all(vapply(
    footer_parts, \(f) grepl("w:sz w:val=\"12\"", read_docx_file(path, f)), logical(1)
  )))
})

# pr_section (fine-grained Word section control) ------------------------------
test_that("save_flex_docx(pr_section) applies custom page margins, keeps caption/notes relocated", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    tbl,
    path = path,
    header = TRUE,
    footer = TRUE,
    pr_section = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5, bottom = 0.5)
    )
  )

  body <- read_docx_part(path, "body")
  # 0.5in = 720 twips
  expect_match(body, "w:top=\"720\"")
  expect_match(body, "w:bottom=\"720\"")
  # caption still in header, notes still in footer
  expect_match(read_docx_part(path, "header"), "Patient characteristics")
  expect_match(read_docx_part(path, "footer"), "Data from the trial dataset")
})

test_that("save_flex_docx(pr_section) header/footer defaults are overridden by ours", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    tbl,
    path = path,
    header = TRUE,
    footer = TRUE,
    pr_section = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5),
      header_default = officer::block_list(officer::fpar(officer::ftext("USER HEADER"))),
      footer_default = officer::block_list(officer::fpar(officer::ftext("USER FOOTER")))
    )
  )

  # the function's relocated caption/notes win; the user's header/footer are dropped
  expect_match(read_docx_part(path, "header"), "Patient characteristics")
  expect_no_match(read_docx_part(path, "header"), "USER HEADER")
  expect_match(read_docx_part(path, "footer"), "Data from the trial dataset")
  expect_no_match(read_docx_part(path, "footer"), "USER FOOTER")
})

test_that("save_flex_docx(pr_section) applies even when there is no caption/footer content", {
  path <- withr::local_tempfile(fileext = ".docx")
  plain <- trial |>
    tbl_summary(include = age) |>
    remove_footnote_header(columns = everything())
  save_flex_docx(
    plain,
    path = path,
    header = FALSE,
    footer = FALSE,
    pr_section = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5)
    )
  )
  # the section (and its margins) is written even with no header/footer content
  expect_match(read_docx_part(path, "body"), "w:top=\"720\"")
})

test_that("save_flex_docx(pr_section) validates the class", {
  path <- withr::local_tempfile(fileext = ".docx")
  expect_error(
    save_flex_docx(tbl, path = path, pr_section = list(page_margins = 1)),
    "prop_section"
  )
})

test_that("save_flex_docx(tbl_split, pr_section) applies to every section, forcing nextPage", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    split_obj,
    path = path,
    # user sets `type = "continuous"`, which must be ignored between tables
    pr_section = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5),
      type = "continuous"
    )
  )

  body <- read_docx_part(path, "body")
  n_tables <- length(split_obj)
  # one section per table, all forced to nextPage (user's `continuous` ignored)
  expect_equal(length(gregexpr("<w:sectPr", body)[[1]]), n_tables)
  expect_equal(length(gregexpr("nextPage", body)[[1]]), n_tables)
  expect_no_match(body, "w:val=\"continuous\"")
  # every section carries the custom margin, and no blank page is introduced
  expect_equal(length(gregexpr("w:top=\"720\"", body)[[1]]), n_tables)
  expect_false(grepl("w:type=\"page\"", body))
  expect_match(body, "</w:sectPr>\\s*</w:body>")
})

test_that("save_flex_docx(list, pr_section) applies custom margins to every section", {
  path <- withr::local_tempfile(fileext = ".docx")
  ft <- as_flex_table(tbl) |> flextable::set_caption("Cap")
  save_flex_docx(
    list(ft, ft),
    path = path,
    pr_section = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5)
    )
  )
  body <- read_docx_part(path, "body")
  expect_equal(length(gregexpr("w:top=\"720\"", body)[[1]]), 2L)
})

test_that("save_flex_docx-lst:pr_section theme element applies and the argument overrides it", {
  # theme sets top margin 0.5in (720 twips)
  path <- withr::local_tempfile(fileext = ".docx")
  with_gtsummary_theme(
    list("save_flex_docx-lst:pr_section" = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5)
    )),
    save_flex_docx(tbl, path = path)
  )
  expect_match(read_docx_part(path, "body"), "w:top=\"720\"")

  # the argument (top = 1in = 1440 twips) overrides the theme
  path2 <- withr::local_tempfile(fileext = ".docx")
  with_gtsummary_theme(
    list("save_flex_docx-lst:pr_section" = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5)
    )),
    save_flex_docx(
      tbl,
      path = path2,
      pr_section = officer::prop_section(page_margins = officer::page_mar(top = 1))
    )
  )
  body2 <- read_docx_part(path2, "body")
  expect_match(body2, "w:top=\"1440\"")
  expect_no_match(body2, "w:top=\"720\"")
})
