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

tbl <-
  trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p() |>
  modify_caption("**Table 1.** Patient characteristics") |>
  modify_source_note("Data from the trial dataset") |>
  modify_abbreviation("Q1 = First quartile")

ftbl <- as_flex_table(tbl)

# add_flex_footer_with_field() ------------------------------------------------
test_that("add_flex_footer_with_field() adds a field row with row-only alignment", {
  before <- flextable::nrow_part(ftbl, "footer")
  out <- add_flex_footer_with_field(ftbl, footnote = "Page {PAGE} of {NUMPAGES}")
  # exactly one new footer row is appended
  expect_equal(flextable::nrow_part(out, "footer"), before + 1L)

  # the row renders as live Word fields
  foot_only <- out |>
    flextable::delete_part(part = "header") |>
    flextable::delete_part(part = "body")
  foot_wml <- officer::to_wml(foot_only, add_ns = TRUE)
  expect_match(foot_wml, "PAGE")
  expect_match(foot_wml, "NUMPAGES")
  expect_match(foot_wml, "instrText")
})

test_that("add_flex_footer_with_field() validates footnote and align", {
  expect_error(add_flex_footer_with_field(mtcars), "flextable")
  expect_error(add_flex_footer_with_field(ftbl, footnote = 1L), "footnote")
  expect_error(add_flex_footer_with_field(ftbl, align = "up"))
})

# save_flex_docx() default behavior -------------------------------------------
test_that("save_flex_docx() writes a file and returns x invisibly", {
  path <- withr::local_tempfile(fileext = ".docx")

  expect_invisible(res <- save_flex_docx(tbl, path = path))
  expect_identical(res, tbl)
  expect_true(file.exists(path))
})

test_that("save_flex_docx() default moves notes to the Word footer with a page field", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path)

  footer <- read_docx_part(path, "footer")
  body <- read_docx_part(path, "body")

  # footer is a real Word table with the notes and a live PAGE/NUMPAGES field
  expect_match(footer, "<w:tbl")
  expect_match(footer, "Wilcoxon rank sum test")
  expect_match(footer, "Data from the trial dataset")
  expect_match(footer, "instrText")
  expect_match(footer, "PAGE")
  expect_match(footer, "NUMPAGES")

  # the footnote region is removed from the body, but the caption stays with it
  expect_no_match(body, "Wilcoxon rank sum test")
  expect_match(body, "Patient characteristics")

  # in-cell footnote reference symbols are retained on the table
  expect_match(body, "superscript")

  # the result is a valid Word document
  expect_s3_class(officer::read_docx(path), "rdocx")
})

test_that("save_flex_docx(body = NULL, footer = NULL) keeps everything in the body", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path, body = NULL, footer = NULL)

  body <- read_docx_part(path, "body")
  expect_match(body, "Patient characteristics")
  expect_match(body, "Wilcoxon rank sum test")
  expect_match(body, "Data from the trial dataset")

  # no footer region was created
  expect_false("word/footer1.xml" %in% unzip(path, list = TRUE)$Name)
})

test_that("save_flex_docx(header = NULL) default puts nothing in the Word header", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(tbl, path = path)
  expect_false("word/header1.xml" %in% unzip(path, list = TRUE)$Name)
})

test_that("save_flex_docx() works when the table has no caption/footnotes", {
  path <- withr::local_tempfile(fileext = ".docx")
  plain <- trial |> tbl_summary(include = age)

  expect_invisible(save_flex_docx(plain, path = path))
  expect_true(file.exists(path))
})

# custom transformers and static tables ---------------------------------------
test_that("save_flex_docx() accepts a static flextable header/footer", {
  path <- withr::local_tempfile(fileext = ".docx")
  hdr <- flextable::flextable(data.frame(x = "Confidential"))
  ftr <- flextable::flextable(data.frame(x = "Company Footer"))
  save_flex_docx(tbl, path = path, header = hdr, footer = ftr)

  expect_match(read_docx_part(path, "header"), "Confidential")
  expect_match(read_docx_part(path, "footer"), "Company Footer")
})

test_that("save_flex_docx() accepts custom body/header/footer transformers", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    tbl,
    path = path,
    body = \(x) flextable::delete_part(x, part = "footer"),
    header = \(x) flextable::delete_part(flextable::delete_part(x, part = "footer"), part = "body"),
    footer = \(x) {
      x |>
        flextable::delete_part(part = "header") |>
        flextable::delete_part(part = "body") |>
        add_flex_footer_with_field("Printed {DATE}", align = "center")
    }
  )

  footer <- read_docx_part(path, "footer")
  expect_match(footer, "DATE")
  expect_match(footer, "instrText")
  # header carries the extracted column labels
  expect_match(read_docx_part(path, "header"), "<w:tbl")
})

test_that("save_flex_docx() treats an empty footer result as no footer region", {
  path <- withr::local_tempfile(fileext = ".docx")
  # a plain flextable has no footer part; extracting it yields an empty flextable
  plain_ft <- flextable::flextable(head(trial[c("age", "grade")]))
  save_flex_docx(
    plain_ft,
    path = path,
    footer = \(x) {
      x |>
        flextable::delete_part(part = "header") |>
        flextable::delete_part(part = "body")
    }
  )
  expect_false("word/footer1.xml" %in% unzip(path, list = TRUE)$Name)
})

test_that("save_flex_docx() input checks", {
  path <- withr::local_tempfile(fileext = ".docx")

  # x must be a gtsummary/flextable object
  expect_error(save_flex_docx(mtcars, path = path), "gtsummary")

  # path is required and must be a string
  expect_error(save_flex_docx(tbl), "path")
  expect_error(save_flex_docx(tbl, path = 1L), "path")

  # body must be a function or NULL; header/footer a function, flextable, or NULL
  expect_error(save_flex_docx(tbl, path = path, body = "x"), "body")
  expect_error(save_flex_docx(tbl, path = path, header = "x"), "header")
  expect_error(save_flex_docx(tbl, path = path, footer = 1L), "footer")

  # dots must be empty
  expect_error(save_flex_docx(tbl, path = path, not_an_arg = TRUE))
})

# flextable input -------------------------------------------------------------
test_that("save_flex_docx() accepts a flextable and returns it invisibly", {
  path <- withr::local_tempfile(fileext = ".docx")
  ft <- as_flex_table(tbl) |> flextable::set_caption("Flextable Caption")

  expect_invisible(res <- save_flex_docx(ft, path = path))
  expect_identical(res, ft)
  expect_true(file.exists(path))

  # default: footer moved to the Word footer, caption stays in body
  expect_match(read_docx_part(path, "footer"), "Wilcoxon rank sum test")
  expect_no_match(read_docx_part(path, "body"), "Wilcoxon rank sum test")
})

# collections -----------------------------------------------------------------
split_tbl <-
  trial |>
  tbl_summary(by = trt, include = c(age, marker, grade)) |>
  modify_source_note("Data from the trial dataset")
split_obj <- tbl_split_by_rows(split_tbl, variables = c(age, marker))

test_that("save_flex_docx() writes a tbl_split as one doc with a section per table", {
  path <- withr::local_tempfile(fileext = ".docx")

  expect_invisible(res <- save_flex_docx(split_obj, path = path))
  expect_identical(res, split_obj)

  body <- read_docx_part(path, "body")
  n_tables <- length(split_obj)
  expect_equal(length(gregexpr("<w:tbl ", body)[[1]]), n_tables)
  expect_equal(length(gregexpr("<w:sectPr", body)[[1]]), n_tables)
  expect_equal(length(gregexpr("nextPage", body)[[1]]), n_tables)
  expect_false(grepl('w:type="page"', body))
  expect_match(body, "</w:sectPr>\\s*</w:body>")
})

test_that("save_flex_docx(tbl_split) applies the footer transformer per section", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(split_obj, path = path)

  parts <- unzip(path, list = TRUE)$Name
  footer_parts <- grep("word/footer[0-9]+\\.xml", parts, value = TRUE)
  expect_gte(length(footer_parts), length(split_obj))

  # every section's footer carries the source note and the page field
  expect_true(all(vapply(
    footer_parts,
    \(f) grepl("Data from the trial dataset", read_docx_file(path, f)) &&
      grepl("PAGE", read_docx_file(path, f)),
    logical(1)
  )))
})

test_that("save_flex_docx() length-1 tbl_split works", {
  spl1 <- structure(list(split_tbl), class = c("tbl_split", "list"))
  path <- withr::local_tempfile(fileext = ".docx")
  expect_invisible(save_flex_docx(spl1, path = path))
  body <- read_docx_part(path, "body")
  expect_equal(length(gregexpr("<w:tbl ", body)[[1]]), 1L)
})

test_that("save_flex_docx() writes a list of flextables, one section per table", {
  path <- withr::local_tempfile(fileext = ".docx")
  lst <- list(ftbl, ftbl)
  expect_invisible(res <- save_flex_docx(lst, path = path))
  expect_identical(res, lst)

  body <- read_docx_part(path, "body")
  expect_equal(length(gregexpr("<w:tbl ", body)[[1]]), length(lst))
  expect_equal(length(gregexpr("<w:sectPr", body)[[1]]), length(lst))
})

test_that("save_flex_docx() errors on a bare list, empty collection, non-flextable list", {
  path <- withr::local_tempfile(fileext = ".docx")

  # a plain list of gtsummary tables is not accepted
  expect_error(save_flex_docx(list(split_tbl, split_tbl), path = path), "tbl_split")

  # an empty tbl_split has nothing to write
  empty_split <- structure(list(), class = c("tbl_split", "list"))
  expect_error(save_flex_docx(empty_split, path = path), "empty")

  # an empty list and a list mixing flextables and other objects are rejected
  expect_error(save_flex_docx(list(), path = path), "flextable")
  expect_error(save_flex_docx(list(ftbl, mtcars), path = path), "flextable")
})

# styling preserved in the relocated footer -----------------------------------
test_that("save_flex_docx() preserves footer styling in the Word footer flextable", {
  # a footer fontsize set on the flextable is reflected in the Word footer
  # (6pt -> 12 half-points)
  ft <- as_flex_table(tbl) |> flextable::fontsize(size = 6, part = "footer")
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(ft, path = path)
  expect_match(read_docx_part(path, "footer"), "w:sz w:val=\"12\"")
})

# pr_section (fine-grained Word section control) ------------------------------
test_that("save_flex_docx(pr_section) applies custom page margins", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    tbl,
    path = path,
    pr_section = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5, bottom = 0.5)
    )
  )
  body <- read_docx_part(path, "body")
  # 0.5in = 720 twips
  expect_match(body, "w:top=\"720\"")
  expect_match(body, "w:bottom=\"720\"")
})

test_that("save_flex_docx(pr_section) header/footer defaults are ignored", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    tbl,
    path = path,
    pr_section = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5),
      footer_default = officer::block_list(officer::fpar(officer::ftext("USER FOOTER")))
    )
  )
  # our footer wins; the user's footer_default is dropped
  expect_no_match(read_docx_part(path, "footer"), "USER FOOTER")
  expect_match(read_docx_part(path, "footer"), "Data from the trial dataset")
})

test_that("save_flex_docx(pr_section) applies even with no header/footer content", {
  path <- withr::local_tempfile(fileext = ".docx")
  save_flex_docx(
    tbl,
    path = path,
    footer = NULL,
    pr_section = officer::prop_section(page_margins = officer::page_mar(top = 0.5))
  )
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
    pr_section = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5),
      type = "continuous"
    )
  )
  body <- read_docx_part(path, "body")
  n_tables <- length(split_obj)
  expect_equal(length(gregexpr("nextPage", body)[[1]]), n_tables)
  expect_no_match(body, "w:val=\"continuous\"")
  expect_equal(length(gregexpr("w:top=\"720\"", body)[[1]]), n_tables)
})

test_that("save_flex_docx-lst:pr_section theme element applies and the argument overrides it", {
  path <- withr::local_tempfile(fileext = ".docx")
  with_gtsummary_theme(
    list("save_flex_docx-lst:pr_section" = officer::prop_section(
      page_margins = officer::page_mar(top = 0.5)
    )),
    save_flex_docx(tbl, path = path)
  )
  expect_match(read_docx_part(path, "body"), "w:top=\"720\"")

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
