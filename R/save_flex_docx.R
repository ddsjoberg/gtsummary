#' Save a gtsummary table to a Word file
#'
#' @description
#' `r lifecycle::badge("experimental")`\cr
#' Save a gtsummary table or a flextable to a Word (`.docx`) file using the
#' flextable package.
#'
#' The `header` and `footer` arguments control where the table caption and the
#' footnote-region content (footnotes, source notes, and abbreviations) are
#' placed in the Word document. When `TRUE`, this content is moved to the Word
#' document's page **header** and **footer** regions (where it repeats on every
#' page) instead of appearing in the flow of the table itself. When `FALSE`, the
#' content is rendered as part of the table, matching [`as_flex_table()`].
#'
#' Relocated content keeps its styling: the caption is written to the header
#' with its markdown emphasis (`**bold**`, `_italic_`) preserved, and the
#' footnote-region content is written to the footer **as a flextable**, so
#' footnote reference symbols, emphasis, and per-cell styling are retained.
#'
#' Page-level furniture that is not part of the table (page numbers, dates,
#' logos, boilerplate) is supplied through a Word `template`: design the header
#' and footer in Word, mark where the caption and notes should go with Word
#' bookmarks, and `save_flex_docx()` merges the relocated caption/notes into
#' those regions while preserving everything else the template defines.
#'
#' A collection of tables is also accepted: a `tbl_split` object (from
#' [`tbl_split_by_rows()`] or [`tbl_split_by_columns()`]), or a plain list of
#' flextables. Each table is written to its own Word section so that each table's
#' caption and footnote-region content populate that section's own header/footer
#' regions, one table per page. When a `template` is supplied, its header/footer
#' furniture repeats on every section and each table's caption/notes are merged
#' into that section's own copy of the template regions.
#'
#' A `flextable` object (or a list of them) is also accepted. Its caption
#' (`flextable::set_caption()`) is relocated to the Word header and its footer
#' part (`flextable::add_footer_lines()`) to the Word footer, matching the
#' gtsummary behavior.
#'
#' @param x (`gtsummary`, `tbl_split`, `flextable`, or `list`)\cr
#'   a gtsummary table, a `tbl_split` object (a list of gtsummary tables), a
#'   `flextable` object, or a plain list of `flextable` objects
#' @param path (`string`)\cr
#'   file path to write the Word (`.docx`) file to
#' @param header (scalar `logical`)\cr
#'   whether to place the table caption in the Word document's page header
#'   region. When `FALSE`, the caption is rendered as the table caption. Default
#'   is `FALSE`.
#' @param footer (scalar `logical`)\cr
#'   whether to place the footnotes, source notes, and abbreviations in the Word
#'   document's page footer region (as a flextable). When `FALSE`, this content
#'   is rendered in the table's footer. Default is `FALSE`.
#' @param template (`string`)\cr
#'   an optional file path to a Word (`.docx`) document used as a template. The
#'   template's page header/footer regions (e.g. page numbers, dates, logos, or
#'   boilerplate) are carried through to the output. To place the relocated
#'   caption or notes inside the template's header/footer, add Word bookmarks
#'   named by `header_bookmark`/`footer_bookmark` (each in its own paragraph);
#'   the paragraph holding the bookmark is replaced by the caption/notes. For a
#'   collection (`tbl_split` or a list of flextables) the furniture repeats on
#'   every table's section, with each table's caption/notes merged into that
#'   section's own copy of the template regions. Default is `NULL`.
#' @param header_bookmark,footer_bookmark (`string`)\cr
#'   the names of the Word bookmarks in `template` at which the relocated caption
#'   (`header_bookmark`) and notes (`footer_bookmark`) are inserted. Only used
#'   when `template` is supplied. Defaults are `"gtsummary_caption"` and
#'   `"gtsummary_footnotes"`.
#' @param pr_section (`officer::prop_section`)\cr
#'   an optional [`officer::prop_section()`] object used as the base Word section,
#'   giving fine-grained control over page margins, page size, orientation, and
#'   section columns (e.g.
#'   `officer::prop_section(page_margins = officer::page_mar(top = 0.5))`). The
#'   section's header and footer regions are always managed by `save_flex_docx()`
#'   (the relocated caption and notes), so any `header_default`/`footer_default`
#'   set on `pr_section` are ignored. For a collection (`tbl_split` or a list of
#'   flextables) the same `pr_section` is applied to every table's section, and
#'   the paging `type` is fixed to `"nextPage"` (any `type` on `pr_section` is
#'   ignored) so tables page correctly. Overrides the
#'   `save_flex_docx-lst:pr_section` theme element. Default is `NULL`.
#' @param ... These dots are for future extensions and must be empty.
#'
#' @export
#' @return the original object `x` (invisibly)
#'
#' @seealso [`as_flex_table()`]
#'
#' @section Limitations:
#'
#' Relocating the caption to the Word header (`header = TRUE`) preserves the
#' `**bold**` and `_italic_` markdown emphasis gtsummary supports, but other
#' markdown/HTML is not interpreted. Relocating the notes to the Word footer
#' (`footer = TRUE`) renders them as a flextable, preserving footnote reference
#' symbols and cell styling. When a `template` is used, its header and footer
#' each hold a single shared region (there is no per-page-position variation).
#' For a collection with a `template`, page furniture that embeds images (e.g. a
#' logo) is repeated on every section, but the bookmarked caption/notes are
#' merged per section via a copy of the template regions.
#'
#' @examplesIf gtsummary:::is_pkg_installed(c("flextable", "officer"))
#' tbl <-
#'   trial |>
#'   tbl_summary(by = trt, include = c(age, grade)) |>
#'   modify_caption("**Table 1. Patient Characteristics**")
#'
#' # save the table, placing caption in the header and notes in the footer
#' save_flex_docx(tbl, path = tempfile(fileext = ".docx"), header = TRUE, footer = TRUE)
#'
#' # a split table is written with one table per section/page
#' trial |>
#'   tbl_summary(by = trt, include = c(age, marker, grade), missing = ~"no") |>
#'   modify_footnote_body(
#'     "Footnotes only appear on the pages where the mark is present",
#'     columns = "label",
#'     rows = label == "Age"
#'   ) |>
#'   tbl_split_by_rows(variables = marker) |>
#'   save_flex_docx(path = tempfile(fileext = ".docx"), footer = TRUE)
#'
#' # a flextable (or a list of flextables) is also accepted
#' ft <-
#'   as_flex_table(tbl) |>
#'   flextable::set_caption("Table 1")
#' save_flex_docx(ft, path = tempfile(fileext = ".docx"))
#'
#' # customize the Word page margins and orientation via a prop_section()
#' save_flex_docx(
#'   tbl,
#'   path = tempfile(fileext = ".docx"),
#'   pr_section = officer::prop_section(
#'     page_margins = officer::page_mar(top = 0.5, bottom = 0.5),
#'     page_size = officer::page_size(orient = "landscape")
#'   )
#' )
save_flex_docx <- function(x,
                           path,
                           header = FALSE,
                           footer = FALSE,
                           template = NULL,
                           header_bookmark = "gtsummary_caption",
                           footer_bookmark = "gtsummary_footnotes",
                           pr_section = NULL,
                           ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_dots_empty()
  check_not_missing(x)
  check_not_missing(path)
  # accepted: a gtsummary table, a tbl_split, a flextable, or a plain list of
  # flextables. a tbl_split is itself a list but is matched by its class first.
  is_flextable_list <-
    !inherits(x, c("gtsummary", "tbl_split", "flextable")) &&
      is.list(x) && length(x) > 0L && all(map_lgl(x, \(el) inherits(el, "flextable")))
  if (!inherits(x, c("gtsummary", "tbl_split", "flextable")) && !is_flextable_list) {
    cli::cli_abort(
      c(
        "The {.arg x} argument must be a {.cls gtsummary}, {.cls tbl_split}, or
         {.cls flextable} object, or a list of {.cls flextable} objects.",
        i = "A list must be non-empty and contain only {.cls flextable} elements."
      ),
      call = get_cli_abort_call()
    )
  }
  check_string(path)
  check_scalar_logical(header)
  check_scalar_logical(footer)
  check_string(header_bookmark)
  check_string(footer_bookmark)
  if (!is.null(template)) {
    check_string(template)
    if (!file.exists(template)) {
      cli::cli_abort(
        "The {.arg template} file does not exist: {.file {template}}.",
        call = get_cli_abort_call()
      )
    }
  }
  check_pkg_installed(c("flextable", "officer"))

  # resolve `pr_section` with argument-over-theme precedence, then validate. the
  # resolved base section controls page margins/size/orientation/columns; its
  # header/footer defaults are later overwritten by the relocated caption/notes.
  pr_section <- pr_section %||% get_theme_element("save_flex_docx-lst:pr_section", eval = TRUE)
  if (!is.null(pr_section) && !inherits(pr_section, "prop_section")) {
    cli::cli_abort(
      "The {.arg pr_section} argument must be an {.cls officer::prop_section}
       object (e.g. from {.fn officer::prop_section}) or {.code NULL}.",
      call = get_cli_abort_call()
    )
  }

  # collections: one section (with its own header/footer) per table ------------
  if (inherits(x, "tbl_split") || is_flextable_list) {
    if (length(x) == 0L) {
      cli::cli_abort(
        "The {.arg x} argument is an empty collection with no tables to write.",
        call = get_cli_abort_call()
      )
    }
    if (!is.null(template)) {
      return(
        .save_flex_docx_collection_template(
          x,
          path = path,
          header = header,
          footer = footer,
          template = template,
          header_bookmark = header_bookmark,
          footer_bookmark = footer_bookmark,
          pr_section = pr_section
        )
      )
    }
    return(
      .save_flex_docx_collection(
        x,
        path = path,
        header = header,
        footer = footer,
        pr_section = pr_section
      )
    )
  }

  # single gtsummary table or flextable ----------------------------------------
  built <- .flex_docx_build_one(x, header = header, footer = footer)

  if (!is.null(template)) {
    .flex_docx_write_template_one(
      built,
      path = path,
      template = template,
      header = header,
      footer = footer,
      header_bookmark = header_bookmark,
      footer_bookmark = footer_bookmark,
      pr_section = pr_section
    )
    return(invisible(x))
  }

  # write the Word file --------------------------------------------------------
  # a section is supplied when a region has content, or when the caller passed a
  # `pr_section` (so custom page margins/size apply even with no caption/notes).
  # otherwise `save_as_docx()` uses its default section.
  has_content <- !is.null(built$caption_fpar) || !is.null(built$footer_ft)
  if (has_content || !is.null(pr_section)) {
    flextable::save_as_docx(
      built$ft,
      path = path,
      pr_section = .flex_docx_prop_section(
        built$caption_fpar,
        built$footer_ft,
        base = pr_section
      )
    )
  } else {
    flextable::save_as_docx(built$ft, path = path)
  }

  invisible(x)
}

#' Write a collection of tables to a single Word file, one section per table
#'
#' Each table is added to the document, separated by a section break, so that
#' each table's caption/notes populate that section's own Word header/footer
#' regions.
#'
#' @inheritParams save_flex_docx
#' @return the original collection `x` (invisibly)
#' @keywords internal
#' @noRd
.save_flex_docx_collection <- function(x, path, header, footer, pr_section = NULL) {
  doc <- officer::read_docx()

  for (i in seq_along(x)) {
    built <- .flex_docx_build_one(x[[i]], header = header, footer = footer)

    doc <- flextable::body_add_flextable(doc, built$ft)

    # every section uses the same base `pr_section` (page margins/size/etc.), but
    # `type = "nextPage"` is forced so tables page correctly without blank pages,
    # overriding any `type` set in the user's `pr_section`.
    section <-
      .flex_docx_prop_section(
        built$caption_fpar,
        built$footer_ft,
        base = pr_section,
        type = "nextPage"
      )

    if (i < length(x)) {
      # close this table's section, attaching its own header/footer. `type =
      # "nextPage"` starts the *next* section on a new page, so no explicit page
      # break is added between tables (that would insert a blank page).
      doc <- officer::body_end_block_section(doc, officer::block_section(section))
    } else {
      # the last table uses the document's default section instead of a block
      # section. `body_end_block_section()` appends a trailing paragraph and a
      # closing section, which would render as an extra blank page at the end.
      doc <- officer::body_set_default_section(doc, section)
    }
  }

  print(doc, target = path)

  invisible(x)
}

#' Write a single table to a Word file using a template document
#'
#' The template's page header/footer regions are preserved; the relocated
#' caption and notes are merged into them at the named bookmarks.
#'
#' @param built (`list`)\cr the value returned by `.flex_docx_build_one()`
#' @inheritParams save_flex_docx
#' @return `NULL`, invisibly
#' @keywords internal
#' @noRd
.flex_docx_write_template_one <- function(built, path, template, header, footer,
                                          header_bookmark, footer_bookmark,
                                          pr_section) {
  doc <- officer::read_docx(path = template)
  doc <- flextable::body_add_flextable(doc, built$ft)

  # the default section must be (re)set: officer cannot round-trip a template's
  # footer/header regions otherwise. drop any header/footer defaults so the
  # template's regions survive (a `footer_default`/`header_default` would replace
  # them). geometry (margins/size/orientation) comes from `pr_section` when
  # supplied, otherwise it is carried over from the template itself.
  base_section <- pr_section %||% .flex_docx_section_from_template(doc)
  doc <- officer::body_set_default_section(
    doc,
    .flex_docx_prop_section(NULL, NULL, base = base_section)
  )

  # merge the relocated caption/notes into the template's regions at the
  # bookmarks. inject only what was relocated (gated by header/footer).
  if (isTRUE(header) && !is.null(built$caption_fpar)) {
    if (!.flex_docx_inject_at_bkm(doc$headers, header_bookmark, built$caption_fpar)) {
      cli::cli_abort(
        c(
          "The {.arg header_bookmark} {.val {header_bookmark}} was not found in
           the {.arg template} header.",
          i = "Add a Word bookmark named {.val {header_bookmark}} to the
               template's header where the caption should appear."
        ),
        call = get_cli_abort_call()
      )
    }
  }
  if (isTRUE(footer) && !is.null(built$footer_ft)) {
    if (!.flex_docx_inject_at_bkm(doc$footers, footer_bookmark, built$footer_ft)) {
      cli::cli_abort(
        c(
          "The {.arg footer_bookmark} {.val {footer_bookmark}} was not found in
           the {.arg template} footer.",
          i = "Add a Word bookmark named {.val {footer_bookmark}} to the
               template's footer where the notes should appear."
        ),
        call = get_cli_abort_call()
      )
    }
  }

  print(doc, target = path)

  invisible(NULL)
}

#' Write a collection of tables to a Word file using a template document
#'
#' Each table becomes its own Word section. The template's page header/footer
#' furniture (page numbers, dates, logos, boilerplate) is repeated on every
#' section, and each table's relocated caption/notes are merged into that
#' section's copy of the template regions at the bookmarks.
#'
#' officer creates the per-section header/footer parts only when the document is
#' printed, so they cannot be edited before printing (unlike the single-table
#' path, which edits the template's own regions in place). Instead, each section
#' is given a placeholder region carrying a unique bookmark; after printing, each
#' section's region file is rewritten with a fresh copy of the template furniture
#' (plus the injected caption/notes), and the document is repackaged.
#'
#' @inheritParams save_flex_docx
#' @return the original collection `x` (invisibly)
#' @keywords internal
#' @noRd
.save_flex_docx_collection_template <- function(x, path, header, footer, template,
                                                header_bookmark, footer_bookmark,
                                                pr_section) {
  regions <- .flex_docx_template_regions(template, header_bookmark, footer_bookmark)
  has_header <- !is.null(regions$header)
  has_footer <- !is.null(regions$footer)

  # validate bookmarks when relocation is requested (mirrors the single-table path)
  if (isTRUE(header) && !isTRUE(regions$header$has_bookmark)) {
    cli::cli_abort(
      c(
        "The {.arg header_bookmark} {.val {header_bookmark}} was not found in
         the {.arg template} header.",
        i = "Add a Word bookmark named {.val {header_bookmark}} to the
             template's header where the caption should appear."
      ),
      call = get_cli_abort_call()
    )
  }
  if (isTRUE(footer) && !isTRUE(regions$footer$has_bookmark)) {
    cli::cli_abort(
      c(
        "The {.arg footer_bookmark} {.val {footer_bookmark}} was not found in
         the {.arg template} footer.",
        i = "Add a Word bookmark named {.val {footer_bookmark}} to the
             template's footer where the notes should appear."
      ),
      call = get_cli_abort_call()
    )
  }

  built_list <- lapply(x, .flex_docx_build_one, header = header, footer = footer)

  # build the document with a placeholder (uniquely bookmarked) header/footer per
  # section, so each section gets its own region part to rewrite after printing.
  doc <- officer::read_docx(path = template)
  base_section <- pr_section %||% .flex_docx_section_from_template(doc)
  for (i in seq_along(built_list)) {
    doc <- flextable::body_add_flextable(doc, built_list[[i]]$ft)
    section <-
      .flex_docx_placeholder_section(base_section, i, has_header, has_footer, type = "nextPage")
    if (i < length(built_list)) {
      doc <- officer::body_end_block_section(doc, officer::block_section(section))
    } else {
      doc <- officer::body_set_default_section(doc, section)
    }
  }
  built_docx <- tempfile(fileext = ".docx")
  print(doc, target = built_docx)

  # post-print: rewrite each section's placeholder region with the template
  # furniture + injected caption/notes, then repackage to `path`.
  .flex_docx_merge_collection_sections(
    built_docx, path, built_list, regions,
    header = header, footer = footer,
    header_bookmark = header_bookmark, footer_bookmark = footer_bookmark,
    has_header = has_header, has_footer = has_footer
  )

  invisible(x)
}

#' Capture a template's header/footer regions for cloning into each section
#'
#' Unzips the template and, for the header and footer, selects the region part
#' that holds the corresponding bookmark (falling back to the first part). Returns
#' the region XML (as a string, re-parsed per section), whether the bookmark was
#' present, the region's relationship nodes (as strings, for images/hyperlinks),
#' and the extraction directory (so media files can be copied). Either region is
#' `NULL` when the template has no part of that kind.
#'
#' @param template (`string`)\cr path to the template `.docx`
#' @param header_bookmark,footer_bookmark (`string`)\cr the bookmark names
#' @return a list with elements `header`, `footer` (each a list or `NULL`) and
#'   `dir` (the extraction directory)
#' @keywords internal
#' @noRd
.flex_docx_template_regions <- function(template, header_bookmark, footer_bookmark) {
  dir <- tempfile()
  dir.create(dir)
  utils::unzip(template, exdir = dir)
  word <- file.path(dir, "word")

  pick <- function(kind, bookmark) {
    files <- list.files(word, pattern = sprintf("^%s[0-9]+[.]xml$", kind), full.names = TRUE)
    if (length(files) == 0L) {
      return(NULL)
    }
    bkm_pattern <- sprintf("w:name=\"%s\"", bookmark)
    with_bkm <- Filter(
      function(f) grepl(bkm_pattern, paste(readLines(f, warn = FALSE), collapse = ""), fixed = TRUE),
      files
    )
    file <- if (length(with_bkm) > 0L) with_bkm[[1]] else files[[1]]
    rels_file <- file.path(dirname(file), "_rels", paste0(basename(file), ".rels"))
    rels <-
      if (file.exists(rels_file)) {
        vapply(xml2::xml_children(xml2::read_xml(rels_file)), as.character, character(1))
      } else {
        character(0)
      }
    list(
      xml = paste(readLines(file, warn = FALSE), collapse = ""),
      has_bookmark = length(with_bkm) > 0L,
      rels = rels
    )
  }

  list(
    header = pick("header", header_bookmark),
    footer = pick("footer", footer_bookmark),
    dir = dir
  )
}

#' Build a placeholder section for one table in the template collection path
#'
#' Carries the geometry from `base` and, for each region the template has, a
#' header/footer default holding a uniquely named bookmark. The bookmark lets the
#' post-print step locate this section's region part; its content is then replaced
#' wholesale by the template furniture.
#'
#' @param base (`officer::prop_section`)\cr the base geometry section
#' @param index (`integer`)\cr the table's position in the collection
#' @param has_header,has_footer (`logical`)\cr whether the template has that region
#' @param type (`string`)\cr the section `type` (e.g. `"nextPage"`)
#' @return an `officer::prop_section` object
#' @keywords internal
#' @noRd
.flex_docx_placeholder_section <- function(base, index, has_header, has_footer, type) {
  section_args <- .flex_docx_base_geometry_args(base)
  section_args$type <- type
  if (isTRUE(has_header)) {
    section_args$header_default <- officer::block_list(officer::fpar(
      officer::run_bookmark(paste0("gtsummary_sec_header_", index), officer::ftext(""))
    ))
  }
  if (isTRUE(has_footer)) {
    section_args$footer_default <- officer::block_list(officer::fpar(
      officer::run_bookmark(paste0("gtsummary_sec_footer_", index), officer::ftext(""))
    ))
  }
  do.call(officer::prop_section, section_args)
}

#' Rewrite each section's placeholder regions and repackage the document
#'
#' For each table, finds its placeholder header/footer part (by the unique
#' bookmark), replaces the part's content with a fresh copy of the template
#' furniture, injects the caption/notes at the bookmark (or drops the placeholder
#' paragraph when there is nothing to inject), and merges the template region's
#' relationships. Template media files are copied over so images/logos resolve.
#' The modified files are repackaged to `path` with `zip::zipr()` (the same
#' mechanism officer uses to write `.docx`).
#'
#' @param built_docx (`string`)\cr the printed (placeholder) document
#' @param path (`string`)\cr the output file path
#' @param built_list (`list`)\cr the per-table `.flex_docx_build_one()` results
#' @param regions (`list`)\cr the value from `.flex_docx_template_regions()`
#' @inheritParams save_flex_docx
#' @param has_header,has_footer (`logical`)\cr whether the template has that region
#' @return `NULL`, invisibly
#' @keywords internal
#' @noRd
.flex_docx_merge_collection_sections <- function(built_docx, path, built_list, regions,
                                                 header, footer, header_bookmark,
                                                 footer_bookmark, has_header, has_footer) {
  dir <- tempfile()
  dir.create(dir)
  utils::unzip(built_docx, exdir = dir)
  word <- file.path(dir, "word")

  # copy the template's media (logos, images) so relationships in the cloned
  # furniture resolve in the repackaged document.
  tmpl_media <- file.path(regions$dir, "word", "media")
  if (dir.exists(tmpl_media)) {
    dest_media <- file.path(word, "media")
    dir.create(dest_media, showWarnings = FALSE)
    file.copy(list.files(tmpl_media, full.names = TRUE), dest_media, overwrite = FALSE)
  }

  for (i in seq_along(built_list)) {
    if (has_header) {
      .flex_docx_rewrite_region(
        word,
        marker = paste0("gtsummary_sec_header_", i),
        kind = "header",
        furniture = regions$header,
        bookmark = header_bookmark,
        content = if (isTRUE(header)) built_list[[i]]$caption_fpar else NULL
      )
    }
    if (has_footer) {
      .flex_docx_rewrite_region(
        word,
        marker = paste0("gtsummary_sec_footer_", i),
        kind = "footer",
        furniture = regions$footer,
        bookmark = footer_bookmark,
        content = if (isTRUE(footer)) built_list[[i]]$footer_ft else NULL
      )
    }
  }

  # drop the template's original (now unreferenced) header/footer parts, which
  # would otherwise linger with their placeholder text.
  .flex_docx_prune_unreferenced_regions(dir)

  if (file.exists(path)) file.remove(path)
  zip::zipr(zipfile = path, files = list.files(dir, full.names = TRUE), recurse = TRUE)

  invisible(NULL)
}

#' Remove unreferenced header/footer parts from an unzipped document
#'
#' After the collection template path replaces the default section, the template's
#' own header/footer parts are left unreferenced (still holding their placeholder
#' text). This removes any header/footer part not referenced by a section in
#' `document.xml`, along with its relationship entry and content-type override, so
#' the output has no stray placeholder content.
#'
#' @param root (`string`)\cr the unzipped document's root directory
#' @return `NULL`, invisibly
#' @keywords internal
#' @noRd
.flex_docx_prune_unreferenced_regions <- function(root) {
  word <- file.path(root, "word")
  doc <- xml2::read_xml(file.path(word, "document.xml"))
  refs <- xml2::xml_find_all(doc, "//w:headerReference|//w:footerReference")
  ref_ids <- vapply(refs, function(n) xml2::xml_attr(n, "id"), character(1))

  rels_file <- file.path(word, "_rels", "document.xml.rels")
  rels <- xml2::read_xml(rels_file)
  ct_file <- file.path(root, "[Content_Types].xml")
  ct <- xml2::read_xml(ct_file)

  removed_any <- FALSE
  for (rn in xml2::xml_children(rels)) {
    type <- xml2::xml_attr(rn, "Type")
    if (!grepl("/(header|footer)$", type)) next
    if (xml2::xml_attr(rn, "Id") %in% ref_ids) next
    target <- xml2::xml_attr(rn, "Target")
    part_file <- file.path(word, target)
    if (file.exists(part_file)) file.remove(part_file)
    part_rels <- file.path(word, "_rels", paste0(basename(target), ".rels"))
    if (file.exists(part_rels)) file.remove(part_rels)
    override <- xml2::xml_find_first(
      ct, sprintf("//*[local-name()='Override'][@PartName='/word/%s']", target)
    )
    if (!inherits(override, "xml_missing")) xml2::xml_remove(override)
    xml2::xml_remove(rn)
    removed_any <- TRUE
  }
  if (removed_any) {
    xml2::write_xml(rels, rels_file)
    xml2::write_xml(ct, ct_file)
  }

  invisible(NULL)
}

#' Rewrite one section's region file with the template furniture (+ content)
#'
#' Locates the region part containing `marker`, replaces its children with a
#' fresh copy of the template furniture, injects `content` at `bookmark` (a
#' flextable/`fpar` inserted after the bookmark's paragraph, which is then
#' removed), and merges the template region's relationships into the part.
#'
#' @param word (`string`)\cr the `word/` directory of the unzipped document
#' @param marker (`string`)\cr the unique placeholder bookmark for this section
#' @param kind (`string`)\cr `"header"` or `"footer"`
#' @param furniture (`list`)\cr the region entry from `.flex_docx_template_regions()`
#' @param bookmark (`string`)\cr the content bookmark name
#' @param content (`flextable`, `officer::fpar`, or `NULL`)\cr the content to inject
#' @return `NULL`, invisibly
#' @keywords internal
#' @noRd
.flex_docx_rewrite_region <- function(word, marker, kind, furniture, bookmark, content) {
  files <- list.files(word, pattern = sprintf("^%s[0-9]+[.]xml$", kind), full.names = TRUE)
  target <- NULL
  for (f in files) {
    if (grepl(marker, paste(readLines(f, warn = FALSE), collapse = ""), fixed = TRUE)) {
      target <- f
      break
    }
  }
  if (is.null(target)) {
    return(invisible(NULL))
  }

  region <- xml2::read_xml(target)
  furniture_doc <- xml2::read_xml(furniture$xml)

  # replace the region's content with a fresh copy of the template furniture
  for (ch in xml2::xml_children(region)) xml2::xml_remove(ch)
  for (ch in xml2::xml_children(furniture_doc)) xml2::xml_add_child(region, ch)

  # inject content at the bookmark, or drop the placeholder paragraph
  bm <- xml2::xml_find_first(region, sprintf("//w:bookmarkStart[@w:name='%s']", bookmark))
  if (!inherits(bm, "xml_missing")) {
    para <- xml2::xml_find_first(bm, "ancestor::w:p")
    if (!inherits(para, "xml_missing")) {
      if (!is.null(content)) {
        wml <- officer::to_wml(content, add_ns = TRUE)
        xml2::xml_add_sibling(para, xml2::as_xml_document(wml), .where = "after")
      }
      xml2::xml_remove(para)
    }
  }

  xml2::write_xml(region, target)
  .flex_docx_merge_region_rels(target, furniture$rels)

  invisible(NULL)
}

#' Merge template region relationships into a section's region part
#'
#' Adds the template region's relationship entries (images, hyperlinks) to the
#' section part's `.rels` file (creating it when absent), skipping ids already
#' present. A no-op when the template region has no relationships (the common
#' case of text and page fields).
#'
#' @param region_file (`string`)\cr the section region file just written
#' @param rels (`character`)\cr relationship nodes as strings
#' @return `NULL`, invisibly
#' @keywords internal
#' @noRd
.flex_docx_merge_region_rels <- function(region_file, rels) {
  if (length(rels) == 0L) {
    return(invisible(NULL))
  }
  ns <- "http://schemas.openxmlformats.org/package/2006/relationships"
  rels_dir <- file.path(dirname(region_file), "_rels")
  rels_file <- file.path(rels_dir, paste0(basename(region_file), ".rels"))
  if (file.exists(rels_file)) {
    rels_doc <- xml2::read_xml(rels_file)
  } else {
    dir.create(rels_dir, showWarnings = FALSE)
    rels_doc <- xml2::read_xml(sprintf("<Relationships xmlns=\"%s\"></Relationships>", ns))
  }
  existing <- vapply(xml2::xml_children(rels_doc), function(n) xml2::xml_attr(n, "Id"), character(1))
  for (r in rels) {
    node <- xml2::xml_child(xml2::read_xml(sprintf("<Relationships xmlns=\"%s\">%s</Relationships>", ns, r)))
    if (!xml2::xml_attr(node, "Id") %in% existing) {
      xml2::xml_add_child(rels_doc, node)
    }
  }
  xml2::write_xml(rels_doc, rels_file)

  invisible(NULL)
}

#' Build the flextable and relocated caption/notes for one table
#'
#' Shared by the single-table and collection paths, for both gtsummary and
#' flextable input. Obtains the flextable (converting a gtsummary table via
#' [`as_flex_table()`], or using a flextable directly) and, when relocation is
#' requested, produces the caption paragraph (for the Word header) and the
#' footer-only flextable (for the Word footer), removing that content from the
#' table so it is not also rendered in the body.
#'
#' @inheritParams save_flex_docx
#' @return a list with elements `ft` (flextable), `caption_fpar`
#'   (`officer::fpar` or `NULL`), and `footer_ft` (flextable or `NULL`)
#' @keywords internal
#' @noRd
.flex_docx_build_one <- function(x, header, footer) {
  is_flextable <- inherits(x, "flextable")

  # extract caption text, then obtain the flextable with the relocated caption
  # suppressed.
  if (is_flextable) {
    caption_text <- .flex_docx_caption_flextable(x)
    ft <- x
    if (isTRUE(header)) {
      # caption is relocated to the Word header; clear it so it does not also
      # render in the table body.
      ft <- flextable::set_caption(ft, caption = "")
    }
  } else {
    caption_text <- .flex_docx_caption(x)
    flextable_calls <- as_flex_table(x, return_calls = TRUE)
    if (isTRUE(header)) {
      # caption is relocated to the Word header; drop the flextable caption
      flextable_calls[["set_caption"]] <- NULL
    }
    ft <- .eval_list_of_exprs(flextable_calls)
  }

  # build the relocated caption paragraph for the Word header (with markdown
  # emphasis preserved), matched to the flextable body font.
  caption_fpar <-
    if (isTRUE(header) && !is.null(caption_text)) {
      .flex_docx_caption_fpar(caption_text, fp_text = .flex_docx_default_font())
    } else {
      NULL
    }

  # build the relocated notes as a footer-only flextable for the Word footer, and
  # remove the footer part from the table. the footer flextable keeps the footer
  # part's styling and the in-cell reference symbols stay on the header/body.
  footer_ft <- NULL
  if (isTRUE(footer)) {
    footer_ft <- .flex_docx_footer_flextable(ft)
    if (flextable::nrow_part(ft, part = "footer") > 0L) {
      ft <- flextable::delete_part(ft, part = "footer")
    }
  }

  list(ft = ft, caption_fpar = caption_fpar, footer_ft = footer_ft)
}

#' Build an `officer::prop_section()` from the relocated caption/notes
#'
#' A region is only attached when there is content for it.
#'
#' @param caption_fpar (`officer::fpar` or `NULL`)\cr the relocated caption
#' @param footer_ft (`flextable` or `NULL`)\cr the relocated notes flextable
#' @param ... additional arguments passed to `officer::prop_section()` (e.g.
#'   `type`)
#' @param base (`officer::prop_section` or `NULL`)\cr an optional user-supplied
#'   section whose properties (page margins, size, orientation, columns, and
#'   `type`) are used as the base. Its `header_default`/`footer_default` are
#'   always discarded: `save_flex_docx()` owns those regions.
#' @return an `officer::prop_section` object
#' @keywords internal
#' @noRd
.flex_docx_prop_section <- function(caption_fpar, footer_ft, base = NULL, ...) {
  # start from the user's base section fields (dropping its header/footer
  # defaults, which we always own), then let `...` overrides win (e.g. the forced
  # `type = "nextPage"` for collections), and finally attach our relocated
  # caption/notes as the header/footer defaults.
  section_args <- .flex_docx_base_geometry_args(base)
  section_args <- utils::modifyList(section_args, list(...))
  if (!is.null(caption_fpar)) {
    section_args$header_default <- officer::block_list(caption_fpar)
  }
  if (!is.null(footer_ft)) {
    section_args$footer_default <- officer::block_list(footer_ft)
  }
  do.call(officer::prop_section, section_args)
}

#' Extract the geometry-only fields from a base `officer::prop_section`
#'
#' Returns the base section's fields (page margins, size, orientation, columns,
#' and `type`) with the header/footer defaults dropped, as a plain list suitable
#' for `do.call(officer::prop_section, .)`. `save_flex_docx()` always owns the
#' header/footer regions, so those defaults are never carried over. Returns an
#' empty list when `base` is `NULL`.
#'
#' @param base (`officer::prop_section` or `NULL`)\cr the base section
#' @return a named list of `prop_section` arguments
#' @keywords internal
#' @noRd
.flex_docx_base_geometry_args <- function(base) {
  if (is.null(base)) {
    return(list())
  }
  base_fields <- unclass(base)
  base_fields[c(
    "header_default", "header_even", "header_first",
    "footer_default", "footer_even", "footer_first"
  )] <- NULL
  base_fields
}

#' Inject content at a Word bookmark inside a set of header/footer parts
#'
#' Renders `value` (a flextable or an `officer::fpar`) to WordprocessingML and
#' replaces the paragraph that holds the named bookmark with it. A table is
#' block-level, so it is inserted as a sibling of the bookmark's paragraph, then
#' the (placeholder) paragraph is removed. Returns `TRUE` if the bookmark was
#' found in at least one part.
#'
#' @param parts (`list`)\cr `doc$headers` or `doc$footers` (a list of officer
#'   `docx_part` objects)
#' @param bookmark (`string`)\cr the Word bookmark name
#' @param value (`flextable` or `officer::fpar`)\cr the content to insert
#' @return a scalar logical: whether the bookmark was found
#' @keywords internal
#' @noRd
.flex_docx_inject_at_bkm <- function(parts, bookmark, value) {
  wml <- officer::to_wml(value, add_ns = TRUE)
  injected <- FALSE
  for (part in parts) {
    node <- part$get()
    bm <- xml2::xml_find_first(node, sprintf("//w:bookmarkStart[@w:name='%s']", bookmark))
    if (inherits(bm, "xml_missing")) next
    para <- xml2::xml_find_first(bm, "ancestor::w:p")
    if (inherits(para, "xml_missing")) next
    # insert the rendered content after the placeholder paragraph, then drop the
    # placeholder paragraph (which held the bookmark and any placeholder text).
    xml2::xml_add_sibling(para, xml2::as_xml_document(wml), .where = "after")
    xml2::xml_remove(para)
    injected <- TRUE
  }
  injected
}

#' Extract the caption text from a gtsummary table for the Word header
#'
#' Returns the caption string (keeping markdown emphasis, which is interpreted
#' when the caption paragraph is built), or `NULL` when there is no caption.
#'
#' @param x (`gtsummary`)\cr a gtsummary table
#' @return a string or `NULL`
#' @keywords internal
#' @noRd
.flex_docx_caption <- function(x) {
  caption <- x$table_styling$caption
  if (is.null(caption) || !nzchar(caption)) {
    return(NULL)
  }
  caption
}

#' Extract the caption text from a flextable for the Word header
#'
#' Reads `ft$caption$value` (set via `flextable::set_caption()`). Returns the
#' caption string as-is (flextable captions are plain text), or `NULL` when there
#' is no caption.
#'
#' @param x (`flextable`)\cr a flextable object
#' @return a string or `NULL`
#' @keywords internal
#' @noRd
.flex_docx_caption_flextable <- function(x) {
  caption <- x$caption$value
  if (is.null(caption) || !is.character(caption) || !nzchar(caption)) {
    return(NULL)
  }
  caption
}

#' Build the footer-only flextable for the Word footer
#'
#' Derives a flextable that contains only the (styled) footer part of `ft`, by
#' deleting the header and body parts. This keeps the footer's reference symbols,
#' emphasis, and per-cell styling so the Word footer renders them as a flextable.
#' Returns `NULL` when `ft` has no footer content.
#'
#' @param ft (`flextable`)\cr the built flextable
#' @return a `flextable` or `NULL`
#' @keywords internal
#' @noRd
.flex_docx_footer_flextable <- function(ft) {
  if (flextable::nrow_part(ft, part = "footer") == 0L) {
    return(NULL)
  }
  ft |>
    flextable::delete_part(part = "header") |>
    flextable::delete_part(part = "body")
}

#' Build the relocated caption paragraph for the Word header
#'
#' Parses the `**bold**` / `_italic_` markdown subset gtsummary supports into
#' `officer::ftext()` runs and returns an `officer::fpar()`. The parsing mirrors
#' `.chr_with_md_to_ft_compose()` (in `R/as_flex_table.R`) but emits officer
#' runs instead of `flextable::compose()` calls.
#'
#' @param caption (`string`)\cr the caption text (possibly with markdown)
#' @param fp_text (`fp_text`)\cr run properties applied to every run so the
#'   caption matches the table body font
#' @return an `officer::fpar` object
#' @keywords internal
#' @noRd
.flex_docx_caption_fpar <- function(caption, fp_text = officer::fp_text()) {
  break_chr <- "@@@@@@@@@@@&@@@@@@@@@"
  # wrap emphasis spans in a delimiter so they can be split into their own runs
  x <- str_replace_all(caption, "\\*\\*(.*?)\\*\\*", paste0(break_chr, "**\\1**", break_chr))
  x <- str_replace_all(x, "\\_(.*?)\\_", paste0(break_chr, "_\\1_", break_chr))

  pieces <- str_split(x, break_chr)[[1]]
  pieces <- pieces[nzchar(pieces)]

  runs <-
    lapply(pieces, function(piece) {
      if (startsWith(piece, "**") && endsWith(piece, "**")) {
        officer::ftext(
          str_replace_all(piece, "\\*\\*(.*?)\\*\\*", "\\1"),
          prop = stats::update(fp_text, bold = TRUE)
        )
      } else if (startsWith(piece, "_") && endsWith(piece, "_")) {
        officer::ftext(
          str_replace_all(piece, "\\_(.*?)\\_", "\\1"),
          prop = stats::update(fp_text, italic = TRUE)
        )
      } else {
        officer::ftext(piece, prop = fp_text)
      }
    })

  inject(officer::fpar(!!!runs))
}

#' Resolve the flextable default font for the Word caption
#'
#' Reads `flextable::get_flextable_defaults()` and returns an `officer::fp_text()`
#' carrying the table body's `font.family` and `font.size`. This lets the Word
#' caption match the flextable body font instead of falling back to the Word
#' template default. A property is omitted when the corresponding flextable
#' default is missing, so the existing default still applies.
#'
#' @return an `officer::fp_text` object
#' @keywords internal
#' @noRd
.flex_docx_default_font <- function() {
  defaults <- flextable::get_flextable_defaults()
  args <- list()
  if (!is.null(defaults$font.family)) args$font.family <- defaults$font.family
  if (!is.null(defaults$font.size)) args$font.size <- defaults$font.size
  do.call(officer::fp_text, args)
}

#' Build an `officer::prop_section()` carrying a template's page geometry
#'
#' Reads a read-in template's page size, orientation, and margins via
#' [`officer::docx_dim()`] and returns a `prop_section` reproducing them (with no
#' header/footer defaults). Used when a `template` is supplied without a
#' `pr_section`: the default section must be re-set for officer to round-trip the
#' template's header/footer regions, and this preserves the template's geometry
#' rather than resetting it to the officer defaults.
#'
#' @param doc (`rdocx`)\cr a document from [`officer::read_docx()`]
#' @return an `officer::prop_section` object
#' @keywords internal
#' @noRd
.flex_docx_section_from_template <- function(doc) {
  dd <- officer::docx_dim(doc)
  # `docx_dim()` reports the (oriented) page dims in inches; `page_size()` with
  # `orient` swaps width/height itself, so feed it the portrait dims (short side
  # as width, long side as height) and let `orient` apply the rotation.
  short <- min(dd$page[["width"]], dd$page[["height"]])
  long <- max(dd$page[["width"]], dd$page[["height"]])
  m <- dd$margins
  officer::prop_section(
    page_size = officer::page_size(
      width = short,
      height = long,
      orient = if (isTRUE(dd$landscape)) "landscape" else "portrait"
    ),
    page_margins = officer::page_mar(
      top = m[["top"]], bottom = m[["bottom"]],
      left = m[["left"]], right = m[["right"]],
      header = m[["header"]], footer = m[["footer"]]
    )
  )
}
