#' Save a gtsummary table to a Word file
#'
#' @description
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
#' A collection of tables is also accepted: a `tbl_split` object (from
#' [`tbl_split_by_rows()`] or [`tbl_split_by_columns()`]), or a plain list of
#' flextables. Each table is written to its own Word section so that each table's
#' caption and footnote-region content populate that section's own header/footer
#' regions, one table per page.
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
#'   document's page footer region. When `FALSE`, this content is rendered in the
#'   table's footer. Default is `FALSE`.
#' @param page (`string`)\cr
#'   an optional string added as a page-number line in a header/footer region.
#'   The tokens `{PAGE}` and `{NUMPAGES}` are replaced with live Word fields for
#'   the current page number and total page count (e.g.
#'   `"Page {PAGE} of {NUMPAGES}"`); all other text is added verbatim. Default is
#'   `NULL` (no page line). This is independent of the `header`/`footer`
#'   arguments.
#' @param page_location (`string`)\cr
#'   where to place the `page` text, as `"<region>-<alignment>"`. Must be one of
#'   `"footer-right"` (default), `"footer-center"`, `"footer-left"`,
#'   `"header-right"`, `"header-center"`, or `"header-left"`.
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
#' Relocating the caption to the Word header (`header = TRUE`) and the
#' footnotes, source notes, and abbreviations to the Word footer
#' (`footer = TRUE`) is lossy:
#'
#' - **Plain text only.** Relocated content is rendered as plain text: markdown
#'   and HTML are not interpreted and emphasis markers (`**bold**`, `_italic_`)
#'   are stripped. Only the font family and size are carried over; colors,
#'   indentation, per-cell styling, and column structure are not preserved.
#'
#' @examplesIf gtsummary:::is_pkg_installed(c("flextable", "officer"))
#' tbl <-
#'   trial |>
#'   tbl_summary(by = trt, include = c(age, grade)) |>
#'   modify_caption("**Table 1. Patient Characteristics**")
#'
#' # save the table, placing caption in the header and notes in the footer
#' save_flex_docx(tbl, path = tempfile(fileext = ".docx"))
#'
#' # add a "Page X of Y" line to the footer
#' save_flex_docx(
#'   tbl,
#'   path = tempfile(fileext = ".docx"),
#'   page = "Page {PAGE} of {NUMPAGES}"
#' )
#'
#' # a split table is written with one table per section/page
#' trial |>
#'   tbl_summary(by = trt, include = c(age, marker, grade)) |>
#'   tbl_split_by_rows(variables = c(age, marker)) |>
#'   save_flex_docx(path = tempfile(fileext = ".docx"))
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
                         page = NULL,
                         page_location = c(
                           "footer-right", "footer-center", "footer-left",
                           "header-right", "header-center", "header-left"
                         ),
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
  if (!is.null(page)) check_string(page)
  page_location <- arg_match(page_location)
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
    return(
      .save_flex_docx_collection(
        x,
        path = path,
        header = header,
        footer = footer,
        page = page,
        page_location = page_location,
        pr_section = pr_section
      )
    )
  }

  # single gtsummary table or flextable ----------------------------------------
  built <-
    .flex_docx_build_one(
      x,
      header = header,
      footer = footer,
      page = page,
      page_location = page_location
    )

  # write the Word file --------------------------------------------------------
  # a section is supplied when a region has content, or when the caller passed a
  # `pr_section` (so custom page margins/size apply even with no caption/notes).
  # otherwise `save_as_docx()` uses its default section.
  has_content <- length(built$header_fpars) > 0L || length(built$footer_fpars) > 0L
  if (has_content || !is.null(pr_section)) {
    flextable::save_as_docx(
      built$ft,
      path = path,
      pr_section = .flex_docx_prop_section(
        built$header_fpars,
        built$footer_fpars,
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
#' Each table is added to the document, separated by a page break, and closed by
#' a section break so that each table's caption/notes populate that section's own
#' Word header/footer regions.
#'
#' @inheritParams save_flex_docx
#' @return the original collection `x` (invisibly)
#' @keywords internal
#' @noRd
.save_flex_docx_collection <- function(x, path, header, footer, page, page_location,
                                       pr_section = NULL) {
  doc <- officer::read_docx()

  for (i in seq_along(x)) {
    built <-
      .flex_docx_build_one(
        x[[i]],
        header = header,
        footer = footer,
        page = page,
        page_location = page_location
      )

    doc <- flextable::body_add_flextable(doc, built$ft)

    # every section uses the same base `pr_section` (page margins/size/etc.), but
    # `type = "nextPage"` is forced so tables page correctly without blank pages,
    # overriding any `type` set in the user's `pr_section`.
    section <-
      .flex_docx_prop_section(
        built$header_fpars,
        built$footer_fpars,
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

#' Build the flextable and header/footer paragraph lists for one table
#'
#' Shared by the single-table and collection paths, for both gtsummary and
#' flextable input. Obtains the flextable (converting a gtsummary table via
#' [`as_flex_table()`], or using a flextable directly), relocates the caption
#' and/or footer content, and assembles the Word header/footer paragraph lists
#' (caption/notes plus the optional page-number line).
#'
#' @inheritParams save_flex_docx
#' @return a list with elements `ft` (flextable), `header_fpars` (list of
#'   `fpar`), and `footer_fpars` (list of `fpar`)
#' @keywords internal
#' @noRd
.flex_docx_build_one <- function(x, header, footer, page, page_location) {
  is_flextable <- inherits(x, "flextable")

  # extract caption and footer content, then obtain the flextable with the
  # relocated content suppressed.
  if (is_flextable) {
    caption_text <- .flex_docx_caption_flextable(x)
    footer_lines <- .flex_docx_footer_lines_flextable(x)
    ft <- x
    if (isTRUE(header)) {
      # caption is relocated to the Word header; clear it so it does not also
      # render in the table body.
      ft <- flextable::set_caption(ft, caption = "")
    }
  } else {
    caption_text <- .flex_docx_caption(x)
    footer_lines <- .flex_docx_footer_lines(x)
    flextable_calls <- as_flex_table(x, return_calls = TRUE)
    if (isTRUE(header)) {
      # caption is relocated to the Word header; drop the flextable caption
      flextable_calls[["set_caption"]] <- NULL
    }
    ft <- .eval_list_of_exprs(flextable_calls)
  }

  # extract per-part styling from the flextable so each Word region can inherit
  # it. the footer part must be read *before* it is deleted below. the Word
  # header region inherits from the flextable header part, the Word footer region
  # from the flextable footer part. only inherit when content is actually
  # relocated into the region (a caption for the header, footer lines for the
  # footer): flextable keeps a blank footer row even with no notes, so
  # `nrow_part()` alone would wrongly report content, and inheriting then would
  # apply footer styling to a region that only holds a page-number line.
  header_extracted <-
    if (isTRUE(header) && !is.null(caption_text)) .flex_docx_part_font(ft, "header") else NULL
  footer_extracted <-
    if (isTRUE(footer) && length(footer_lines) > 0L) .flex_docx_part_font(ft, "footer") else NULL

  if (isTRUE(footer)) {
    # footnote text, source notes, and abbreviations are relocated to the Word
    # footer. Deleting the flextable footer part removes this text while the
    # in-cell footnote reference symbols (set on the header/body parts by the
    # `flextable::footnote()` calls) are retained.
    if (flextable::nrow_part(ft, part = "footer") > 0L) {
      ft <- flextable::delete_part(ft, part = "footer")
    }
  }

  # resolve the font for each Word region. the base is the flextable body font
  # (so regions match the body by default, instead of the Word template default
  # e.g. Cambria); on top of that we merge the styling extracted from the
  # corresponding flextable part (when present), so styling applied to the
  # flextable header/footer flows through to the Word header/footer.
  base_fp <- .flex_docx_default_font()
  header_fp <- .flex_docx_region_font(base_fp, extracted_props = header_extracted)
  footer_fp <- .flex_docx_region_font(base_fp, extracted_props = footer_extracted)

  # assemble the Word header/footer paragraph lists. content order is
  # caption/notes first, then the optional page-number line as a separate
  # paragraph.
  header_fpars <- list()
  footer_fpars <- list()

  if (isTRUE(header) && !is.null(caption_text)) {
    header_fpars <- c(header_fpars, list(officer::fpar(officer::ftext(caption_text, prop = header_fp))))
  }
  if (isTRUE(footer) && length(footer_lines) > 0L) {
    footer_fpars <- c(footer_fpars, lapply(footer_lines, \(line) officer::fpar(officer::ftext(line, prop = footer_fp))))
  }

  # optional page-number line (independent of the header/footer flags). it adopts
  # the resolved style of whichever region it is placed in.
  if (!is.null(page)) {
    page_region <- sub("-.*$", "", page_location)
    page_align <- sub("^.*-", "", page_location)
    page_fp <- if (identical(page_region, "header")) header_fp else footer_fp
    page_fpar <- .flex_docx_page_fpar(page, alignment = page_align, fp_text = page_fp)
    if (identical(page_region, "header")) {
      header_fpars <- c(header_fpars, list(page_fpar))
    } else {
      footer_fpars <- c(footer_fpars, list(page_fpar))
    }
  }

  list(ft = ft, header_fpars = header_fpars, footer_fpars = footer_fpars)
}

#' Build an `officer::prop_section()` from header/footer paragraph lists
#'
#' A region is only attached when it has at least one paragraph.
#'
#' @param header_fpars,footer_fpars (`list`)\cr lists of `officer::fpar` objects
#' @param ... additional arguments passed to `officer::prop_section()` (e.g.
#'   `type`)
#' @param base (`officer::prop_section` or `NULL`)\cr an optional user-supplied
#'   section whose properties (page margins, size, orientation, columns, and
#'   `type`) are used as the base. Its `header_default`/`footer_default` are
#'   always discarded: `save_flex_docx()` owns those regions.
#' @return an `officer::prop_section` object
#' @keywords internal
#' @noRd
.flex_docx_prop_section <- function(header_fpars, footer_fpars, base = NULL, ...) {
  # start from the user's base section fields (dropping its header/footer
  # defaults, which we always own), then let `...` overrides win (e.g. the forced
  # `type = "nextPage"` for collections), and finally attach our relocated
  # caption/notes as the header/footer defaults.
  section_args <- list()
  if (!is.null(base)) {
    base_fields <- unclass(base)
    base_fields[c(
      "header_default", "header_even", "header_first",
      "footer_default", "footer_even", "footer_first"
    )] <- NULL
    section_args <- base_fields
  }
  section_args <- utils::modifyList(section_args, list(...))
  if (length(header_fpars) > 0L) {
    section_args$header_default <- do.call(officer::block_list, header_fpars)
  }
  if (length(footer_fpars) > 0L) {
    section_args$footer_default <- do.call(officer::block_list, footer_fpars)
  }
  do.call(officer::prop_section, section_args)
}

#' Extract the caption text from a gtsummary table for the Word header
#'
#' Returns the caption string (with markdown emphasis markers stripped, since the
#' Word header renders plain text), or `NULL` when there is no caption.
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
  .strip_markdown(caption)
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

#' Assemble the footer text lines from a flextable footer part
#'
#' Returns the text of each footer row, in order, dropping blank rows. Returns an
#' empty character vector when the flextable has no footer part.
#'
#' @param x (`flextable`)\cr a flextable object
#' @return a character vector (possibly empty)
#' @keywords internal
#' @noRd
.flex_docx_footer_lines_flextable <- function(x) {
  if (flextable::nrow_part(x, part = "footer") == 0L) {
    return(character(0))
  }
  # the rendered footer text lives in the paragraph content of the footer part
  # (`ft$footer$content$data`), a matrix of per-cell chunk data frames each with
  # a `txt` column. the first column of each row holds that line's text (spanned
  # footer cells repeat the same content); concatenate the chunks per row so
  # footnote reference symbols stay attached to their text.
  content <- x$footer$content$data
  lines <- vapply(
    seq_len(nrow(content)),
    function(i) paste(content[i, 1][[1]]$txt, collapse = ""),
    character(1)
  )
  lines[nzchar(lines)]
}

#' Assemble the ordered footer text lines for the Word footer
#'
#' Returns a character vector of the footnote text (in reference-symbol order,
#' each prefixed with its symbol), followed by source notes, followed by the
#' abbreviation line. Returns an empty character vector when there is no footer
#' content.
#'
#' @param x (`gtsummary`)\cr a gtsummary table
#' @return a character vector (possibly empty)
#' @keywords internal
#' @noRd
.flex_docx_footer_lines <- function(x) {
  # add the header `id` column used by `.number_footnotes()`
  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::group_by(.data$hide) |>
    dplyr::mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) |>
    dplyr::ungroup()

  # resolve reference symbols (custom or default integer numbering)
  footnote_symbol <- .resolve_footnote_symbols(x)
  ref_symbol_for <- function(footnote_id) {
    if (is.null(footnote_symbol)) {
      return(as.character(footnote_id))
    }
    .map_footnote_symbols(footnote_id, footnote_symbol)
  }

  # resolve footnote removals/replacements before numbering, mirroring the
  # `as_flex_table()` path. without this, footnotes removed via
  # `remove_footnote_*()` (stored as removal-marker rows / `NA` text) would leak
  # into the Word footer as spurious "<id> NA" lines.
  shown_columns <- x$table_styling$header$column[!x$table_styling$header$hide]
  resolve_footnote_removals <- function(df) {
    if (nrow(df) == 0L) {
      return(df)
    }
    df |>
      dplyr::mutate(remove = ifelse(is.na(.data$footnote), TRUE, .data$remove)) |>
      .filter_row_with_subsequent_replace_or_removal() |>
      dplyr::filter(!.data$remove, .data$column %in% .env$shown_columns)
  }
  # body footnotes additionally carry a `rows` predicate. resolve it to row
  # numbers (against this table's body) and drop footnotes that match no rows,
  # mirroring `as_flex_table()`. this matters for `tbl_split_by_rows()`: a
  # footnote scoped to rows absent from a section must not appear in that
  # section's Word footer.
  resolve_footnote_body <- function(df) {
    if (nrow(df) == 0L) {
      return(df)
    }
    df |>
      dplyr::mutate(
        remove = ifelse(is.na(.data$footnote), TRUE, .data$remove),
        row_numbers = map(.data$rows, \(rows) .rows_expr_to_row_numbers(x$table_body, rows))
      ) |>
      tidyr::unnest(cols = "row_numbers") |>
      .filter_row_with_subsequent_replace_or_removal() |>
      dplyr::filter(!.data$remove, .data$column %in% .env$shown_columns)
  }
  footnote_header_resolved <- resolve_footnote_removals(x$table_styling$footnote_header)
  footnote_spanning_resolved <- resolve_footnote_removals(x$table_styling$footnote_spanning_header)
  footnote_body_resolved <- resolve_footnote_body(x$table_styling$footnote_body)

  # header (and spanning header) footnotes, numbered first
  spanning_header_lvls <- x$table_styling$spanning_header$level |> append(0L) |> max()
  df_footnote_header <-
    dplyr::bind_rows(
      footnote_header_resolved |> dplyr::mutate(level = 0L),
      footnote_spanning_resolved
    ) |>
    dplyr::mutate(row_numbers = .env$spanning_header_lvls - .data$level + 1L) %>%
    .number_footnotes(x, type = .)

  # body footnotes, numbered after the header footnotes
  df_footnote_body <-
    .number_footnotes(
      x,
      type = footnote_body_resolved,
      start_with = dplyr::n_distinct(df_footnote_header$footnote_id)
    )

  df_footnotes <-
    dplyr::bind_rows(
      dplyr::distinct(df_footnote_header, .data$footnote_id, .data$footnote),
      dplyr::distinct(df_footnote_body, .data$footnote_id, .data$footnote)
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$footnote_id)

  footnote_lines <-
    if (nrow(df_footnotes) > 0L) {
      paste0(
        ref_symbol_for(df_footnotes$footnote_id), " ",
        .strip_markdown(df_footnotes$footnote)
      )
    } else {
      character(0L)
    }

  # source notes
  source_note_lines <-
    if (nrow(x$table_styling$source_note) > 0L) {
      .strip_markdown(x$table_styling$source_note$source_note)
    } else {
      character(0L)
    }

  # abbreviations (single assembled line)
  abbreviation_line <- .assemble_abbreviation_source_note(x)
  abbreviation_lines <-
    if (!is.null(abbreviation_line)) .strip_markdown(abbreviation_line) else character(0L)

  c(footnote_lines, source_note_lines, abbreviation_lines)
}

#' Resolve the flextable default font for the Word header/footer
#'
#' Reads `flextable::get_flextable_defaults()` and returns an `officer::fp_text()`
#' carrying the table body's `font.family` and `font.size`. This lets the Word
#' header/footer regions match the flextable body font instead of falling back to
#' the Word template default. A property is omitted when the corresponding
#' flextable default is missing, so the existing default still applies.
#'
#' @return an `officer::fp_text` object
#' @keywords internal
#' @noRd
# the header font size `as_flex_table()` bakes into every table via
# `flextable::fontsize(part = "header", size = 11)`. kept in sync with that call
# in `R/as_flex_table.R`; used to detect (and ignore) the baked-in header size
# when inheriting the header part font for the Word header region.
.flex_docx_header_default_size <- 11

.flex_docx_default_font <- function() {
  defaults <- flextable::get_flextable_defaults()
  args <- list()
  if (!is.null(defaults$font.family)) args$font.family <- defaults$font.family
  if (!is.null(defaults$font.size)) args$font.size <- defaults$font.size
  do.call(officer::fp_text, args)
}

#' Extract a part's text styling as an `fp_text` property list
#'
#' Reads the per-part text styling stored in `ft[[part]]$styles$text` (an
#' `fpstruct` per `fp_text` property) and returns a named list of
#' `officer::fp_text()` properties, so the corresponding Word region (header
#' \eqn{\leftarrow} flextable header part, footer \eqn{\leftarrow} flextable
#' footer part) can inherit the styling a user applied to the flextable. For each
#' property the first cell value is used (falling back to the property default
#' when the part has cells but no data); only property names accepted by
#' `officer::fp_text()` are used.
#'
#' Two adjustments keep the inherited font sensible:
#'
#' - The sub-family properties (`hansi.family`, `cs.family`, `eastasia.family`)
#'   are unreliable in a flextable part: flextable leaves them at its internal
#'   default even when `font.family` is set, which would emit a Word run with
#'   mismatched ascii/hAnsi fonts. They are collapsed to the extracted
#'   `font.family` so the whole run uses one font.
#' - `as_flex_table()` bakes a fixed header font size
#'   (`.flex_docx_header_default_size`) into every table, independent of the
#'   flextable body/default font. For the header part that baked-in size is
#'   treated as "the default" and dropped, so the body font still flows through;
#'   only an explicitly different header size is inherited.
#'
#' @param ft (`flextable`)\cr the flextable object
#' @param part (`string`)\cr one of `"header"`, `"body"`, `"footer"`
#' @return a named list of `fp_text` properties, or `NULL` when the part has no
#'   rows (or nothing to inherit)
#' @keywords internal
#' @noRd
.flex_docx_part_font <- function(ft, part) {
  # nothing to extract when the part has no rows
  if (flextable::nrow_part(ft, part = part) == 0L) {
    return(NULL)
  }

  text_styles <- ft[[part]]$styles$text
  if (is.null(text_styles)) {
    return(NULL)
  }

  valid_props <- names(formals(officer::fp_text))
  args <- list()
  for (prop in intersect(names(text_styles), valid_props)) {
    st <- text_styles[[prop]]
    vals <- as.vector(st$data)
    value <- if (length(vals) > 0L) vals[[1]] else st$default
    if (!is.null(value) && !is.na(value)) {
      args[[prop]] <- value
    }
  }

  # collapse the (unreliable) sub-family properties onto the extracted
  # `font.family` so the Word run uses a single, consistent font.
  args[["hansi.family"]] <- NULL
  args[["cs.family"]] <- NULL
  args[["eastasia.family"]] <- NULL
  if (!is.null(args[["font.family"]])) {
    args[["hansi.family"]] <- args[["font.family"]]
    args[["cs.family"]] <- args[["font.family"]]
    args[["eastasia.family"]] <- args[["font.family"]]
  }

  # treat `as_flex_table()`'s baked-in header size as the default (see the
  # constant's definition) so the body font size still applies unless the user
  # explicitly changed the header size.
  if (identical(part, "header") &&
    isTRUE(args[["font.size"]] == .flex_docx_header_default_size)) {
    args[["font.size"]] <- NULL
  }

  if (length(args) == 0L) {
    return(NULL)
  }
  args
}

#' Merge the extracted part styling onto the base region font
#'
#' Starting from the base body `officer::fp_text`, merges the properties
#' extracted from the corresponding flextable part (when present), so styling
#' applied to the flextable header/footer flows through to the Word
#' header/footer. The merge overrides the named properties and retains the rest
#' via the `update.fp_text` S3 method. An empty/`NULL` list is skipped.
#'
#' @param base_fp (`fp_text`)\cr the base (table body) font
#' @param extracted_props (`list` or `NULL`)\cr named `fp_text` property list
#'   extracted from the flextable part
#' @return an `officer::fp_text` object
#' @keywords internal
#' @noRd
.flex_docx_region_font <- function(base_fp, extracted_props = NULL) {
  if (length(extracted_props) > 0L) {
    return(do.call(stats::update, c(list(object = base_fp), extracted_props)))
  }
  base_fp
}

#' Build a page-number paragraph for the Word header/footer
#'
#' Parses a glue-like `page` string into an `officer::fpar()`, replacing the
#' `{PAGE}` and `{NUMPAGES}` tokens with live Word fields and emitting all other
#' text verbatim. Any other `{token}` triggers an error.
#'
#' @param page (`string`)\cr the user-supplied page string
#' @param alignment (`string`)\cr one of "left", "center", "right"
#' @param fp_text (`fp_text`)\cr run properties applied to every run (literal
#'   text and the `{PAGE}`/`{NUMPAGES}` fields) so the page line matches the
#'   table body font
#' @return an `officer::fpar` object
#' @keywords internal
#' @noRd
.flex_docx_page_fpar <- function(page, alignment, fp_text = officer::fp_text()) {
  # split into literal segments and `{...}` tokens, keeping the delimiters
  pieces <- str_extract_all(page, "\\{[^}]*\\}|[^{]+")[[1]]

  # validate tokens are PAGE/NUMPAGES only
  tokens <- pieces[str_detect(pieces, "^\\{.*\\}$")]
  token_names <- str_replace_all(tokens, "^\\{|\\}$", "")
  invalid <- setdiff(token_names, c("PAGE", "NUMPAGES"))
  if (!is_empty(invalid)) {
    cli::cli_abort(
      c(
        "The {.arg page} argument contains {?an /}invalid placeholder{?s}:
         {.val {unique(invalid)}}.",
        i = "Only {.val {'{PAGE}'}} and {.val {'{NUMPAGES}'}} are supported."
      ),
      call = get_cli_abort_call()
    )
  }

  # map each piece to an officer run, applying the shared font properties so the
  # page line (including the `{PAGE}`/`{NUMPAGES}` fields) matches the table body
  runs <-
    lapply(pieces, function(piece) {
      switch(piece,
        "{PAGE}" = officer::run_word_field(field = "PAGE", prop = fp_text),
        "{NUMPAGES}" = officer::run_word_field(field = "NUMPAGES", prop = fp_text),
        officer::ftext(piece, prop = fp_text)
      )
    })

  inject(officer::fpar(!!!runs, fp_p = officer::fp_par(text.align = alignment)))
}

#' Strip the small subset of markdown emphasis markers gtsummary supports
#'
#' The Word header/footer regions render plain text, so bold (`**`) and italic
#' (`_`) markers are removed rather than interpreted.
#'
#' @param x (`character`)\cr a character vector
#' @return a character vector with `**` and `_` emphasis markers removed
#' @keywords internal
#' @noRd
.strip_markdown <- function(x) {
  x <- str_replace_all(x, "\\*\\*(.*?)\\*\\*", "\\1")
  x <- str_replace_all(x, "\\_(.*?)\\_", "\\1")
  x
}
