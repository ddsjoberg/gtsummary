#' Save a gtsummary table to a Word file
#'
#' @description
#' Save a gtsummary table to a Word (`.docx`) file using the flextable package.
#'
#' The `header` and `footer` arguments control where the table caption and the
#' footnote-region content (footnotes, source notes, and abbreviations) are
#' placed in the Word document. When `TRUE`, this content is moved to the Word
#' document's page **header** and **footer** regions (where it repeats on every
#' page) instead of appearing in the flow of the table itself. When `FALSE`, the
#' content is rendered as part of the table, matching [`as_flex_table()`].
#'
#' In-cell footnote reference symbols are always retained on the table cells;
#' when `footer = TRUE`, only the footnote *text* is relocated to the Word
#' document footer.
#'
#' A `tbl_split` object (from [`tbl_split_by_rows()`] or
#' [`tbl_split_by_columns()`]) is also accepted. Each table in the split is
#' written to its own Word section, separated by a page break, so that each
#' table's caption and footnote-region content populate that section's own
#' header/footer regions.
#'
#' @param x (`gtsummary` or `tbl_split`)\cr
#'   a gtsummary table, or a `tbl_split` object (a list of gtsummary tables)
#' @param path (`string`)\cr
#'   file path to write the Word (`.docx`) file to
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to include in the output. Default is `everything()`.
#' @param header (scalar `logical`)\cr
#'   whether to place the table caption in the Word document's page header
#'   region. When `FALSE`, the caption is rendered as the table caption. Default
#'   is `TRUE`.
#' @param footer (scalar `logical`)\cr
#'   whether to place the footnotes, source notes, and abbreviations in the Word
#'   document's page footer region. When `FALSE`, this content is rendered in the
#'   table's footer. Default is `TRUE`.
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
#' @param ... These dots are for future extensions and must be empty.
#'
#' @export
#' @return the original gtsummary table `x` (invisibly)
#'
#' @seealso [`as_flex_table()`]
#'
#' @examplesIf gtsummary:::is_pkg_installed(c("flextable", "officer"))
#' tbl <-
#'   trial |>
#'   tbl_summary(by = trt, include = c(age, grade)) |>
#'   modify_caption("**Table 1. Patient Characteristics**")
#'
#' # save the table, placing caption in the header and notes in the footer
#' as_flex_word(tbl, path = tempfile(fileext = ".docx"))
#'
#' # add a "Page X of Y" line to the footer
#' as_flex_word(
#'   tbl,
#'   path = tempfile(fileext = ".docx"),
#'   page = "Page {PAGE} of {NUMPAGES}"
#' )
#'
#' # a split table is written with one table per section/page
#' trial |>
#'   tbl_summary(by = trt, include = c(age, marker, grade)) |>
#'   tbl_split_by_rows(variables = c(age, marker)) |>
#'   as_flex_word(path = tempfile(fileext = ".docx"))
as_flex_word <- function(x,
                         path,
                         include = everything(),
                         header = TRUE,
                         footer = TRUE,
                         page = NULL,
                         page_location = c(
                           "footer-right", "footer-center", "footer-left",
                           "header-right", "header-center", "header-left"
                         ),
                         ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_dots_empty()
  check_not_missing(x)
  check_not_missing(path)
  if (!inherits(x, c("gtsummary", "tbl_split"))) {
    cli::cli_abort(
      "The {.arg x} argument must be a {.cls gtsummary} or {.cls tbl_split} object.",
      call = get_cli_abort_call()
    )
  }
  check_string(path)
  check_scalar_logical(header)
  check_scalar_logical(footer)
  if (!is.null(page)) check_string(page)
  page_location <- arg_match(page_location)
  check_pkg_installed(c("flextable", "officer"))

  # tbl_split: one section (with its own header/footer) per table --------------
  if (inherits(x, "tbl_split")) {
    if (length(x) == 0L) {
      cli::cli_abort(
        "The {.arg x} argument is an empty {.cls tbl_split} with no tables to write.",
        call = get_cli_abort_call()
      )
    }
    return(
      .as_flex_word_split(
        x,
        path = path,
        include = {{ include }},
        header = header,
        footer = footer,
        page = page,
        page_location = page_location
      )
    )
  }

  # single gtsummary table -----------------------------------------------------
  built <-
    .flex_word_build_one(
      x,
      include = {{ include }},
      header = header,
      footer = footer,
      page = page,
      page_location = page_location
    )

  # write the Word file --------------------------------------------------------
  # `prop_section()` is only supplied when a region has content; otherwise
  # `save_as_docx()` uses its default section.
  if (length(built$header_fpars) > 0L || length(built$footer_fpars) > 0L) {
    flextable::save_as_docx(
      built$ft,
      path = path,
      pr_section = .flex_word_prop_section(built$header_fpars, built$footer_fpars)
    )
  } else {
    flextable::save_as_docx(built$ft, path = path)
  }

  invisible(x)
}

#' Write a `tbl_split` to a single Word file, one section per table
#'
#' Each table is added to the document, separated by a page break, and closed by
#' a section break so that each table's caption/notes populate that section's own
#' Word header/footer regions.
#'
#' @inheritParams as_flex_word
#' @return the original `tbl_split` `x` (invisibly)
#' @keywords internal
#' @noRd
.as_flex_word_split <- function(x, path, include, header, footer, page, page_location) {
  doc <- officer::read_docx()

  for (i in seq_along(x)) {
    built <-
      .flex_word_build_one(
        x[[i]],
        include = {{ include }},
        header = header,
        footer = footer,
        page = page,
        page_location = page_location
      )

    # page break before every table except the first
    if (i > 1L) {
      doc <- officer::body_add_break(doc)
    }

    doc <- flextable::body_add_flextable(doc, built$ft)

    # close this table's section, attaching its own header/footer. `type =
    # "nextPage"` starts each section on a new page.
    doc <-
      officer::body_end_block_section(
        doc,
        officer::block_section(
          .flex_word_prop_section(
            built$header_fpars,
            built$footer_fpars,
            type = "nextPage"
          )
        )
      )
  }

  print(doc, target = path)

  invisible(x)
}

#' Build the flextable and header/footer paragraph lists for one gtsummary table
#'
#' Shared by the single-table and `tbl_split` paths. Builds the flextable (with
#' the caption and/or footer content suppressed when relocated), and assembles
#' the Word header/footer paragraph lists (caption/notes plus the optional
#' page-number line).
#'
#' @inheritParams as_flex_word
#' @return a list with elements `ft` (flextable), `header_fpars` (list of
#'   `fpar`), and `footer_fpars` (list of `fpar`)
#' @keywords internal
#' @noRd
.flex_word_build_one <- function(x, include, header, footer, page, page_location) {
  # extract caption and footer content before (optionally) suppressing them
  caption_text <- .flex_word_caption(x)
  footer_lines <- .flex_word_footer_lines(x)

  # build the flextable, suppressing the caption and/or footer content that is
  # being relocated to the Word document header/footer regions
  flextable_calls <- as_flex_table(x, include = {{ include }}, return_calls = TRUE)

  if (isTRUE(header)) {
    # caption is relocated to the Word header; drop the flextable caption
    flextable_calls[["set_caption"]] <- NULL
  }

  ft <- .eval_list_of_exprs(flextable_calls)

  if (isTRUE(footer)) {
    # footnote text, source notes, and abbreviations are relocated to the Word
    # footer. Deleting the flextable footer part removes this text while the
    # in-cell footnote reference symbols (set on the header/body parts by the
    # `flextable::footnote()` calls) are retained.
    if (flextable::nrow_part(ft, part = "footer") > 0L) {
      ft <- flextable::delete_part(ft, part = "footer")
    }
  }

  # resolve the font used by the flextable body so the Word header/footer
  # regions match it (they would otherwise fall back to the Word template
  # default, e.g. Cambria).
  fp_text <- .flex_word_default_font()

  # assemble the Word header/footer paragraph lists. content order is
  # caption/notes first, then the optional page-number line as a separate
  # paragraph.
  header_fpars <- list()
  footer_fpars <- list()

  if (isTRUE(header) && !is.null(caption_text)) {
    header_fpars <- c(header_fpars, list(officer::fpar(officer::ftext(caption_text, prop = fp_text))))
  }
  if (isTRUE(footer) && length(footer_lines) > 0L) {
    footer_fpars <- c(footer_fpars, lapply(footer_lines, \(line) officer::fpar(officer::ftext(line, prop = fp_text))))
  }

  # optional page-number line (independent of the header/footer flags)
  if (!is.null(page)) {
    page_region <- sub("-.*$", "", page_location)
    page_align <- sub("^.*-", "", page_location)
    page_fpar <- .flex_word_page_fpar(page, alignment = page_align, fp_text = fp_text)
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
#' @return an `officer::prop_section` object
#' @keywords internal
#' @noRd
.flex_word_prop_section <- function(header_fpars, footer_fpars, ...) {
  section_args <- list(...)
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
.flex_word_caption <- function(x) {
  caption <- x$table_styling$caption
  if (is.null(caption) || !nzchar(caption)) {
    return(NULL)
  }
  .strip_markdown(caption)
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
.flex_word_footer_lines <- function(x) {
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

  # header (and spanning header) footnotes, numbered first
  spanning_header_lvls <- x$table_styling$spanning_header$level |> append(0L) |> max()
  df_footnote_header <-
    dplyr::bind_rows(
      x$table_styling$footnote_header |> dplyr::mutate(level = 0L),
      x$table_styling$footnote_spanning_header
    ) |>
    dplyr::mutate(row_numbers = .env$spanning_header_lvls - .data$level + 1L) %>%
    .number_footnotes(x, type = .)

  # body footnotes, numbered after the header footnotes
  df_footnote_body <-
    .number_footnotes(
      x,
      type = x$table_styling$footnote_body,
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
.flex_word_default_font <- function() {
  defaults <- flextable::get_flextable_defaults()
  args <- list()
  if (!is.null(defaults$font.family)) args$font.family <- defaults$font.family
  if (!is.null(defaults$font.size)) args$font.size <- defaults$font.size
  do.call(officer::fp_text, args)
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
.flex_word_page_fpar <- function(page, alignment, fp_text = officer::fp_text()) {
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
