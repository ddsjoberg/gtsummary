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
#' @param addl_cmds (named `list`)\cr
#'   an optional list of additional flextable command expressions applied to the
#'   flextable after it is created from the gtsummary table. Do **not** include
#'   the flextable object argument; each expression is piped onto the flextable
#'   (e.g. `rlang::expr(flextable::bold(part = "header"))`). A **named** entry is
#'   inserted *after* the call of that name; run
#'   `as_flex_table(x, return_calls = TRUE)` to see the available names. An
#'   **unnamed** entry is appended after all existing calls. These are applied
#'   after any commands from the `as_flex_table-lst:addl_cmds` theme element.
#'   Default is `NULL`.
#' @param header_style,footer_style (named `list`)\cr
#'   optional named lists of [`officer::fp_text()`] properties (e.g.
#'   `list(font.size = 8, font.family = "Arial")`) used to style the text in the
#'   Word document's header and footer regions, respectively. By default (when
#'   these are `NULL`) each region inherits the styling of the corresponding
#'   flextable part: the Word header from the flextable header part and the Word
#'   footer from the flextable footer part. For example, applying
#'   `flextable::fontsize(size = 6, part = "footer")` (e.g. via `addl_cmds` or a
#'   theme) yields a size-6 Word footer. (The Word header font size follows the
#'   flextable body font unless the header part size is explicitly changed, since
#'   `as_flex_table()` always sets a fixed header size internally.) Values set
#'   here are merged on top of
#'   that inherited styling and override it, so unspecified properties are
#'   retained. Values set here also override the corresponding
#'   `save_flex_docx-lst:header_style` / `save_flex_docx-lst:footer_style` theme
#'   elements; those theme elements apply only when the flextable part carries no
#'   styling to inherit (e.g. an empty footer). The page-number line adopts the
#'   style of whichever region it is placed in (via `page_location`). Default is
#'   `NULL`.
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
#' # add custom flextable commands and style the footer separately
#' save_flex_docx(
#'   tbl,
#'   path = tempfile(fileext = ".docx"),
#'   addl_cmds = list(rlang::expr(flextable::fontsize(size = 8, part = "all"))),
#'   footer_style = list(font.size = 8, italic = TRUE)
#' )
save_flex_docx <- function(x,
                         path,
                         include = everything(),
                         header = TRUE,
                         footer = TRUE,
                         page = NULL,
                         page_location = c(
                           "footer-right", "footer-center", "footer-left",
                           "header-right", "header-center", "header-left"
                         ),
                         addl_cmds = NULL,
                         header_style = NULL,
                         footer_style = NULL,
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
  if (!is.null(addl_cmds)) check_class(addl_cmds, "list")
  .check_flex_docx_style(header_style, "header_style")
  .check_flex_docx_style(footer_style, "footer_style")
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
      .save_flex_docx_split(
        x,
        path = path,
        include = {{ include }},
        header = header,
        footer = footer,
        page = page,
        page_location = page_location,
        addl_cmds = addl_cmds,
        header_style = header_style,
        footer_style = footer_style
      )
    )
  }

  # single gtsummary table -----------------------------------------------------
  built <-
    .flex_docx_build_one(
      x,
      include = {{ include }},
      header = header,
      footer = footer,
      page = page,
      page_location = page_location,
      addl_cmds = addl_cmds,
      header_style = header_style,
      footer_style = footer_style
    )

  # write the Word file --------------------------------------------------------
  # `prop_section()` is only supplied when a region has content; otherwise
  # `save_as_docx()` uses its default section.
  if (length(built$header_fpars) > 0L || length(built$footer_fpars) > 0L) {
    flextable::save_as_docx(
      built$ft,
      path = path,
      pr_section = .flex_docx_prop_section(built$header_fpars, built$footer_fpars)
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
#' @inheritParams save_flex_docx
#' @return the original `tbl_split` `x` (invisibly)
#' @keywords internal
#' @noRd
.save_flex_docx_split <- function(x, path, include, header, footer, page, page_location,
                                addl_cmds = NULL, header_style = NULL, footer_style = NULL) {
  doc <- officer::read_docx()

  for (i in seq_along(x)) {
    built <-
      .flex_docx_build_one(
        x[[i]],
        include = {{ include }},
        header = header,
        footer = footer,
        page = page,
        page_location = page_location,
        addl_cmds = addl_cmds,
        header_style = header_style,
        footer_style = footer_style
      )

    doc <- flextable::body_add_flextable(doc, built$ft)

    # close this table's section, attaching its own header/footer. `type =
    # "nextPage"` already starts the *next* section on a new page, so no explicit
    # page break is added between tables (doing so would insert a blank page).
    doc <-
      officer::body_end_block_section(
        doc,
        officer::block_section(
          .flex_docx_prop_section(
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
#' @inheritParams save_flex_docx
#' @return a list with elements `ft` (flextable), `header_fpars` (list of
#'   `fpar`), and `footer_fpars` (list of `fpar`)
#' @keywords internal
#' @noRd
.flex_docx_build_one <- function(x, include, header, footer, page, page_location,
                                 addl_cmds = NULL, header_style = NULL, footer_style = NULL) {
  # extract caption and footer content before (optionally) suppressing them
  caption_text <- .flex_docx_caption(x)
  footer_lines <- .flex_docx_footer_lines(x)

  # build the flextable, suppressing the caption and/or footer content that is
  # being relocated to the Word document header/footer regions
  flextable_calls <- as_flex_table(x, include = {{ include }}, return_calls = TRUE)

  if (isTRUE(header)) {
    # caption is relocated to the Word header; drop the flextable caption
    flextable_calls[["set_caption"]] <- NULL
  }

  # insert user-supplied flextable commands. these are applied after the theme
  # `as_flex_table-lst:addl_cmds` commands (already inserted by `as_flex_table()`
  # above). named entries are inserted after the matching call; unnamed entries
  # are appended after all existing calls.
  flextable_calls <- .flex_docx_insert_addl_cmds(flextable_calls, addl_cmds)

  ft <- .eval_list_of_exprs(flextable_calls)

  # extract per-part styling from the flextable so each Word region can inherit
  # it. the footer part must be read *before* it is deleted below. the Word
  # header region inherits from the flextable header part, the Word footer region
  # from the flextable footer part. only inherit when content is actually
  # relocated into the region (a caption for the header, footer lines for the
  # footer): flextable keeps a blank footer row even with no notes, so
  # `nrow_part()` alone would wrongly report content, and inheriting then would
  # shadow the theme style for a region that only holds a page-number line.
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
  # e.g. Cambria). on top of that we merge, in increasing precedence, the theme
  # style, the flextable part's own styling (when present), and finally the
  # argument style. so the argument wins over an explicit part style, which in
  # turn wins over the theme style (the theme style therefore only takes effect
  # when the part carries nothing to inherit).
  base_fp <- .flex_docx_default_font()
  header_fp <- .flex_docx_region_font(
    base_fp,
    theme_props = get_theme_element("save_flex_docx-lst:header_style", eval = TRUE),
    extracted_props = header_extracted,
    arg_props = header_style
  )
  footer_fp <- .flex_docx_region_font(
    base_fp,
    theme_props = get_theme_element("save_flex_docx-lst:footer_style", eval = TRUE),
    extracted_props = footer_extracted,
    arg_props = footer_style
  )

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
#' @return an `officer::prop_section` object
#' @keywords internal
#' @noRd
.flex_docx_prop_section <- function(header_fpars, footer_fpars, ...) {
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
.flex_docx_caption <- function(x) {
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
  footnote_header_resolved <- resolve_footnote_removals(x$table_styling$footnote_header)
  footnote_spanning_resolved <- resolve_footnote_removals(x$table_styling$footnote_spanning_header)
  footnote_body_resolved <- resolve_footnote_removals(x$table_styling$footnote_body)

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

#' Merge header/footer style property lists onto the base region font
#'
#' Starting from the base body `officer::fp_text`, merges (in order) the region's
#' theme property list, the properties extracted from the flextable part, and the
#' argument property list. Later merges win on shared properties, so the
#' precedence is `body font < theme style < extracted part font < argument
#' style`. Each merge overrides the named properties and retains the rest via the
#' `update.fp_text` S3 method. Empty/`NULL` lists are skipped.
#'
#' @param base_fp (`fp_text`)\cr the base (table body) font
#' @param theme_props,extracted_props,arg_props (`list` or `NULL`)\cr named
#'   `fp_text` property lists from the theme element, the flextable part, and the
#'   function argument
#' @return an `officer::fp_text` object
#' @keywords internal
#' @noRd
.flex_docx_region_font <- function(base_fp, theme_props = NULL,
                                   extracted_props = NULL, arg_props = NULL) {
  fp <- base_fp
  for (props in list(theme_props, extracted_props, arg_props)) {
    if (length(props) > 0L) {
      fp <- do.call(stats::update, c(list(object = fp), props))
    }
  }
  fp
}

#' Insert user-supplied flextable commands into the call list
#'
#' Named entries are inserted after the call of the matching name (via
#' `add_expr_after()`); unnamed entries are appended after all existing calls.
#' Entries are processed in list order. Errors when a name is not an existing
#' call name, pointing users to `as_flex_table(x, return_calls = TRUE)`.
#'
#' @param calls (`list`)\cr the named list of flextable call expressions
#' @param addl_cmds (`list` or `NULL`)\cr user commands (named and/or unnamed)
#' @return the updated call list
#' @keywords internal
#' @noRd
.flex_docx_insert_addl_cmds <- function(calls, addl_cmds) {
  if (length(addl_cmds) == 0L) {
    return(calls)
  }

  nms <- names(addl_cmds) %||% rep("", length(addl_cmds))
  for (i in seq_along(addl_cmds)) {
    nm <- nms[i]
    if (!is.na(nm) && nzchar(nm)) {
      if (!nm %in% names(calls)) {
        cli::cli_abort(
          c(
            "Each named element of {.arg addl_cmds} must match a flextable call name.",
            i = "{.val {nm}} is not a valid name.",
            i = "Run {.code as_flex_table(x, return_calls = TRUE)} to see valid names."
          ),
          call = get_cli_abort_call()
        )
      }
      calls <- add_expr_after(
        calls = calls,
        add_after = nm,
        expr = addl_cmds[[i]],
        new_name = paste0("user_added_", i)
      )
    } else {
      calls <- c(calls, set_names(list(addl_cmds[[i]]), paste0("user_added_", i)))
    }
  }

  calls
}

#' Validate a header_style/footer_style argument
#'
#' Must be `NULL` or a fully named list (the names are `fp_text` property names).
#'
#' @param x the argument value
#' @param arg_name (`string`)\cr the argument name for the error message
#' @return `NULL`, invisibly (called for its side effect)
#' @keywords internal
#' @noRd
.check_flex_docx_style <- function(x, arg_name) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  nms <- names(x)
  if (!is.list(x) || is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort(
      "The {.arg {arg_name}} argument must be {.code NULL} or a fully named
       {.cls list} of {.fn officer::fp_text} properties.",
      call = get_cli_abort_call()
    )
  }
  invisible(NULL)
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
