#' Save a gtsummary table to a Word file
#'
#' @description
#' `r lifecycle::badge("experimental")`\cr
#' Save a gtsummary table or a flextable to a Word (`.docx`) file using the
#' flextable package.
#'
#' **This function is highly experimental.** Its arguments and behavior are
#' likely to change in future releases, and it may eventually be spun off into a
#' separate package (as this function works with any flextable object in
#' addition to gtsummary tables). Use with that in mind.
#'
#' The table is written into the **body** of the Word document. The `body`,
#' `header`, and `footer` arguments are transformers applied to the (source)
#' flextable to build, respectively, the content placed in the document body and
#' in the Word page **header** and **footer** regions (which repeat on every
#' page). Each is a function of a flextable, a static `flextable`, or `NULL`.
#'
#' By default the footnote region of the table (footnotes, source notes, and
#' abbreviations) is moved out of the body and into the Word footer as a
#' flextable, followed by a right-aligned `"Page X of Y"` line built from live
#' Word fields. Compose your own behavior with flextable functions such as
#' [`flextable::delete_part()`], [`flextable::add_footer_lines()`], and
#' [`flextable::as_word_field()`].
#'
#' A collection of tables is also accepted: a `tbl_split` object (from
#' [`tbl_split_by_rows()`] or [`tbl_split_by_columns()`]), or a plain list of
#' flextables. Each table is written to its own Word section (one table per page)
#' with the `body`/`header`/`footer` transformers applied independently to each.
#'
#' @param x (`gtsummary`, `tbl_split`, `flextable`, or `list`)\cr
#'   a gtsummary table, a `tbl_split` object (a list of gtsummary tables), a
#'   `flextable` object, or a plain list of `flextable` objects
#' @param path (`string`)\cr
#'   file path to write the Word (`.docx`) file to
#' @param body (`function` or `NULL`)\cr
#'   a transformer applied to the source flextable to produce the flextable
#'   placed in the document body. Default `\(x) flextable::delete_part(x, part =
#'   "footer")` removes the footnote region from the body (it is relocated to the
#'   Word footer by the `footer` default). `NULL` uses the source flextable
#'   unchanged.
#' @param footer,header (`function`, `flextable`, or `NULL`)\cr
#'   what to place in the Word page footer/header region: a transformer applied
#'   to the source flextable (returning a `flextable` or `NULL`), a static
#'   `flextable`, or `NULL` for nothing. The footer default keeps only the
#'   table's footnote region (deleting the header and body parts), appends a
#'   right-aligned `"Page X of Y"` line of live Word fields
#'   (`flextable::as_word_field()`), and fits it to the page width. The header
#'   default is `NULL` (the caption stays in the body with the table).
#' @param template (`string`)\cr
#'   an optional file path to a Word (`.docx`) document used as the base for the
#'   output. Its page setup (size, orientation, margins) and body content are
#'   carried through; its header/footer text is not (those regions are managed by
#'   `save_flex_docx()`). See the *Using a Word template* section. Default is
#'   `NULL`.
#' @param pr_section (`officer::prop_section`)\cr
#'   an optional [`officer::prop_section()`] object used as the base Word section,
#'   giving fine-grained control over page margins, page size, orientation, and
#'   section columns (e.g.
#'   `officer::prop_section(page_margins = officer::page_mar(top = 0.5))`). Only
#'   its geometry is used: `save_flex_docx()` always owns the header/footer
#'   regions, so any `header_default`/`footer_default` set on `pr_section` are
#'   ignored. For a collection (`tbl_split` or a list of flextables) the same
#'   geometry is applied to every table's section and the paging `type` is fixed
#'   to `"nextPage"` (any `type` on `pr_section` is ignored) so tables page
#'   correctly. Overrides the `save_flex_docx-lst:pr_section` theme element.
#'   Default is `NULL`.
#' @param ... These dots are for future extensions and must be empty.
#'
#' @export
#' @return the original object `x` (invisibly)
#'
#' @seealso [`as_flex_table()`]
#'
#' @section Using a Word template:
#'
#' The `template` argument accepts a path to a Word (`.docx`) document used as the
#' base for the output. Its **page setup** (size, orientation, margins, section
#' columns) and any **body content** (e.g. a cover page or introductory text) are
#' carried through, with the table written into the body after that content.
#'
#' `save_flex_docx()` **manages the Word header and footer regions itself** (via
#' the `header`/`footer` arguments), so **a template's own header/footer text is
#' not carried through** — whatever `save_flex_docx()` places in a region (or
#' leaves empty) takes precedence and blanks out the template's text there. This
#' is intentional: header/footer text in a template and table placement in the
#' header/footer are **not meant to be mixed**. Because the default `footer`
#' places a table, a template's header/footer text is superseded by default. Put
#' the content you want in the header/footer into the `header`/`footer` arguments
#' rather than into the template.
#'
#' @examplesIf FALSE && gtsummary:::is_pkg_installed(c("flextable", "officer"))
#' theme_gtsummary_compact()
#'
#' # Example 1 ----------------------------------
#' # Default behavior is to place the footnote in the footer and add 'Page X of Y'
#' tbl <-
#'   trial |>
#'   tbl_summary(by = trt, include = c(age, grade)) |>
#'   modify_caption("**Table 1. Patient Characteristics**")
#'
#' # by default the footnotes move to the Word footer with a page-number line
#' save_flex_docx(tbl, path = tempfile(fileext = ".docx"))
#'
#' # keep the whole table (including footnotes) in the body, nothing in the footer
#' save_flex_docx(
#'   tbl,
#'   path = tempfile(fileext = ".docx"),
#'   body = NULL,
#'   footer = NULL
#' )
#'
#' # Example 2 ----------------------------------
#' # This example places a header typically found in the pharmaceutical space,
#' # including protocol number, table title/number, and sub-population label.
#'
#' # place a static report header (with a live "Page X of Y" field) in the header
#' header_ft <-
#'   data.frame(
#'     col1 = c("Protocol: ABC123", NA),
#'     col2 = c("Table 14.3.6 Adverse Event Rates by SOC and PT", "Safety Population"),
#'     col3 = c(NA_character_, NA_character_),
#'     stringsAsFactors = FALSE
#'   ) |>
#'   flextable::flextable() |>
#'   flextable::delete_part(part = "header") |>
#'   flextable::align(j = 1, align = "left", part = "body") |>
#'   flextable::align(j = 2, align = "center", part = "body") |>
#'   flextable::align(j = 3, align = "right", part = "body") |>
#'   flextable::compose(
#'     i = 1, j = 3,
#'     value = flextable::as_paragraph(
#'       "Page ", flextable::as_word_field("PAGE"),
#'       " of ", flextable::as_word_field("NUMPAGES")
#'     ),
#'     part = "body"
#'   ) |>
#'   flextable::border_remove() |>
#'   flextable::fontsize(size = 8, part = "all") |>
#'   flextable::padding(padding.top = 0, padding.bottom = 0, part = "all") |>
#'   flextable::set_table_properties(layout = "autofit", width = 1)
#' save_flex_docx(tbl, path = tempfile(fileext = ".docx"), header = header_ft)
#'
#' # a split table is written with one table per section/page
#' trial |>
#'   tbl_summary(by = trt, include = c(age, marker, grade), missing = ~"no") |>
#'   tbl_split_by_rows(variables = marker) |>
#'   save_flex_docx(path = tempfile(fileext = ".docx"))
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
#'
#' reset_gtsummary_theme()
save_flex_docx <- function(x,
                           path,
                           body = \(x) flextable::delete_part(x, part = "footer"),
                           footer = \(x) {
                             x %>%
                               flextable::delete_part(part = "header") %>%
                               flextable::delete_part(part = "body") %>%
                               flextable::add_footer_lines(
                                 values = flextable::as_paragraph(
                                   "Page ", flextable::as_word_field("PAGE"),
                                   " of ", flextable::as_word_field("NUMPAGES")
                                 )
                               ) %>%
                               flextable::align(
                                 i = flextable::nrow_part(x, "footer"),
                                 part = "footer", align = "right"
                               ) %>%
                               flextable::set_table_properties(layout = "autofit", width = 1)
                           },
                           header = NULL,
                           template = NULL,
                           pr_section = NULL,
                           ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_dots_empty()
  check_not_missing(x)
  check_not_missing(path)

  # resolve argument defaults from theme elements when the caller did not supply
  # them (an explicitly passed argument always wins).
  if (missing(body)) body <- get_theme_element("save_flex_docx-arg:body", default = body)
  if (missing(footer)) footer <- get_theme_element("save_flex_docx-arg:footer", default = footer)
  if (missing(header)) header <- get_theme_element("save_flex_docx-arg:header", default = header)
  if (missing(template)) template <- get_theme_element("save_flex_docx-arg:template", default = template)

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
  .flex_docx_check_body(body)
  .flex_docx_check_region(header, "header")
  .flex_docx_check_region(footer, "footer")
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

  # resolve `pr_section` with argument-over-theme precedence, then validate. only
  # its geometry is used; header/footer defaults are always owned by this function.
  pr_section <- pr_section %||% get_theme_element("save_flex_docx-lst:pr_section", eval = TRUE)
  if (!is.null(pr_section) && !inherits(pr_section, "prop_section")) {
    cli::cli_abort(
      "The {.arg pr_section} argument must be an {.cls officer::prop_section}
       object (e.g. from {.fn officer::prop_section}) or {.code NULL}.",
      call = get_cli_abort_call()
    )
  }

  # collections: one section per table -----------------------------------------
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
        body = body,
        header = header,
        footer = footer,
        template = template,
        pr_section = pr_section
      )
    )
  }

  # single gtsummary table or flextable ----------------------------------------
  built <- .flex_docx_build_one(x, body = body, header = header, footer = footer)

  # when a template is supplied, build the document on top of it (its page setup
  # and any header/footer furniture are carried through; a placed table overrides
  # the corresponding region).
  if (!is.null(template)) {
    .flex_docx_write_one_template(built, path = path, template = template, pr_section = pr_section)
    return(invisible(x))
  }

  # write the Word file. a section is supplied when a region has content, or when
  # the caller passed a `pr_section` (so custom geometry applies even with no
  # header/footer). otherwise `save_as_docx()` uses its default section.
  has_content <- !is.null(built$header_ft) || !is.null(built$footer_ft)
  if (has_content || !is.null(pr_section)) {
    flextable::save_as_docx(
      built$body_ft,
      path = path,
      pr_section = .flex_docx_prop_section(built$header_ft, built$footer_ft, base = pr_section)
    )
  } else {
    flextable::save_as_docx(built$body_ft, path = path)
  }

  invisible(x)
}

#' Write a collection of tables to a single Word file, one section per table
#'
#' Each table is added to the document, separated by a section break, so that
#' each table's header/footer content populates that section's own Word regions.
#'
#' @inheritParams save_flex_docx
#' @return the original collection `x` (invisibly)
#' @keywords internal
#' @noRd
.save_flex_docx_collection <- function(x, path, body, header, footer,
                                       template = NULL, pr_section = NULL) {
  doc <- if (is.null(template)) officer::read_docx() else officer::read_docx(path = template)

  # when a template is supplied and the caller gave no `pr_section`, carry the
  # template's page geometry across every section.
  base_section <- pr_section %||%
    if (!is.null(template)) .flex_docx_section_from_template(doc) else NULL

  for (i in seq_along(x)) {
    built <- .flex_docx_build_one(x[[i]], body = body, header = header, footer = footer)

    doc <- flextable::body_add_flextable(doc, built$body_ft)

    # every section uses the same base geometry, but `type = "nextPage"` is forced
    # so tables page correctly without blank pages (overriding any user `type`).
    section <-
      .flex_docx_prop_section(
        built$header_ft,
        built$footer_ft,
        base = base_section,
        type = "nextPage"
      )

    if (i < length(x)) {
      # close this table's section, attaching its own header/footer. `type =
      # "nextPage"` starts the *next* section on a new page, so no explicit page
      # break is added between tables (that would insert a blank page).
      doc <- officer::body_end_block_section(doc, officer::block_section(section))
    } else {
      # the last table uses the document's default section instead of a block
      # section (a trailing block section would render as an extra blank page).
      doc <- officer::body_set_default_section(doc, section)
    }
  }

  print(doc, target = path)

  invisible(x)
}

#' Write a single table to a Word file on top of a template document
#'
#' Reads the template, adds the body flextable, and sets the default section with
#' the resolved header/footer content. The template's page setup (and any
#' header/footer furniture) is carried through; when a table is placed in a
#' region, that region's default is set, which supersedes the template's content
#' for that region.
#'
#' @param built (`list`)\cr the value from `.flex_docx_build_one()`
#' @inheritParams save_flex_docx
#' @return `NULL`, invisibly
#' @keywords internal
#' @noRd
.flex_docx_write_one_template <- function(built, path, template, pr_section) {
  doc <- officer::read_docx(path = template)
  doc <- flextable::body_add_flextable(doc, built$body_ft)
  base_section <- pr_section %||% .flex_docx_section_from_template(doc)
  doc <- officer::body_set_default_section(
    doc,
    .flex_docx_prop_section(built$header_ft, built$footer_ft, base = base_section)
  )
  print(doc, target = path)

  invisible(NULL)
}

#' Build an `officer::prop_section()` carrying a template's page geometry
#'
#' Reads a template document's page size, orientation, and margins via
#' [`officer::docx_dim()`] and returns a `prop_section` reproducing them (with no
#' header/footer defaults). Used when a `template` is supplied without a
#' `pr_section`: the default section must be (re)set for officer to keep the
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

#' Build the body/header/footer flextables for one table
#'
#' Shared by the single-table and collection paths, for both gtsummary and
#' flextable input. Obtains the source flextable (converting a gtsummary table
#' via [`as_flex_table()`], or using a flextable directly), then applies the
#' `body`/`header`/`footer` transformers independently.
#'
#' @inheritParams save_flex_docx
#' @return a list with elements `body_ft` (flextable), `header_ft`, and
#'   `footer_ft` (each a flextable or `NULL`)
#' @keywords internal
#' @noRd
.flex_docx_build_one <- function(x, body, header, footer) {
  ft <-
    if (inherits(x, "flextable")) {
      x
    } else {
      .eval_list_of_exprs(as_flex_table(x, return_calls = TRUE))
    }

  list(
    body_ft = .flex_docx_apply_body(body, ft),
    header_ft = .flex_docx_resolve_region(header, ft, "header"),
    footer_ft = .flex_docx_resolve_region(footer, ft, "footer")
  )
}

#' Apply the `body` transformer to the source flextable
#'
#' `NULL` returns the flextable unchanged; a function is applied and its result
#' validated to be a flextable.
#'
#' @param body (`function` or `NULL`)\cr the body transformer
#' @param ft (`flextable`)\cr the source flextable
#' @return a `flextable`
#' @keywords internal
#' @noRd
.flex_docx_apply_body <- function(body, ft) {
  if (is.null(body)) {
    return(ft)
  }
  out <- body(ft)
  if (!inherits(out, "flextable")) {
    cli::cli_abort(
      "The {.arg body} function must return a {.cls flextable} object, not
       {.obj_type_friendly {out}}.",
      call = get_cli_abort_call()
    )
  }
  out
}

#' Resolve a `header`/`footer` argument into a flextable (or `NULL`)
#'
#' `NULL` returns `NULL`; a function is applied to the source flextable; a static
#' `flextable` is returned as-is. The result must be a flextable or `NULL`, and a
#' flextable with no rows in any part is treated as `NULL` (rendering a fully
#' empty flextable errors in officer).
#'
#' @param arg (`function`, `flextable`, or `NULL`)\cr the region argument
#' @param ft (`flextable`)\cr the source flextable
#' @param arg_name (`string`)\cr the argument name, for error messages
#' @return a `flextable` or `NULL`
#' @keywords internal
#' @noRd
.flex_docx_resolve_region <- function(arg, ft, arg_name) {
  if (is.null(arg)) {
    return(NULL)
  }
  out <- if (is.function(arg)) arg(ft) else arg
  if (is.null(out)) {
    return(NULL)
  }
  if (!inherits(out, "flextable")) {
    cli::cli_abort(
      "The {.arg {arg_name}} argument must resolve to a {.cls flextable} object
       or {.code NULL}, not {.obj_type_friendly {out}}.",
      call = get_cli_abort_call()
    )
  }
  # a flextable with no rows in any part cannot be rendered; treat as empty
  total_rows <-
    flextable::nrow_part(out, "header") +
    flextable::nrow_part(out, "body") +
    flextable::nrow_part(out, "footer")
  if (total_rows == 0L) {
    return(NULL)
  }
  out
}

#' Validate the `body` argument
#' @keywords internal
#' @noRd
.flex_docx_check_body <- function(body) {
  if (!is.null(body) && !is.function(body)) {
    cli::cli_abort(
      "The {.arg body} argument must be a function or {.code NULL}.",
      call = get_cli_abort_call()
    )
  }
}

#' Validate a `header`/`footer` argument
#' @keywords internal
#' @noRd
.flex_docx_check_region <- function(arg, arg_name) {
  if (!is.null(arg) && !is.function(arg) && !inherits(arg, "flextable")) {
    cli::cli_abort(
      "The {.arg {arg_name}} argument must be a function, a {.cls flextable}
       object, or {.code NULL}.",
      call = get_cli_abort_call()
    )
  }
}

#' Build an `officer::prop_section()` from the header/footer flextables
#'
#' A region is only attached when there is a flextable for it.
#'
#' @param header_ft,footer_ft (`flextable` or `NULL`)\cr the region content
#' @param ... additional arguments passed to `officer::prop_section()` (e.g.
#'   `type`)
#' @param base (`officer::prop_section` or `NULL`)\cr an optional user-supplied
#'   section whose geometry (page margins, size, orientation, columns, and
#'   `type`) is used as the base. Its `header_default`/`footer_default` are always
#'   discarded: `save_flex_docx()` owns those regions.
#' @return an `officer::prop_section` object
#' @keywords internal
#' @noRd
.flex_docx_prop_section <- function(header_ft, footer_ft, base = NULL, ...) {
  section_args <- .flex_docx_base_geometry_args(base)
  section_args <- utils::modifyList(section_args, list(...))
  if (!is.null(header_ft)) {
    section_args$header_default <- officer::block_list(header_ft)
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
