#' Save a gtsummary table to a Word file
#'
#' @description
#' `r lifecycle::badge("experimental")`\cr
#' Save a gtsummary table or a flextable to a Word (`.docx`) file using the
#' flextable package.
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
#' [`flextable::delete_part()`] and the helper [`add_flex_footer_with_field()`].
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
#'   table's footnote region and appends a page-number line (`\(x) x |>
#'   flextable::delete_part(part = "header") |> flextable::delete_part(part =
#'   "body") |> add_flex_footer_with_field() |>
#'   flextable::set_table_properties(layout = "autofit", width = 1)`). The header
#'   default is `NULL` (the caption stays in the body with the table).
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
#' @seealso [`as_flex_table()`], [`add_flex_footer_with_field()`]
#'
#' @examplesIf gtsummary:::is_pkg_installed(c("flextable", "officer"))
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
#' # place a static flextable in the page header
#' hdr <- flextable::flextable(data.frame(x = "Confidential"))
#' save_flex_docx(tbl, path = tempfile(fileext = ".docx"), header = hdr)
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
save_flex_docx <- function(x,
                           path,
                           body = \(x) flextable::delete_part(x, part = "footer"),
                           footer = \(x) {
                             x |>
                               flextable::delete_part(part = "header") |>
                               flextable::delete_part(part = "body") |>
                               add_flex_footer_with_field() |>
                               flextable::set_table_properties(layout = "autofit", width = 1)
                           },
                           header = NULL,
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
  .flex_docx_check_body(body)
  .flex_docx_check_region(header, "header")
  .flex_docx_check_region(footer, "footer")
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
        pr_section = pr_section
      )
    )
  }

  # single gtsummary table or flextable ----------------------------------------
  built <- .flex_docx_build_one(x, body = body, header = header, footer = footer)

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
.save_flex_docx_collection <- function(x, path, body, header, footer, pr_section = NULL) {
  doc <- officer::read_docx()

  for (i in seq_along(x)) {
    built <- .flex_docx_build_one(x[[i]], body = body, header = header, footer = footer)

    doc <- flextable::body_add_flextable(doc, built$body_ft)

    # every section uses the same base geometry, but `type = "nextPage"` is forced
    # so tables page correctly without blank pages (overriding any user `type`).
    section <-
      .flex_docx_prop_section(
        built$header_ft,
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
      # section (a trailing block section would render as an extra blank page).
      doc <- officer::body_set_default_section(doc, section)
    }
  }

  print(doc, target = path)

  invisible(x)
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

#' Add a footer line with Word field codes to a flextable
#'
#' @description
#' Appends a single footer row to a flextable whose text is `footnote`, with any
#' `{token}` replaced by a live Word field code (e.g. `{PAGE}`, `{NUMPAGES}`,
#' `{DATE}`) and all other text rendered verbatim. The new row's alignment is set
#' by `align` and does not affect any existing footer rows. Useful for adding a
#' page-number line to a footer flextable placed in a Word footer region by
#' [`save_flex_docx()`].
#'
#' @param x (`flextable`)\cr a flextable object
#' @param footnote (`string`)\cr the footer text; `{token}` becomes a Word field
#'   code matching `token`. Default is `"Page {PAGE} of {NUMPAGES}"`.
#' @param align (`string`)\cr alignment of the new row, one of `"right"`
#'   (default), `"center"`, or `"left"`
#' @return a `flextable` object
#' @export
#'
#' @examplesIf gtsummary:::is_pkg_installed(c("flextable"))
#' trial |>
#'   tbl_summary(by = trt, include = age) |>
#'   as_flex_table() |>
#'   add_flex_footer_with_field(footnote = "Page {PAGE} of {NUMPAGES}")
add_flex_footer_with_field <- function(x, footnote = "Page {PAGE} of {NUMPAGES}", align = "right") {
  set_cli_abort_call()
  .flex_docx_check_flextable(x)
  check_string(footnote)
  align <- arg_match(align, values = c("right", "center", "left"))

  # split into literal segments and `{...}` field tokens, keeping delimiters
  pieces <- str_extract_all(footnote, "\\{[^}]*\\}|[^{]+")[[1]]
  chunks <-
    lapply(pieces, function(piece) {
      if (str_detect(piece, "^\\{.*\\}$")) {
        flextable::as_word_field(str_replace_all(piece, "^\\{|\\}$", ""))
      } else {
        piece
      }
    })

  x <- flextable::add_footer_lines(x, values = inject(flextable::as_paragraph(!!!chunks)))
  flextable::align(x, i = flextable::nrow_part(x, "footer"), part = "footer", align = align)
}

#' Validate that an object is a flextable
#' @keywords internal
#' @noRd
.flex_docx_check_flextable <- function(x) {
  check_not_missing(x)
  if (!inherits(x, "flextable")) {
    cli::cli_abort(
      "The {.arg x} argument must be a {.cls flextable} object, not
       {.obj_type_friendly {x}}.",
      call = get_cli_abort_call()
    )
  }
}
