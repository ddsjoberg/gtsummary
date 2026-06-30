#' Modify Abbreviations
#'
#' All abbreviations will be coalesced when printing the final table into
#' a single specialized source note.
#'
#' @inheritParams modify_footnote2
#' @param abbreviation (`string`)\cr
#'   a string. In `remove_abbreviation()`, the default value is `NULL`, which
#'   will remove all abbreviation source notes.
#' @param prefix,sep1,sep2 (`character`)\cr
#'   optional arguments controlling how the abbreviation source note is
#'   assembled as `paste0(prefix, sep1, paste(abbreviations, collapse = sep2))`.
#'   - `prefix` is a character vector of length two giving the leading text, as
#'     `c(singular, plural)`. The first element is used when a single
#'     abbreviation is present and the second when multiple abbreviations are
#'     present. Pass `c("", "")` to omit the leading text. Defaults to `NULL`,
#'     in which case the theme element `"modify_abbreviation-arg:prefix"` is
#'     used, falling back to the translated default
#'     `c("Abbreviation", "Abbreviations")`. Unlike the default, a user-supplied
#'     `prefix` is used as-is and is not translated.
#'   - `sep1` is a string giving the separator between the prefix and the
#'     abbreviations. Defaults to `NULL`, in which case the theme element
#'     `"modify_abbreviation-arg:sep1"` is used, falling back to the default
#'     `": "`. It is omitted when the resolved prefix is empty.
#'   - `sep2` is a string giving the separator placed between abbreviations.
#'     Defaults to `NULL`, in which case the theme element
#'     `"modify_abbreviation-arg:sep2"` is used, falling back to the default
#'     `", "`.
#'
#' @return Updated gtsummary object
#' @name modify_abbreviation
#' @seealso [Footnotes vs Source Notes vs Abbreviations](https://www.danieldsjoberg.com/gtsummary/articles/modify-functions.html)
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("broom", "broom.helpers"))
#' # Example 1 ----------------------------------
#' tbl_summary(
#'   trial,
#'   by = trt,
#'   include = age,
#'   type = age ~ "continuous2"
#' ) |>
#'   modify_table_body(~dplyr::mutate(.x, label = sub("Q1, Q3", "IQR", x = label))) |>
#'   modify_abbreviation("IQR = Interquartile Range")
#'
#' # Example 2 ----------------------------------
#' lm(marker ~ trt, trial) |>
#'   tbl_regression() |>
#'   remove_abbreviation("CI = Confidence Interval")
#'
#' # Example 3 ----------------------------------
#' # customize the prefix and separators of the abbreviation source note
#' tbl_summary(
#'   trial,
#'   by = trt,
#'   include = age,
#'   type = age ~ "continuous2"
#' ) |>
#'   modify_table_body(~dplyr::mutate(.x, label = sub("Q1, Q3", "IQR", x = label))) |>
#'   modify_abbreviation("IQR = Interquartile Range") |>
#'   modify_abbreviation("SD = Standard Deviation") |>
#'   modify_abbreviation(
#'     "N = Number of Observations",
#'     prefix = c("Key", "Keys"),
#'     sep2 = "; "
#'   )
NULL

#' @export
#' @rdname modify_abbreviation
modify_abbreviation <- function(x, abbreviation, text_interpret = c("md", "html", "none"),
                                prefix = NULL, sep1 = NULL, sep2 = NULL) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(abbreviation)
  text_interpret <- arg_match(text_interpret, error_call = get_cli_abort_call())
  if (!is.null(prefix) && (!is.character(prefix) || length(prefix) != 2L)) {
    cli::cli_abort(
      "The {.arg prefix} argument must be a character vector of length 2.",
      call = get_cli_abort_call()
    )
  }
  if (!is.null(sep1)) check_string(sep1)
  if (!is.null(sep2)) check_string(sep2)

  # add updates to `x$table_styling$abbreviation` ------------------------------
  x <- x |>
    .modify_abbreviation(
      abbreviation = abbreviation, text_interpret = text_interpret,
      prefix = prefix, sep1 = sep1, sep2 = sep2
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_abbreviation
remove_abbreviation <- function(x, abbreviation = NULL) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(abbreviation, allow_empty = TRUE)

  # remove all abbreviations if abbreviation=NULL ------------------------------
  if (is_empty(abbreviation)) {
    x$table_styling$abbreviation <-
      .restore_abbreviation_attr(
        x$table_styling$abbreviation[0, ],
        x$table_styling$abbreviation
      )
    return(x)
  }

  # check passed abbreviations for validity
  if (nrow(x$table_styling$abbreviation) == 0L) {
    cli::cli_abort("There are no abbreviations to remove.", call = get_cli_abort_call())
  }
  if (!isTRUE(abbreviation %in% x$table_styling$abbreviation$abbreviation)) {
    cli::cli_abort(
      "The {.arg abbreviation} argument must be one of {.val {unique(x$table_styling$abbreviation$abbreviation)}}.",
      call = get_cli_abort_call()
    )
  }

  # remove abbreviation --------------------------------------------------------
  x$table_styling$abbreviation <-
    x$table_styling$abbreviation |>
    dplyr::filter(!.data$abbreviation %in% .env$abbreviation) |>
    .restore_abbreviation_attr(x$table_styling$abbreviation)

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

# column (`string`)\cr
#  an optional column name from `x$table_body`. When supplied, the abbreviation
#  is tied to a column and it only printed when the column appears in the
#  final printed table. This is primarily used internally.
.modify_abbreviation <- function(x, abbreviation, text_interpret = "md", column = NA_character_,
                                 prefix = NULL, sep1 = NULL, sep2 = NULL) {
  previous <- x$table_styling$abbreviation
  updated <- previous |>
    dplyr::bind_rows(
      dplyr::tibble(
        column = column,
        abbreviation = abbreviation,
        text_interpret = .interpret_fun(text_interpret)
      )
    )

  # carry forward any previously set prefix/sep attributes (dropped by dplyr),
  # then override with the newly supplied (non-NULL) values
  updated <- .restore_abbreviation_attr(updated, previous)
  if (!is.null(prefix)) attr(updated, "prefix") <- prefix
  if (!is.null(sep1)) attr(updated, "sep1") <- sep1
  if (!is.null(sep2)) attr(updated, "sep2") <- sep2

  x$table_styling$abbreviation <- updated
  x
}

# copy the `prefix`/`sep1`/`sep2` attributes from `from` onto `to`; used to
# persist them across the dplyr operations that rebuild the abbreviation tibble
# (dplyr drops non-standard attributes).
.restore_abbreviation_attr <- function(to, from) {
  attr(to, "prefix") <- attr(from, "prefix")
  attr(to, "sep1") <- attr(from, "sep1")
  attr(to, "sep2") <- attr(from, "sep2")
  to
}

# Assemble the abbreviation source note string.
#
# Resolves the prefix and separators using the order: tibble attribute (set via
# `modify_abbreviation()`), then theme element, then built-in default. Only the
# built-in default prefix is translated; attribute/theme values are literal.
# Returns a single string, or NULL when there are no abbreviations.
.assemble_abbreviation_source_note <- function(x) {
  df_abbreviation <- x$table_styling$abbreviation
  if (nrow(df_abbreviation) == 0L) {
    return(NULL)
  }

  # resolve separators: attribute -> theme -> default
  sep1 <- attr(df_abbreviation, "sep1") %||%
    get_theme_element("modify_abbreviation-arg:sep1", default = ": ")
  sep2 <- attr(df_abbreviation, "sep2") %||%
    get_theme_element("modify_abbreviation-arg:sep2", default = ", ")

  # resolve prefix: attribute -> theme -> default
  prefix <- attr(df_abbreviation, "prefix") %||%
    get_theme_element("modify_abbreviation-arg:prefix", default = NULL)
  is_default_prefix <- is.null(prefix)
  if (is_default_prefix) prefix <- c("Abbreviation", "Abbreviations")

  label <- if (nrow(df_abbreviation) > 1L) prefix[2] else prefix[1]
  # only the built-in default prefix is translated; attribute/theme values
  # are used literally
  if (is_default_prefix) label <- translate_string(label)

  joined <- paste(df_abbreviation$abbreviation, collapse = sep2)

  # keep the prefix/sep1 only when there is leading text
  if (nzchar(label)) paste0(label, sep1, joined) else joined
}
