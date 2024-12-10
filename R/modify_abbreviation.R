#' Modify Abbreviations
#'
#' All abbreviations will be coalesced when printing the final table into
#' a single source note.
#'
#' @inheritParams modify_footnote2
#' @param abbreviation (`string`)\cr
#'   a string
#'
#' @return Updated gtsummary object
#' @name modify_abbreviation
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"))
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
NULL

#' @export
#' @rdname modify_abbreviation
modify_abbreviation <- function(x, abbreviation, text_interpret = c("md", "html")) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(abbreviation)
  text_interpret <- arg_match(text_interpret, error_call = get_cli_abort_call())

  # add updates to `x$table_styling$abbreviation` ------------------------------
  x <- x |>
    .modify_abbreviation(abbreviation = abbreviation, text_interpret = text_interpret)

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_abbreviation
remove_abbreviation <- function(x, abbreviation) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(abbreviation)
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
    dplyr::filter(!.data$abbreviation %in% .env$abbreviation)

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

# column (`string`)\cr
#  an optional column name from `x$table_body`. When supplied, the abbreviation
#  is tied to a column and it only printed when the column appears in the
#  final printed table. This is primarily used internally.
.modify_abbreviation <- function(x, abbreviation, text_interpret = "md", column = NA_character_) {
  x$table_styling$abbreviation <- x$table_styling$abbreviation |>
    dplyr::bind_rows(
      dplyr::tibble(
        column = column,
        abbreviation = abbreviation,
        text_interpret = paste0("gt::", text_interpret)
      )
    )
  x
}
