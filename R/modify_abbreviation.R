#' Modify Abbreviations
#'
#' All abbreviations will be coalesced when printing the final table into
#' a single source note.
#'
#' @inheritParams modify_footnote
#' @param abbreviation (`string`)\cr
#'   a string
#'
#' @return Updated gtsummary object
#' @name modify_abbreviation
#'
#' @examples
#' # TODO: Add examples
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
  x$table_styling$abbreviation <- x$table_styling$abbreviation |>
    dplyr::bind_rows(
      dplyr::tibble(
        abbreviation = unlist(lst_footnotes) |> unname(),
        text_interpret = paste0("gt::", text_interpret)
      )
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_footnote
modify_abbreviation <- function(x, abbreviation) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(abbreviation)
  if (!isTRUE(abbreviation %in% x$table_styling$abbreviation$abbreviation)) {
    cli::cli_abort(
      "The {.arg abbreviation} must be one of {.val {unique(x$table_styling$abbreviation$abbreviation)}}.",
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
