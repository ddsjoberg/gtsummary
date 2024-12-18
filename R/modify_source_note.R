#' Modify source note
#'
#' @description
#' Add and remove source notes from a table.
#' Source notes are similar to footnotes, expect they are not linked to a cell in
#' the table.
#'
#' @param x (`gtsummary`)\cr
#'   A gtsummary object.
#' @param source_note (`string`)\cr
#'   A string to add as a source note.
#' @param source_note_id (`integers`)\cr
#'   Integers specifying the ID of the source note to remove.
#'   Source notes are indexed sequentially at the time of creation.
#' @inheritParams modify
#'
#' @details
#' Source notes are not supported by `as_kable_extra()`.
#'
#'
#' @return gtsummary object
#' @name modify_source_note
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Example 1 ----------------------------------
#' tbl <- tbl_summary(trial, include = c(marker, grade), missing = "no") |>
#'   modify_source_note("Results as of June 26, 2015")
#'
#' # Example 2 ----------------------------------
#' remove_source_note(tbl, source_note_id = 1)
NULL

#' @export
#' @rdname modify_source_note
modify_source_note <- function(x, source_note, text_interpret = c("md", "html")) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_source_note = match.call()))

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(source_note)
  check_class(x, "gtsummary")
  check_string(source_note)
  text_interpret <- arg_match(text_interpret, error_call = get_cli_abort_call())

  # add source note to table_styling -------------------------------------------
  x$table_styling$source_note <-
    dplyr::bind_rows(
      x$table_styling$source_note,
      dplyr::tibble(
        id = nrow(x$table_styling$source_note) + 1L,
        source_note = source_note,
        text_interpret = paste0("gt::", text_interpret),
        remove = FALSE
      )
    )

  # return table ---------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_source_note
remove_source_note <- function(x, source_note_id) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(remove_source_note = match.call()))

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(source_note_id)
  check_class(x, "gtsummary")
  check_integerish(source_note_id, allow_empty = TRUE)

  # mark source note for removal -----------------------------------------------
  if (!is_empty(source_note_id)) {
    if (any(!source_note_id %in% x$table_styling$source_note$id)) {
      cli::cli_abort(
        c("Argument {.arg source_note_id} is out of bounds.",
          i = "Must be one or more of {.val {x$table_styling$source_note$id}} or {.code NULL}."),
        call = get_cli_abort_call()
      )
    }

    x$table_styling$source_note$remove[x$table_styling$source_note$id %in% source_note_id] <- TRUE
  }
  else {
    x$table_styling$source_note$remove <- TRUE
  }

  # return table ---------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
