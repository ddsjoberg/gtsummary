#' Modify Footnotes
#'
#' @inheritParams modify
#' @param footnote (`string`)\cr
#'   a string
#' @param columns ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to add footnote
#' @param rows (predicate `expression`)\cr
#'   Predicate expression to select rows in `x$table_body`.
#'   Review [rows argument details][rows_argument].
#'
#' @return Updated gtsummary object
#' @name modify_footnote
#'
#' @examples
#' # TODO: Add examples
NULL

#' @export
#' @rdname modify_footnote
modify_footnote_header <- function(x, footnote, columns, text_interpret = c("md", "html")) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_header = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(footnote)
  text_interpret <- arg_match(text_interpret, error_call = get_cli_abort_call())

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  # evaluate the strings with glue ---------------------------------------------
  lst_footnotes <- .evaluate_string_with_glue(x, rep_named(columns, list(footnote)))

  # add updates to `x$table_styling$footnote_header` ---------------------------
  x <-
    .modify_footnote_header(
      x,
      lst_footnotes = lst_footnotes,
      text_interpret = text_interpret,
      remove = FALSE
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_footnote
modify_footnote_body <- function(x, footnote, columns, rows, text_interpret = c("md", "html")) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(footnote)
  text_interpret <- arg_match(text_interpret, error_call = get_cli_abort_call())
  .check_rows_input(x, {{ rows }})

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  # evaluate the strings with glue ---------------------------------------------
  lst_footnotes <- .evaluate_string_with_glue(x, rep_named(columns, list(footnote)))

  # add updates to `x$table_styling$footnote_body` -----------------------------
  x <-
    .modify_footnote_body(
      x,
      lst_footnotes = lst_footnotes,
      rows = {{ rows }},
      text_interpret = text_interpret,
      remove = FALSE
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_footnote
remove_footnote_header <- function(x, columns) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(remove_footnote_header = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  # add updates to `x$table_styling$footnote_header` ---------------------------
  x <-
    .modify_footnote_header(
      x,
      lst_footnotes = rep_named(columns, list(NA_character_)),
      remove = TRUE
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname modify_footnote
remove_footnote_body <- function(x, columns, rows) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(remove_footnote_body = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  .check_rows_input(x, {{ rows }})

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  # add updates to `x$table_styling$footnote_body` -----------------------------
  x <-
    .modify_footnote_body(
      x,
      lst_footnotes = rep_named(columns, list(NA_character_)),
      rows = {{ rows }},
      remove = TRUE
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}

# this checks the rows argument evaluates to a lgl in `x$table_body`
.check_rows_input <- function(x, rows) {
  rows <- enquo(rows)
  # check rows evaluates to a logical
  rows_eval_error <-
    tryCatch(
      eval_tidy(rows, data = x$table_body) %>%
        {!is.logical(.)}, # styler: off
      error = function(e) TRUE
    )

  if (rows_eval_error) {
    cli::cli_abort(
      "The {.arg rows} argument must be an expression that evaluates to a logical vector in {.code x$table_body}.",
      call = get_cli_abort_call()
    )
  }

  invisible()
}

# modify_footnote_*() for internal use (no checking of inputs) -----------------
.modify_footnote_header <- function(x, lst_footnotes, text_interpret = "md", remove = FALSE) {
  # add updates to `x$table_styling$footnote_header` ---------------------------
  x$table_styling$footnote_header <- x$table_styling$footnote_header |>
    dplyr::bind_rows(
      dplyr::tibble(
        column = names(lst_footnotes),
        footnote = unlist(lst_footnotes) |> unname(),
        text_interpret = paste0("gt::", text_interpret),
        remove = remove
      )
    )

  # return table ---------------------------------------------------------------
  x
}

.modify_footnote_body <- function(x, lst_footnotes, rows, text_interpret = "md", remove = FALSE) {
  # add updates to `x$table_styling$footnote_body` -----------------------------
  x$table_styling$footnote_body <- x$table_styling$footnote_body |>
    dplyr::bind_rows(
      dplyr::tibble(
        column = names(lst_footnotes),
        rows = list(enquo(rows)),
        footnote = unlist(lst_footnotes) |> unname(),
        text_interpret = paste0("gt::", text_interpret),
        remove = remove
      )
    )

  # return table ---------------------------------------------------------------
  x
}
