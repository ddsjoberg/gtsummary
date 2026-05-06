#' Create gtsummary table
#'
#' This function ingests a data frame and adds the infrastructure around it
#' to make it a gtsummary object.
#'
#' Function uses `table_body` to create a gtsummary object
#'
#' @param table_body (`data.frame`)\cr
#'   a data frame that will be added as the gtsummary object's `table_body`
#' @param ... other objects that will be added to the gtsummary object list
#'
#' @export
#' @return gtsummary object
#'
#' @examples
#' mtcars[1:2, 1:2] |>
#'   as_gtsummary()
as_gtsummary <- function(table_body, ...) {
  set_cli_abort_call()
  check_data_frame(table_body)
  x <- list() # empty gtsummary object

  # table_body -----------------------------------------------------------------
  x$table_body <- table_body

  # table_styling --------------------------------------------------------------
  cn <- names(x$table_body)
  nc <- length(cn)

  x$table_styling$header <-
    vctrs::new_data_frame(list(
      column = cn,
      hide = rep(FALSE, nc),
      align = ifelse(cn == "label", "left", "center"),
      interpret_label = rep("gt::md", nc),
      label = unname(vapply(cn, function(col) attr(table_body[[col]], "label") %||% col, character(1)))
    ))

  ec <- character(0L)
  el <- logical(0L)
  ei <- integer(0L)

  x$table_styling$spanning_header <-
    vctrs::new_data_frame(list(level = ei, column = ec, spanning_header = ec, text_interpret = ec, remove = el))

  x$table_styling$footnote_header <-
    vctrs::new_data_frame(list(column = ec, footnote = ec, text_interpret = ec, replace = el, remove = el))

  x$table_styling$footnote_body <-
    vctrs::new_data_frame(list(column = ec, rows = list(), footnote = ec, text_interpret = ec, replace = el, remove = el))

  x$table_styling$footnote_spanning_header <-
    vctrs::new_data_frame(list(column = ec, footnote = ec, level = ei, text_interpret = ec, replace = el, remove = el))

  x$table_styling$abbreviation <-
    vctrs::new_data_frame(list(column = ec, abbreviation = ec, text_interpret = ec))

  x$table_styling$source_note <-
    vctrs::new_data_frame(list(id = ei, source_note = ec, text_interpret = ec, remove = el))

  x$table_styling$text_format <-
    vctrs::new_data_frame(list(column = ec, rows = list(), format_type = ec, undo_text_format = el))

  x$table_styling$indent <-
    # if there is a label column, make it indent 0 (which makes it easier to modify later)
    if ("label" %in% cn) {
      vctrs::new_data_frame(list(column = "label", rows = list(rlang::expr(TRUE)), n_spaces = 0L))
    } else {
      vctrs::new_data_frame(list(column = ec, rows = list(), n_spaces = ei))
    }

  x$table_styling$fmt_missing <-
    vctrs::new_data_frame(list(column = ec, rows = list(), symbol = ec))
  x$table_styling$fmt_fun <-
    vctrs::new_data_frame(list(column = ec, rows = list(), fmt_fun = list()))
  x$table_styling$cols_merge <-
    vctrs::new_data_frame(list(column = ec, rows = list(), pattern = ec))
  x$table_styling$post_fmt_fun <-
    vctrs::new_data_frame(list(column = ec, rows = list(), fmt_fun = list()))

  # adding other objects to list -----------------------------------------------
  x <- c(x, list(...))

  # returning gtsummary object -------------------------------------------------
  x |>
    structure(class = "gtsummary")
}
