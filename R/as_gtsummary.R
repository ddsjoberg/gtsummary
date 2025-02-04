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
  x$table_styling$header <-
    dplyr::tibble(
      column = names(x$table_body),
      hide = FALSE,
      align = "center",
      interpret_label = "gt::md"
    ) %>%
    dplyr::mutate(
      label = map_chr(.data$column, ~attr(table_body[[.x]], "label") %||% .x),
      align = ifelse(.data$column %in% "label", "left", .data$align)
    )

  x$table_styling$spanning_header <-
    dplyr::tibble(
      level = integer(),
      column = character(),
      spanning_header = character(),
      text_interpret = character(),
      remove = logical()
    )

  x$table_styling$footnote_header <-
    dplyr::tibble(
      column = character(),
      footnote = character(), text_interpret = character(),
      replace = logical(), remove = logical()
    )

  x$table_styling$footnote_body <-
    dplyr::tibble(
      column = character(), rows = list(),
      footnote = character(), text_interpret = character(),
      replace = logical(), remove = logical()
    )

  x$table_styling$footnote_spanning_header <-
    dplyr::tibble(
      column = character(), footnote = character(),
      level = integer(), text_interpret = character(),
      replace = logical(), remove = logical()
    )

  x$table_styling$abbreviation <-
    dplyr::tibble(
      column = character(),
      abbreviation = character(),
      text_interpret = character()
    )

  x$table_styling$source_note <-
    dplyr::tibble(
      id = integer(),
      source_note = character(),
      text_interpret = character(),
      remove = logical()
    )

  x$table_styling$text_format <-
    dplyr::tibble(
      column = character(), rows = list(),
      format_type = character(), undo_text_format = logical()
    )

  x$table_styling$indent <-
    # if there is a label column, make it indent 0 (which makes it easier to modify later)
    if ("label" %in% x$table_styling$header$column) {
      dplyr::tibble(
        column = "label",
        rows = list(rlang::expr(TRUE)),
        n_spaces = 0L
      )
    } else {
      dplyr::tibble(column = character(), rows = list(), n_spaces = integer())
    }

  x$table_styling$fmt_missing <-
    dplyr::tibble(column = character(), rows = list(), symbol = character())
  x$table_styling$fmt_fun <-
    dplyr::tibble(column = character(), rows = list(), fmt_fun = list())
  x$table_styling$cols_merge <-
    dplyr::tibble(column = character(), rows = list(), pattern = character())

  # adding other objects to list -----------------------------------------------
  x <- c(x, list(...))

  # returning gtsummary object -------------------------------------------------
  x |>
    structure(class = "gtsummary")
}
