# THIS FILE CONTAINS A FEW SCRIPTS THAT ASSIST IN SETTING UP A GENERAL
# GTSUMMARY OBJECT. IF YOU'RE CREATING A GTSUMMARY-LIKE FUNCTION, YOU'LL
# WANT TO GRAB A COPY OF THIS FILE AND PLACE IT IN YOUR PACKAGE.

# LAST UPDATED: 2020-12-19

# .create_gtsummary_object -----------------------------------------------------
#' Function uses `table_body` to create a gtsummary object
#'
#' @param table_body the table_body tibble
#' @param ... other objects that will be added to the gtsummary object list
#'
#' @return gtsummary object
.create_gtsummary_object <- function(table_body, ...) {
  x <- list() # empty gtsummary object

  # table_body -----------------------------------------------------------------
  x$table_body <- table_body

  # table_styling --------------------------------------------------------------
  x$table_styling$header <-
    tibble(
      column = names(x$table_body),
      hide = TRUE,
      align = "center",
      interpret_label = "gt::md",
      label = names(x$table_body),
      interpret_spanning_header = "gt::md",
      spanning_header = NA_character_
    ) %>%
    mutate(
      hide = ifelse(.data$column == "label", FALSE, .data$hide),
      align = ifelse(.data$column == "label", "left", .data$align)
    )

  x$table_styling$footnote <-
    tibble(column = character(), rows = character(),
           text_interpret = character(), footnote = character())
  x$table_styling$footnote_abbrev <-
    tibble(column = character(), rows = character(),
           text_interpret = character(), footnote = character())
  x$table_styling$text_format <-
    tibble(column = "label", rows = "row_type != 'label'",
           format_type = "indent", undo_text_format = FALSE)
  x$table_styling$fmt_missing <-
    tibble(column = character(), rows = character(), symbol = character())
  x$table_styling$fmt_fun <-
    tibble(column = character(), rows = character(), fmt_fun = list())

  # adding other objects to list -----------------------------------------------
  x <- c(x, list(...))

  # returning gtsummary object -------------------------------------------------
  class(x) <- "gtsummary"
  x
}

# this fn updates `table_styling` list to match `table_body`
.update_table_styling <- function(x) {

  x$table_styling$header <-
    tibble(
      column = names(x$table_body),
      hide = TRUE,
      align = "center",
      interpret_label = "gt::md",
      label = names(x$table_body),
      interpret_spanning_header = "gt::md",
      spanning_header = NA_character_
    ) %>%
    dplyr::rows_update(
      x$table_styling$header,
      by = "column"
    )

  x
}
