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
#' @export
#' @keywords internal
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
    tibble(
      column = character(), rows = list(),
      text_interpret = character(), footnote = character()
    )
  x$table_styling$footnote_abbrev <-
    tibble(
      column = character(), rows = list(),
      text_interpret = character(), footnote = character()
    )
  x$table_styling$text_format <-
    .purrr_when(
      isTRUE(all(c("label", "row_type") %in% x$table_styling$header$column)) ~
        tibble(
          column = "label",
          rows = list(rlang::expr(.data$row_type %in% c("level", "missing"))),
          format_type = "indent", undo_text_format = FALSE
        ),
      isFALSE(all(c("label", "row_type") %in% x$table_styling$header$column)) ~
        tibble(
          column = character(), rows = list(),
          format_type = character(), undo_text_format = logical()
        )
    )

  x$table_styling$fmt_missing <-
    tibble(column = character(), rows = list(), symbol = character())
  x$table_styling$fmt_fun <-
    tibble(column = character(), rows = list(), fmt_fun = list())
  x$table_styling$cols_merge <-
    tibble(column = character(), rows = list(), pattern = character())

  # adding other objects to list -----------------------------------------------
  x <- c(x, list(...))

  # returning gtsummary object -------------------------------------------------
  class(x) <- "gtsummary"
  x
}

# this fn updates `table_styling` list to match `table_body`
.update_table_styling <- function(x) {
  # vector of columns deleted in update
  deleted_columns <-
    x$table_styling$header$column %>%
    setdiff(names(x$table_body))

  # if a column was deleted, omit all styling instructions for that column -----
  if (!is_empty(deleted_columns)) {
    for (styling_element in names(x$table_styling)) {
      # if element is a tibble with a column called 'column'
      if (is.data.frame(x$table_styling[[styling_element]]) &&
        "column" %in% names(x$table_styling[[styling_element]])) {
        x$table_styling[[styling_element]] <-
          x$table_styling[[styling_element]] %>%
          filter(!.data$column %in% deleted_columns)
      }
    }
  }

  # update styling header table with new variables -----------------------------
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
    .rows_update_table_styling_header(x$table_styling$header)

  # return x -------------------------------------------------------------------
  x
}

.rows_update_table_styling_header <- function(x, y) {
  common_columns <- intersect(names(x), names(y))

  x %>%
    # updating rows in header
    dplyr::rows_update(
      y %>% select(all_of(common_columns)),
      by = "column"
    ) %>%
    # re-adding the columns not in the original header table
    dplyr::left_join(
      y %>% select(-all_of(setdiff(common_columns, "column"))),
      by = "column"
    )
}


.purrr_when <- function(...) {
  lst_formulas <- rlang::dots_list(...)

  for (i in seq_len(length(lst_formulas))) {
    if (isTRUE(eval_tidy(.f_lhs_as_quo(lst_formulas[[i]])))) {
      return(eval_tidy(.f_rhs_as_quo(lst_formulas[[i]])))
    }
  }
  # if not matches, return NULL
  NULL
}
.f_lhs_as_quo <- function(x) {
  rlang::new_quosure(
    expr = rlang::f_lhs(x),
    env = attr(x, ".Environment")
  )
}
.f_rhs_as_quo <- function(x) {
  rlang::new_quosure(
    expr = rlang::f_rhs(x),
    env = attr(x, ".Environment")
  )
}
