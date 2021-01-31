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
    tibble(column = character(), rows = character(),
           format_type = character(), undo_text_format = logical())
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

# table_header_fill_missing ----------------------------------------------------
#' Function fills out table_header when there are missing columns
#'
#' @param table_header A table_header object
#'
#' @return A table_header object
#' @keywords internal
#' @noRd
table_header_fill_missing <- function(table_header, table_body = NULL) {
  warning("Someone ran `table_header_fill_missing()` !!!! Ewww!")

  # if table_body is not null,
  # ensuring table_header has a row for each col in table_body
  if (!is.null(table_body)) {
    table_header <-
      tibble::tibble(column = names(table_body)) %>%
      dplyr::left_join(table_header, by = "column")
  }

  # table_header must be a tibble with the following columns with
  # at minimum a column named 'column'

  # label ----------------------------------------------------------------------
  if (!"label" %in% names(table_header)) {
    table_header$label <- table_header$column
  }

  # hide -----------------------------------------------------------------------
  # lgl vector
  if (!"hide" %in% names(table_header)) {
    table_header$hide <- TRUE
  }

  # align ----------------------------------------------------------------------
  if (!"align" %in% names(table_header)) {
    table_header$align <-
      ifelse(table_header$column %in% c("label", "groupname_col"), "left", "center")
  }

  # missing_emdash -------------------------------------------------------------
  # results in logical vector indicating which missing cells to replace with emdash
  if (!"missing_emdash" %in% names(table_header)) {
    table_header$missing_emdash <- NA_character_
  }

  # indent ---------------------------------------------------------------------
  # results in logical vector indicating which cells to indent in table_body
  if (!"indent" %in% names(table_header)) {
    table_header$indent <- ifelse(table_header$column == "label",
                                  "row_type != 'label'", NA_character_)
  }

  # text_interpret -------------------------------------------------------------
  # currently defaults to `gt::md` as the only option
  if (!"text_interpret" %in% names(table_header)) {
    table_header$text_interpret <- "gt::md"
  }

  # bold -----------------------------------------------------------------------
  # results in logical vector indicating which cells to bold
  if (!"bold" %in% names(table_header)) {
    table_header$bold <- NA_character_
  }

  # italic ---------------------------------------------------------------------
  # results in logical vector indicating which cells to bold
  if (!"italic" %in% names(table_header)) {
    table_header$italic <- NA_character_
  }

  # fmt_fun --------------------------------------------------------------------
  # list of functions that format the column
  if (!"fmt_fun" %in% names(table_header)) {
    table_header$fmt_fun <- list(NULL)
  }

  # footnote_abbrev ------------------------------------------------------------
  if (!"footnote_abbrev" %in% names(table_header)) {
    table_header$footnote_abbrev <- NA_character_
  }

  # footnote -------------------------------------------------------------------
  if (!"footnote" %in% names(table_header)) {
    table_header$footnote <- NA_character_
  }

  # spanning_header ------------------------------------------------------------
  if (!"spanning_header" %in% names(table_header)) {
    table_header$spanning_header <- NA_character_
  }

  # filling in missing values with default -------------------------------------
  table_header <-
    table_header %>%
    dplyr::mutate(
      label = dplyr::coalesce(.data$label, .data$column),
      hide = dplyr::coalesce(.data$hide, TRUE),
      text_interpret = dplyr::coalesce(.data$text_interpret, "gt::md"),
      align = dplyr::coalesce(.data$align, "center")
    )

  table_header
}


# table_header_fmt_fun ---------------------------------------------------------
# this function makes it easy to update table_header with new formatting functions
# e.g. table_header_fmt_fun(table_header, p.value = pvalue_fun)
#' Function makes it easy to update table_header with new formatting functions
#'
#' @param table_header A `table_header` object
#' @param ... The name of the arg is a column name, and the value is a function
#'
#' @return A `table_header` object
#' @keywords internal
#' @noRd
#' @examples
#' table_header_fmt_fun(
#'   table_header,
#'   p.value = style_pvalue,
#'   estimate = style_sigfig
#' )
table_header_fmt_fun <- function(table_header, ...) {
  warning("Someone ran `table_header_fmt_fun()` !!!! Ewww!")

    # saving passed_dots arguments as a named list
  passed_dots <- list(...)

  # ordering the names to be the same as in table_header
  names_ordered <- table_header$column %>% intersect(names(passed_dots))
  passed_dots <- passed_dots[names_ordered]

  table_header_update <-
    tibble::tibble(
      column = table_header$column %>% intersect(names(passed_dots)),
      fmt_fun = passed_dots
    )

  # updating table_header
  table_header[
    table_header$column %in% table_header_update$column, # selecting rows
    c("column", "fmt_fun") # selecting columns
  ] <- table_header_update[c("column", "fmt_fun")]

  table_header
}
