# THIS FILE CONTAINS A FEW SCRIPTS THAT ASSIST IN SETTING UP A GENERAL
# GTSUMMARY OBJECT. IF YOU'RE CREATING A GTSUMMARY-LIKE FUNCTION, YOU'LL
# WANT TO GRAB A COPY OF THIS FILE AND PLACE IT IN YOUR PACKAGE.

# LAST UPDATED: 2020-07-30

# table_header_fill_missing -----------------------------------------------------
#' Function fills out table_header when there are missing columns
#'
#' @param table_header A table_header object
#'
#' @return A table_header object
#' @keywords internal
#' @noRd
table_header_fill_missing <- function(table_header, table_body = NULL) {
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

# modify_header_internal -------------------------------------------------------
# The function accepts a complete {gtsummary} object as its input, and returns
# an updated version where the column labels have been added to `.$table_header`.
# The function also switches the default `.$table_header$hide` from
# `TRUE` to `FALSE`, resulting in column with labels being printed.
modify_header_internal <- function(x, vars,  text_interpret = c("md", "html")) {
  # input checks ---------------------------------------------------------------
  text_interpret <- match.arg(text_interpret)

  # information data frame -----------------------------------------------------


}

modify_header_internal <- function(x, stat_by = NULL, ...,
                                   text_interpret = c("md", "html"),
                                   .save_call = FALSE) {
  # input checks ---------------------------------------------------------------
  text_interpret <- match.arg(text_interpret)

  # checking whether input is consistent with by variables
  if (!is.null(stat_by) && inherits(x, "tbl_summary") && is.null(x$by)) {
    stop("'stat_by' argument can only be applied to a 'tbl_summary' object that includes a 'by' argument.")
  }

  # initializing empty passed_args (to be filled later)
  passed_args <- NULL

  # stat_by --------------------------------------------------------------------
  if (!is.null(stat_by)) {
    if (!rlang::is_string(stat_by)) {
      "'stat_by' must be a string of length one."
    }

    # converting input into named list (one item in list per by level)
    stat_by_header <-
      x$df_by %>%
      dplyr::rename(level = .data$by_chr) %>%
      dplyr::mutate(
        label = glue::glue(stat_by) %>% as.character()
      )

    passed_args <- stat_by_header$label
    names(passed_args) <- stat_by_header$by_col
  }

  # mapping over dots and updating labels --------------------------------------
  if (!rlang::is_empty(list(...))) {
    # grabbing N from the gtsummary object
    N <- x$N %||% x$n %||% x$inputs$data
    n <- N

    # saving passed_dots arguments as a named list
    passed_dots <- list(...)

    # checking inputs
    if (names(passed_dots) %>% setdiff(names(x$table_body)) %>% length() > 0) {
      stop(glue::glue(
        "{names(passed_dots) %>% setdiff(names(x$table_body)) %>% glue::glue_collapse(sep = ', ')} ",
        "is/are not column names in 'x$table_body'"
      ))
    }
    if (purrr::map_lgl(passed_dots, ~ !rlang::is_string(.)) %>% any()) {
      stop("All arguments passed via '...' must be strings of length one.")
    }
    if (purrr::map_lgl(passed_dots, ~ stringr::str_detect(., stringr::fixed("{n}"))) %>% any() && is.null(n)) {
      stop("{n} value not available in 'x'")
    }
    if (purrr::map_lgl(passed_dots, ~ stringr::str_detect(., stringr::fixed("{N}"))) %>% any() && is.null(N)) {
      stop("{N} value not available in 'x'")
    }

    # applying glue arguments
    passed_dots <- purrr::map_chr(passed_dots, ~ stringr::str_glue(.))

    passed_args <- c(passed_args, passed_dots)
  }

  # ordering the names to be the same as in table_header
  names_ordered <- x$table_header$column %>% intersect(names(passed_args))
  passed_args <- passed_args[names_ordered]

  # updating table header update
  table_header_update <-
    tibble::tibble(
      column = names(passed_args),
      label = passed_args,
      text_interpret = glue::glue("gt::{text_interpret}") %>% as.character(),
      hide = FALSE
    )

  # applying updates to x$table_header -----------------------------------------
  x$table_header[
    x$table_header$column %in% table_header_update$column, # selecting rows
    c("column", "label", "text_interpret", "hide") # selecting columns
  ] <-
    table_header_update[c("column", "label", "text_interpret", "hide")]

  # keeping track of all functions previously run ------------------------------
  if (.save_call == TRUE) {
    x$call_list <- c(x$call_list, list(cols_label_summary = match.call()))
  }

  x
}
