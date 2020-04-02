# THIS FILE CONTAINS A FEW SCRIPTS THAT ASSIST IN SETTING UP A GENERAL
# GTSUMMARY OBJECT. IF YOU'RE CREATING A GTSUMMARY-LIKE FUNCTION, YOU'LL
# WANT TO GRAB A COPY OF THIS FILE AND PLACE IT IN YOUR PACKAGE.
# LAST UPDATED: 2020-03-24

# table_header_fill_missing -----------------------------------------------------
# this function fills out table_header when there are missing cells
table_header_fill_missing <- function(table_header) {
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
    table_header$align <- ifelse(table_header$column == "label", "left", "center")
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
    mutate(
      label = coalesce(.data$label, .data$column),
      hide = coalesce(.data$hide, TRUE),
      text_interpret = coalesce(.data$text_interpret, "gt::md"),
      align = coalesce(.data$align, "center")
    )

  table_header
}


# table_header_fmt_fun ---------------------------------------------------------
# this function makes it easy to update table_header with new formatting functions
# e.g. table_header_fmt_fun(table_header, p.value = pvalue_fun)
table_header_fmt_fun <- function(table_header, ...) {
  # saving passed_dots arguments as a named list
  passed_dots <- list(...)

  # ordering the names to be the same as in table_header
  names_ordered <- table_header$column %>% intersect(names(passed_dots))
  passed_dots <- passed_dots[names_ordered]

  table_header_update <-
    tibble(
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
      rename(level = .data$by_chr) %>%
      mutate(
        label = glue(stat_by) %>% as.character()
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
    # substitute(list(...))[-1] %>%
    # sapply(I)

    # checking inputs
    if (names(passed_dots) %>% setdiff(names(x$table_body)) %>% length() > 0) {
      stop(glue(
        "{names(passed_dots) %>% setdiff(names(x$table_body)) %>% glue_collapse(sep = ', ')} ",
        "is/are not column names in 'x$table_body'"
      ))
    }
    if (map_lgl(passed_dots, ~ !rlang::is_string(.)) %>% any()) {
      stop("All arguments passed via '...' must be strings of length one.")
    }
    if (map_lgl(passed_dots, ~ stringr::str_detect(., stringr::fixed("{n}"))) %>% any() && is.null(n)) {
      stop("{n} value not available in 'x'")
    }
    if (map_lgl(passed_dots, ~ stringr::str_detect(., stringr::fixed("{N}"))) %>% any() && is.null(N)) {
      stop("{N} value not available in 'x'")
    }

    # applying glue arguments
    passed_dots <- map_chr(passed_dots, ~ stringr::str_glue(.))

    passed_args <- c(passed_args, passed_dots)
  }

  # ordering the names to be the same as in table_header
  names_ordered <- x$table_header$column %>% intersect(names(passed_args))
  passed_args <- passed_args[names_ordered]

  # updating table header update
  table_header_update <-
    tibble(
      column = names(passed_args),
      label = passed_args,
      text_interpret = glue("gt::{text_interpret}") %>% as.character(),
      hide = FALSE
    )

  # applying updates to x$table_header -----------------------------------------
  x$table_header[
    x$table_header$column %in% table_header_update$column, # selecting rows
    c("column", "label", "text_interpret", "hide") # selecting columns
    ] <-
    table_header_update[c("column", "label", "text_interpret", "hide")]

  # # updating gt function calls -------------------------------------------------
  # x$gt_calls[["cols_label"]] <-
  #   table_header_to_gt_cols_label(x$table_header)

  # keeping track of all functions previously run ------------------------------
  if (.save_call == TRUE) {
    x$call_list <- c(x$call_list, list(cols_label_summary = match.call()))
  }

  x
}

