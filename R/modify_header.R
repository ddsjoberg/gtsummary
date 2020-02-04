#' Modify column headers in gtsummary tables
#'
#' Column labels can be modified to include calculated statistics;
#' e.g. the N can be dynamically included by wrapping it in curly brackets
#' (following [glue::glue] syntax).
#'
#' @param x gtsummary object, e.g. `tbl_summary` or `tbl_regression`
#' @param stat_by String specifying text to include above the summary statistics
#' stratified by a variable.  Only use with stratified `tbl_summary` objects.
#' The following fields are available for use in the
#' headers:
#' * `{n}` number of observations in each group,
#' * `{N}` total number of observations,
#' * `{p}` percentage in each group,
#' * `{level}` the 'by' variable level,
#' * `"fisher.test"` for a Fisher's exact test,
#'
#' Syntax follows [glue::glue],
#' e.g. `stat_by = "**{level}**, N = {n} ({style_percent(p)\%})"`.
#' The `by` argument from the parent `tbl_summary()` cannot be `NULL`.
#' @param ... Specifies column label of any other column in `.$table_body`.
#' Argument is the column name, and the value is the new column header
#' (e.g. `p.value = "Model P-values"`). Use
#' `print(x$table_body)` to see columns available.
#' @param text_interpret indicates whether text will be interpreted as markdown (`"md"`)
#' or HTML (`"html"`).  The text is interpreted with the {gt} package's `md()` or
#' `html()` functions.  The default is `"md"`, and is ignored when the print engine
#' is not {gt}.
#' @family tbl_summary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_survival tools
#' @author Daniel D. Sjoberg
#' @examples
#' tbl_col_ex1 <-
#'   trial[c("age", "grade", "response")] %>%
#'   tbl_summary() %>%
#'   modify_header(stat_0 = "**All Patients**, N = {N}")
#'
#' tbl_col_ex2 <-
#'   trial[c("age", "grade", "response", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   modify_header(
#'     stat_by = "**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})"
#'   )
#' @return Function return the same class of gtsummary object supplied
#' @export
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_col_ex1.png}{options: width=31\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_col_ex2.png}{options: width=50\%}}

modify_header <- function(x, stat_by = NULL, ..., text_interpret = c("md", "html")) {

  # converting the passed ... to a list, OR if nothing passed to NULL
  if (length(list(...)) == 0) {
    passed_dots <- NULL
  } else {
    passed_dots <- list(...)
  }

  do.call(
    modify_header_internal,
    c(list(
      x = x, stat_by = stat_by, text_interpret = text_interpret,
      .save_call = TRUE
    ), passed_dots)
  )
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

  # updating gt function calls -------------------------------------------------
  x$gt_calls[["cols_label"]] <-
    table_header_to_gt_cols_label(x$table_header)

  # keeping track of all functions previously run ------------------------------
  if (.save_call == TRUE) {
    x$call_list <- c(x$call_list, list(cols_label_summary = match.call()))
  }

  x
}
