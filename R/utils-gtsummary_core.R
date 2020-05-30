# THIS FILE CONTAINS A FEW SCRIPTS THAT ASSIST IN SETTING UP A GENERAL
# GTSUMMARY OBJECT. IF YOU'RE CREATING A GTSUMMARY-LIKE FUNCTION, YOU'LL
# WANT TO GRAB A COPY OF THIS FILE AND PLACE IT IN YOUR PACKAGE.

# LAST UPDATED: 2020-05-09

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

#' Convert tidyselect to variable list
#'
#' Functions takes a list of tidyselect formulas, e.g. `list(starts_with("age") ~ "continuous")`,
#' and returns a named list, e.g. `list(age = "continuous")`.
#'
#' @param .data data with variables to select from
#' @param x list of tidyselect formulas
#' @param .meta_data meta data from tbl_summary. Default is NULL
#' @param arg_name name of argument where selector is called
#' (aids in error messaging). Default is NULL
#' @param select_single Logical indicating if only a single column can be selected
#' @noRd
#' @keywords internal

tidyselect_to_list <- function(.data, x, .meta_data = NULL,
                               arg_name = NULL, select_single = FALSE) {
  # if NULL provided, return NULL ----------------------------------------------
  if (is.null(x)) {
    return(NULL)
  }

  # converting to list if single element passed --------------------------------
  if (inherits(x, "formula")) {
    x <- list(x)
  }

  # returning named list if passed ---------------------------------------------
  if (!is.null(names(x)) && # names are non-null
      length(names(x)) == length(x) && # name of every element of list
      sum(names(x) == "") == 0) { # no names are blank
    return(x)
  }

  # check class of input -------------------------------------------------------
  # each element must be a formula
  is_formula <- purrr::map_lgl(x, ~ inherits(.x, "formula"))

  if (!all(is_formula)) {
    example_text <-
      switch(
        arg_name %||% "not_specified",
        "type" = paste("type = list(age ~ \"continuous\", all_integer() ~ \"categorical\")",
                       collapse = "\n"),
        "label" = paste("label = list(age ~ \"Age, years\", response ~ \"Tumor Response\")",
                        collapse = "\n"),
        "statistic" = paste(c("statistic = list(all_continuous() ~ \"{mean} ({sd})\", all_categorical() ~ \"{n} / {N} ({p}%)\")",
                              "statistic = list(age ~ \"{median}\")"),
                            collapse = "\n"),
        "digits" = paste(c("digits = list(age ~ 2)",
                           "digits = list(all_continuous() ~ 2)"),
                         collapse = "\n"),
        "value" = paste(c("value = list(grade ~ \"III\")",
                          "value = list(all_logical() ~ FALSE)"),
                        collapse = "\n"),
        "test" = paste(c("test = list(all_continuous() ~ \"t.test\")",
                         "test = list(age ~ \"kruskal.test\")"),
                       collapse = "\n")
      ) %||%
      paste(c("label = list(age ~ \"Age, years\")",
              "statistic = list(all_continuous() ~ \"{mean} ({sd})\")",
              "type = list(c(response, death) ~ \"categorical\")"),
            collapse = "\n")

    # printing error for argument input
    error_text <- ifelse(
      !is.null(arg_name),
      glue::glue("There was a problem with the `{arg_name}=` argument input. "),
      glue::glue("There was a problem with one of the function argument inputs. ")
    )
    stop(glue::glue(
      "{error_text}",
      "Below is an example of correct syntax.\n\n",
      "{example_text}"
    ), call. = FALSE)
  }

  # converting all inputs to named list ----------------------------------------
  named_list <-
    purrr::map(
      x,
      function(x) {
        # for each formula extract lhs and rhs ---------------------------------
        lhs <- var_input_to_string(data = .data, # convert lhs selectors to character
                                   select_input = !!rlang::f_lhs(x),
                                   meta_data = .meta_data,
                                   arg_name = arg_name,
                                   select_single = select_single)

        rhs <- rlang::f_rhs(x) %>% rlang::eval_tidy(env = rlang::f_env(x))

        # converting rhs and lhs into a named list
        purrr::map(lhs, ~ list(rhs) %>% rlang::set_names(.x)) %>%
          purrr::flatten()
      }
    ) %>%
    purrr::flatten()

  # removing duplicates (using the last one listed if variable occurs more than once)
  tokeep <-
    names(named_list) %>%
    rev() %>%
    purrr::negate(duplicated)() %>%
    rev()
  named_list[tokeep]
}



#' Convert NSE or SE selectors to character
#'
#' The function accepts a mix of bare (aka symbol input), tidyselect,
#' multiple tidyselectors wrapped in vars(),
#' gtsummary selectors, string, or character vector inputs.  NULL inputs
#' return NULL.
#' @param data a data frame from which columns are selected
#' @param var_input selector statement
#' @param meta_data optional argument for use with `all_categorical()`,
#' `all_dichotomous()`, and `all_continuous()`
#'
#' @return character vector of selected variable names
#' @keywords internal
#' @noRd
#' @examples
#' var_input_to_string(mtcars, select_input = vars(hp, mpg))
#' var_input_to_string(mtcars, select_input = mpg)
#' var_input_to_string(mtcars, select_input = "mpg")
#' var_input_to_string(mtcars, select_input = c("hp", "mpg"))
#' var_input_to_string(mtcars, select_input = c(hp, mpg))
#' var_input_to_string(mtcars, select_input = NULL)
#' var_input_to_string(mtcars, select_input = vars(everything(), -mpg))
#' var_input_to_string(mtcars, select_input = c(everything(), -mpg))
var_input_to_string <- function(data, meta_data = NULL, arg_name = NULL,
                                select_single = FALSE, select_input) {

  select_input <- rlang::enquo(select_input)
  # if NULL passed, return NULL
  if (rlang::quo_is_null(select_input)) {
    return(NULL)
  }

  # converting to list before passing along to next function
  select_input_list <- as.list(rlang::quo_get_expr(select_input))

  # checking if the passed enquo begins with the vars() function
  if (!rlang::quo_is_symbol(select_input) && # if not a symbol (ie name)
      identical(eval(select_input_list[[1]]), dplyr::vars)) # and function is dplyr::vars
  {
    # first item of the list is vars(), removing and passing to tidyselect_to_string()
    return(tidyselect_to_string(...data... = data, ...meta_data... = meta_data,
                                arg_name = arg_name, select_single = select_single,
                                !!!select_input_list[-1]))
  }

  tidyselect_to_string(...data... = data, ...meta_data... = meta_data,
                       arg_name = arg_name, select_single = select_single,
                       !!select_input)
}

# this function handles a single tidyselect function, or bare input
# do not call this function directly. do not pass a vars()
tidyselect_to_string <- function(...data..., ...meta_data... = NULL,
                                 arg_name = NULL, select_single = FALSE, ...) {

  dots_enquo <- rlang::enquos(...)

  # scoping data to use gtsummary select functions
  scoped_data(...data...)
  if(!is.null(...meta_data...)) scoped_meta_data(...meta_data...)

  tryCatch({
    result <-
      rlang::call2(dplyr::select, .data = ...data...[0, , drop = FALSE], !!!dots_enquo) %>%
      rlang::eval_tidy() %>%
      colnames()

    # if `!!!dots_enquo` resolves to a NULL object, the above call will return
    # `character(0)`. If this occurs, return a NULL object
    if (identical(result, character(0))) return(NULL)
  },
  error = function(e) {
    if (!is.null(arg_name))
      error_msg <- glue::glue("Error in `{arg_name}=` argument input. Select from ",
                        "{paste(sQuote(names(...data...)), collapse = ', ')}")
    else error_msg <- as.character(e)
    stop(error_msg, call. = FALSE)
  })

  # assuring only a single column is selected
  if (select_single == TRUE && length(result) != 1) {
    stop(glue::glue(
      "Error in `{arg_name}=` argument input--select only a single column. ",
      "The following columns were selected, ",
      "{paste(sQuote(result), collapse = ', ')}"
    ), call. = FALSE)
  }
  result
}

# select helpers environments --------------------------------------------------
# setting environments
data_env <- rlang::new_environment()
meta_data_env <- rlang::new_environment()

# registering data information
scoped_data <- function(.data) {
  data_env$numeric <- purrr::map_lgl(.data, is.numeric)
  data_env$character <- purrr::map_lgl(.data, is.character)
  data_env$integer <- purrr::map_lgl(.data, is.integer)
  data_env$double <- purrr::map_lgl(.data, is.double)
  data_env$logical <- purrr::map_lgl(.data, is.logical)
  data_env$factor <- purrr::map_lgl(.data, is.factor)
}

# registering meta data information
scoped_meta_data <- function(.meta_data) {
  meta_data_env$summary_type <-
    .meta_data$summary_type %>%
    rlang::set_names(.meta_data$variable)
}
