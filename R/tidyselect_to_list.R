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
  is_formula <- map_lgl(x, ~ inherits(.x, "formula"))

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
      glue("There was a problem with the `{arg_name}=` argument input. "),
      glue("There was a problem with one of the function argument inputs. ")
    )
    stop(glue(
      "{error_text}",
      "Below is an example of correct syntax.\n\n",
      "{example_text}"
    ), call. = FALSE)
  }

  # converting all inputs to named list ----------------------------------------
  named_list <-
    map(
      x,
      function(x) {
        # for each formula extract lhs and rhs ---------------------------------
        lhs <- var_input_to_string(data = .data, # convert lhs selectors to character
                                   select_input = !!rlang::f_lhs(x),
                                   meta_data = .meta_data, arg_name = arg_name,
                                   select_single = select_single)

        rhs <- rlang::f_rhs(x) %>% eval()

        # converting rhs and lhs into a named list
        map(lhs, ~ list(rhs) %>% rlang::set_names(.x)) %>%
          flatten()
      }
    ) %>%
    flatten()

  # removing duplicates (using the last one listed if variable occurs more than once)
  tokeep <-
    names(named_list) %>%
    rev() %>%
    negate(duplicated)() %>%
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
      error_msg <- glue("Error in `{arg_name}=` argument input. Select from ",
                        "{paste(sQuote(names(...data...)), collapse = ', ')}")
    else error_msg <- as.character(e)
    stop(error_msg, call. = FALSE)
  })

  # assuring only a single column is selected
  if (select_single == TRUE && length(result) != 1) {
    stop(glue(
      "Error in `{arg_name}=` argument input--select only a single column. ",
      "The following columns were selected, ",
      "{paste(sQuote(result), collapse = ', ')}"
    ), call. = FALSE)
  }
  result
}

