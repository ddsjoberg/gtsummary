#' Convert tidyselect to variable list
#'
#' Functions takes a list of tidyselect formulas, e.g. `list(starts_with("age") ~ "continuous")`,
#' and returns a named list, e.g. `list(age = "continuous")`.
#'
#' @param .data data with variables to select from
#' @param x list of tidyselect formulas
#' @param .meta_data meta data from tbl_summary. Default is NULL
#' @param input_type indicates type of example to print in deprecation note
#' @noRd
#' @keywords internal

tidyselect_to_list <- function(.data, x, .meta_data = NULL, input_type = NULL) {
  # if NULL provided, return NULL ----------------------------------------------
  if (is.null(x)) {
    return(NULL)
  }

  # converting to list if single element passed --------------------------------
  if (class(x) == "formula") {
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
  is_formula <- map_lgl(x, ~ class(.x) == "formula")

  if (!all(is_formula)) {
    example_text <-
      switch(
        input_type %||% "mixed",
        "type" = paste("type = list(vars(age) ~ \"continuous\", all_integer() ~ \"categorical\")", collapse = "\n"),
        "label" = paste("label = list(vars(age) ~ \"Age, years\", vars(response) ~ \"Tumor Response\")", collapse = "\n"),
        "statistic" = paste("statistic = list(all_continuous() ~ \"{mean} ({sd})\", all_categorical() ~ \"{n} / {N} ({p}%)\")",
          "statistic = list(vars(age) ~ \"{median}\")",
          collapse = "\n"
        ),
        "digits" = paste("digits = list(vars(age) ~ 2)",
          "digits = list(all_continuous() ~ 2)",
          collapse = "\n"
        ),
        "value" = paste("value = list(vars(grade) ~ \"III\")",
          "value = list(all_logical() ~ FALSE)",
          collapse = "\n"
        ),
        "test" = paste("test = list(all_continuous() ~ \"t.test\")",
          "test = list(vars(age) ~ \"kruskal.test\")",
          collapse = "\n"
        ),
        "mixed" = paste("label = list(vars(age) ~ \"Age, years\")",
          "statistic = list(all_continuous() ~ \"{mean} ({sd})\")",
          collapse = "\n"
        )
      )

    stop(glue(
      "There was a problem with one of the function argument inputs. ",
      "Review the documentation and update the argument input.",
      "Below is an example of correct syntax.\n\n",
      "{example_text}"
    ), call. = FALSE)
  }

  # converting all inputs to named list ----------------------------------------
  named_list <-
    map(
      x, ~ tidyselect_to_list_one(
        .data = .data, x = .x,
        .meta_data = .meta_data, input_type = input_type
      )
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

tidyselect_to_list_one <- function(.data, x, .meta_data = NULL, input_type = NULL) {
  # for each formula extract lhs and rhs ---------------------------------------
  lhs <- var_input_to_string(data = .data,
                             var_input = !!rlang::f_lhs(x),
                             meta_data = .meta_data)
  rhs <- rlang::f_rhs(x) %>% eval()

  # converting rhs and lhs into a named list
  result <-
    map(lhs, ~ list(rhs) %>% rlang::set_names(.x)) %>%
    flatten()

  result
}

var_input_to_string <- function(data, var_input, meta_data = NULL) {
  var_input <- rlang::enquo(var_input)
  # if NULL passed, return NULL
  if (rlang::quo_is_null(var_input)) {
    return(NULL)
  }

  if (!rlang::quo_is_symbol(var_input)) {
    # checking if the passed enquo begins with the vars() function
    str_fun_name <- rlang::quo_get_expr(var_input)[[1]] %>% deparse()
    if (str_fun_name == "vars" || endsWith(str_fun_name, "::vars")) {
      var_str <- purrr::map(
        as.list(rlang::quo_get_expr(var_input))[-1],
        ~tidyselect_to_string(data, !!.x, meta_data)
      ) %>%
        unlist() %>%
        unique()

      return(var_str)
    }
  }

  tidyselect_to_string(data, !!var_input, meta_data)
}

# this function handles a single tidyselect function, or bare input
# do not call this function directly. do not pass a vars()
tidyselect_to_string <- function(data, var, meta_data = NULL) {
  var <- rlang::enquo(var)

  # scoping data to use gtsummary select functions
  scoped_data(data)
  if(!is.null(meta_data)) scoped_meta_data(meta_data)

  # selecting with standard tidyselect functions and bare inputs
  dplyr::select(data[0, ], !!var) %>% names()
}

