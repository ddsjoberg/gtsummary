#' Convert formula selector to a named list
#'
#' Functions takes a list of formulas, e.g. `list(starts_with("age") ~ "continuous")`,
#' and returns a named list, e.g. `list(age = "continuous")`.
#'
#' @param x list of selecting formulas
#' @inheritParams .select_to_varnames
#' @export
.formula_list_to_named_list <- function(x, data = NULL, var_info = NULL,
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
  if (!purrr::every(x, ~inherits(.x, "formula"))) {
    .formula_select_error(arg_name = arg_name)
  }

  # converting all inputs to named list ----------------------------------------
  named_list <-
    purrr::map(
      x,
      function(x) {
        # for each formula extract lhs and rhs ---------------------------------
        # checking the LHS is not empty
        f_lhs_quo <- .f_side_as_quo(x, "lhs")
        if (rlang::quo_is_null(f_lhs_quo)) .formula_select_error(arg_name = arg_name)
        # extract LHS of formula
        lhs <- .select_to_varnames(select = !!f_lhs_quo,
                                   data = data,
                                   var_info = var_info,
                                   arg_name = arg_name,
                                   select_single = select_single)

        # evaluate RHS of formula in the original formula environment
        rhs <- .f_side_as_quo(x, "rhs") %>% rlang::eval_tidy()

        # converting rhs and lhs into a named list
        purrr::map(lhs, ~ list(rhs) %>% rlang::set_names(.x)) %>%
          purrr::flatten()
      }
    ) %>%
    purrr::flatten()

  # removing duplicates (using the last one listed if variable occurs more than once)
  tokeep <- names(named_list) %>% rev() %>% {!duplicated(.)} %>% rev()
  named_list[tokeep]
}

#' Variable selector
#'
#' Function takes `select()`-like inputs and converts the selector to
#' a character vector of variable names. Functions accepts tidyselect syntax,
#' and additional selector functions defined within the package
#'
#' @param select A single object selecting variables, e.g. `c(age, stage)`,
#' `starts_with("age")`
#' @param data A data frame to select columns from. Default is NULL
#' @param var_info A data frame of variable names and attributes. May also pass
#' a character vector of variable names. Default is NULL
#' @param arg_name Optional string indicating the source argument name. This
#' helps in the error messaging. Default is NULL.
#' @param select_single Logical indicating whether the result must be a single
#' variable. Default is `FALSE`
#'
#' @return A character vector of variable names
#' @export
.select_to_varnames <- function(select, data = NULL, var_info = NULL,
                                arg_name = NULL, select_single = FALSE) {
  if (is.null(data) && is.null(var_info))
    stop("At least one of `data=` and `var_info=` must be specified.")

  select <- rlang::enquo(select)

  # if NULL passed, return NULL
  if (rlang::quo_is_null(select)) return(NULL)

  # convert var_info to data frame if data not provided ------------------------
  if (is.null(data)) data <- .var_info_to_df(var_info)

  if (!is.null(var_info)) {
    # scoping the variable types
    .scope_var_info(var_info)
    # un-scoping on exit
    on.exit(rm(list = ls(envir = env_variable_type), envir = env_variable_type))
  }

  # determine if selecting input begins with `var()`
  select_input_starts_var <-
    !rlang::quo_is_symbol(select) && # if not a symbol (ie name)
    tryCatch(identical(
      eval(as.list(rlang::quo_get_expr(select)) %>% purrr::pluck(1)),
      dplyr::vars),
      error = function(e) FALSE)

  # performing selecting
  res <-
    tryCatch({
      if (select_input_starts_var) {
        # `vars()` evaluates to a list of quosures; unquoting them in `select()`
        names(dplyr::select(data, !!!rlang::eval_tidy(select)))
      }
      else {
        names(dplyr::select(data, !!select))
      }
    },
    error = function(e) {
      if (!is.null(arg_name))
        error_msg <- stringr::str_glue("Error in `{arg_name}=` argument input. Select from ",
                                       "{paste(sQuote(names(data)), collapse = ', ')}")
      else error_msg <- as.character(e)
      stop(error_msg, call. = FALSE)
    })

  # assuring only a single column is selected
  if (select_single == TRUE && length(res) > 1) {
    stop(stringr::str_glue(
      "Error in `{arg_name}=` argument input--select only a single column. ",
      "The following columns were selected, ",
      "{paste(sQuote(res), collapse = ', ')}"
    ), call. = FALSE)
  }

  # if nothing is selected, return a NULL
  if (length(res) == 0) return(NULL)

  res
}


#' Generate a custom selector function
#'
#' @param variable_column string indicating column variable names are stored
#' @param select_column character vector of columns used in the `select_expr=` argument
#' @param select_expr unquoted predicate command to subset a data frame to select variables
#' @param fun_name quoted name of function where `.generic_selector()` is being used.
#' This helps with error messaging.
#'
#' @return custom selector functions
#' @export
.generic_selector <- function(variable_column, select_column, select_expr, fun_name) {
  # ensuring the proper data has been scoped to use this function
  if (!exists("df_var_info", envir = env_variable_type) ||
      (exists("df_var_info", envir = env_variable_type) &&
       !all(c(variable_column, select_column) %in% names(env_variable_type$df_var_info)))) {
    usethis::ui_oops("Cannot use selector '{fun_name}()' in this context.")
    stop("Invalid syntax", call. = FALSE)
  }

  # selecting the variable from the variable information data frame
  env_variable_type$df_var_info %>%
    dplyr::select(all_of(c(variable_column, select_column))) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::filter({{ select_expr }}) %>%
    dplyr::pull(all_of(variable_column)) %>%
    unique()
}


# scoping the variable characteristics
.scope_var_info <- function(x) {
  # removing everything from selecting environment
  rm(list = ls(envir = env_variable_type), envir = env_variable_type)
  if (!inherits(x, "data.frame")) return(invisible(NULL))

  # saving var_info to selecting environment, where it may be utilized by selecting fns
  env_variable_type$df_var_info <- x

  return(invisible(NULL))
}

# function that converts a meta_data tibble to a tibble of variable names (to be used in selecting)
.var_info_to_df <- function(x) {
  # converting variable name and class into data frame so users can use `where(predicate)`-types
  if (inherits(x, "data.frame") && all(c("variable", "var_class") %in% names(x))) {
    # keep unique var names
    x <-
      dplyr::select(x, all_of(c("variable", "var_class"))) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(.data$variable))
    df <-
      purrr::map2_dfc(
        x$variable, x$var_class,
        function(var, class) {
          switch(
            class,
            "numeric" = data.frame(pi),
            "character" = data.frame(letters[1]),
            "factor" = data.frame(datasets::iris$Species[1]),
            "ordered" = data.frame(factor(datasets::iris$Species[1], ordered = TRUE)),
            "integer" = data.frame(1L),
            "Date" = data.frame(Sys.Date()),
            "POSIXlt" = data.frame(as.POSIXlt(Sys.Date())),
            "POSIXct" = data.frame(as.POSIXct(Sys.Date())),
            "difftime" = data.frame(Sys.Date() - Sys.Date())
          ) %||%
            data.frame(NA) %>%
            purrr::set_names(var)
        }
      )
  }
  # if a data.frame
  else if (inherits(x, "data.frame") && "variable" %in% names(x)) {
    df <- purrr::map_dfc(unique(x$variable), ~data.frame(NA) %>% purrr::set_names(.x))
  }
  # if only a vector of names were passed, converting them to a data frame
  else if (rlang::is_vector(x) && !is.list(x)) {
    df <- purrr::map_dfc(unique(x), ~data.frame(NA) %>% purrr::set_names(.x))
  }
  # return data frame with variables as column names
  df
}

# extract LHS/RHS of formula as quosure. attached env will be the formula env
.f_side_as_quo <- function(x, side = c("lhs", "rhs")) {
  side <- match.arg(side)
  f_expr <-
    switch(side,
           "lhs" = rlang::f_lhs(x),
           "rhs" = rlang::f_rhs(x))
  f_quo <- rlang::quo(!!f_expr)
  attr(f_quo, ".Environment") <- rlang::f_env(x)
  f_quo
}

# there are a couple of places the users input may result in an error.
# this function prints an informative error msg with correct syntax example
.formula_select_error <- function(arg_name) {
  example_text <- formula_select_examples[[arg_name %||% "not_an_arg"]] %||%
    paste(c("label = list(age ~ \"Age, years\")",
            "statistic = list(all_continuous() ~ \"{mean} ({sd})\")",
            "type = list(c(response, death) ~ \"categorical\")"))

  # printing error for argument input
  if (!is.null(arg_name))
    usethis::ui_oops(stringr::str_glue(
      "There was a problem with the",
      "{usethis::ui_code(stringr::str_glue('{arg_name}='))} argument input."))
  else
    usethis::ui_oops("There was a problem with one of the function argument inputs.")
  usethis::ui_info("Below is an example of correct syntax.")
  purrr::walk(example_text, ~print(usethis::ui_code(.)))
  stop("Invalid argument syntax", call. = FALSE)
}

formula_select_examples <- list(
  labels = "labels = list(age ~ \"Age, years\", response ~ \"Tumor Response\")",
  label = "label = list(age ~ \"Age, years\", response ~ \"Tumor Response\")",
  type = "type = list(age ~ \"continuous\", where(is.integer) ~ \"categorical\")",
  statistic = c("statistic = list(all_continuous() ~ \"{mean} ({sd})\", all_categorical() ~ \"{n} / {N} ({p}%)\")",
                "statistic = list(age ~ \"{median}\")"),
  digits = c("digits = list(age ~ 2)", "digits = list(all_continuous() ~ 2)"),
  value = c("value = list(grade ~ \"III\")", "value = list(all_logical() ~ FALSE)"),
  test = c("test = list(all_continuous() ~ \"t.test\")", "test = list(age ~ \"kruskal.test\")")
)

# set new environment for new tidyselect funs
env_variable_type <- rlang::new_environment()

