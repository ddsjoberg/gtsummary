#' Convert tidyselect to variable list
#'
#' Functions takes a list of tidyselect formulas, e.g. `list(starts_with("age") ~ "continuous")`,
#' and returns a named list, e.g. `list(age = "continuous")`.
#'
#' @param .data data with variables to select from
#' @param x list of tidyselect formulas
#' @param .meta_data meta data from tbl_summary. Default is NULL
#' @param input_type indicates type of example to print in deprecation note
#' @keywords internal

tidyselect_to_list <- function(.data, x, .meta_data = NULL, input_type = NULL) {
  # if x is a named list, print depcrecation note and return the input as is
  if (!is.null(names(x))) {
    example_text <-
      switch(
        input_type %||% "mixed",
        "type" = "type = list(\"age\" ~ \"continuous\", all_integer() ~ \"categorical\")",
        "label" = "label = list(\"age\" ~ \"Age, years\", \"response\" ~ \"Tumor Response\")",
        "statistic" = "statistic = list(all_continuous() ~ \"{mean} ({sd})\", all_categorical() ~ \"{n} / {N} ({p}%)\") \nstatistic = list(\"age\" ~ \"{median}\")",
        "digits" = "digits = list(\"age\" ~ 2)\ndigits = list(all_continuous() ~ 2)",
        "value" = "value = list(\"grade\" ~ \"III\") \nvalue = list(all_logical() ~ FALSE)",
        "test" = "test = list(all_continuous() ~ \"t.test\") \ntest = list(\"age\" ~ \"kruskal.test\")",
        "mixed" = "label = list(\"age\" ~ \"Age, years\") \nstatistic = list(all_continuous() ~ \"{mean} ({sd})\")"
      )

    stop_defunct(glue(
      "Passing named lists is deprecated. \n",
      "Update code to pass a list of formulas. \n",
      "The LHS of the formula selects the variables, and \n",
      "the RHS are the instructions.  For example, \n\n",
      "{example_text}"
    ))
    return(x)
  }

  # if a single formula is passed, putting it in a list
  if (class(x) == "formula") x <- list(x)

  # registering names of columns in data
  tidyselect::scoped_vars(vars = names(.data))
  scoped_data(.data)
  if (!is.null(.meta_data)) scoped_meta_data(.meta_data)


  # number of formulas to work through
  n <- length(x)

  # if NULL provided, return NULL
  if (n == 0) {
    return(NULL)
  }

  # initializing empty results
  lhs <- vector("list", n)
  rhs <- vector("list", n)

  # for each formula extract lhs and rhs
  for (i in seq_len(n)) {
    # checking input is a formula
    if (!rlang::is_formula(x[[i]])) stop("Input must be a formula")

    lhs[[i]] <- rlang::f_lhs(x[[i]]) %>% eval()
    rhs[[i]] <- rlang::f_rhs(x[[i]]) %>% eval()
  }



  # if tidyselect function returned numeric position, grab character name
  lhs <- map_if(lhs, is.numeric, ~ names(.data)[.x])
  # TODO: fix this garbage code for dplyr::vars()
  # if tidyselect function returned quosure, convert to character
  # > dplyr::vars(grade) %>% as.character()
  # [1] "~grade"
  lhs <- map_if(
    lhs, ~ class(.x) == "quosures",
    ~ as.character(.x) %>% stringr::str_remove(stringr::fixed("~"))
  )

  # converting rhs and lhs into a named list
  result <-
    map2(lhs, rhs, ~ rep(list(.y), length(.x)) %>% rlang::set_names(.x)) %>%
    flatten()

  # removing duplicates (using the last one listed if variable occurs more than once)
  tokeep <-
    names(result) %>%
    rev() %>%
    negate(duplicated)() %>%
    rev()
  result <- result[tokeep]

  result
}
