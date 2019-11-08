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

  # check class of input -------------------------------------------------------
  # each element must be a formula or a named element of a list
  is_formula <- map_lgl(x, ~ class(.x) == "formula")
  if (is.null(names(x))) has_name <- rep(FALSE, length(x))
  else has_name <- as.logical(names(x) != "")

  if (!all(is_formula | has_name)) {
    example_text <-
      switch(
        input_type %||% "mixed",
        "type" = paste("type = list(vars(age) ~ \"continuous\", all_integer() ~ \"categorical\")", collapse = "\n"),
        "label" = paste("label = list(vars(age) ~ \"Age, years\", vars(response) ~ \"Tumor Response\")", collapse = "\n"),
        "statistic" = paste("statistic = list(all_continuous() ~ \"{mean} ({sd})\", all_categorical() ~ \"{n} / {N} ({p}%)\")",
                            "statistic = list(vars(age) ~ \"{median}\")", collapse = "\n"),
        "digits" = paste("digits = list(vars(age) ~ 2)",
                         "digits = list(all_continuous() ~ 2)", collapse = "\n"),
        "value" = paste("value = list(vars(grade) ~ \"III\")",
                        "value = list(all_logical() ~ FALSE)", collapse = "\n"),
        "test" = paste("test = list(all_continuous() ~ \"t.test\")",
                       "test = list(vars(age) ~ \"kruskal.test\")", collapse = "\n"),
        "mixed" = paste("label = list(vars(age) ~ \"Age, years\")",
                        "statistic = list(all_continuous() ~ \"{mean} ({sd})\")", collapse = "\n")
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
    imap(
      x, ~tidyselect_to_list_one(.data = .data, x = .x, x_name = .y,
                                 .meta_data = .meta_data, input_type = input_type)
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

tidyselect_to_list_one <- function(.data, x, x_name, .meta_data = NULL, input_type = NULL) {
  # if named list item, return unmodified --------------------------------------
  if (rlang::is_string(x_name) && x_name != "") {
    x <- list(x)
    names(x) <- x_name
    return(x)
  }

  # registering names of columns in data ---------------------------------------
  tidyselect::scoped_vars(vars = names(.data))
  scoped_data(.data)
  if (!is.null(.meta_data)) scoped_meta_data(.meta_data)

  # for each formula extract lhs and rhs ---------------------------------------
  lhs <- rlang::f_lhs(x) %>% eval()
  rhs <- rlang::f_rhs(x) %>% eval()

  # if tidyselect function returned numeric position, grab character name
  if (is.numeric(lhs)) lhs <- names(.data)[lhs]

  # if varlist supplied in vars() converting to strings
  if (class(lhs) == "quosures") lhs <- map_chr(lhs, rlang::as_label)

  # converting rhs and lhs into a named list
  result <-
    map(lhs, ~ list(rhs) %>% rlang::set_names(.x)) %>%
    flatten()

  result
}
