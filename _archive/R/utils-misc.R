# converts a character vector into a quotes list separated by a comma, eg 'a', 'b'
quoted_list <- function(x) {
  paste(shQuote(x, type = "csh"), collapse = ", ")
}

# used in the as_flex_table (and friends) functions for inserting calls
add_expr_after <- function(calls, add_after, expr, new_name = NULL) {
  # checking input
  if (!rlang::is_string(add_after) || !add_after %in% names(calls)) {
    stop(glue("`add_after=` must be one of {quoted_list(names(calls))}"))
  }

  # position to insert, and name of list
  index <- which(names(calls) == add_after)
  new_name <- new_name %||% "user_added"
  new_list <- list(expr) %>% set_names(new_name)

  # insert list
  append(calls, new_list, after = index)
}


# this function is used to fill in missing values in the
# x$table_styling$header$modify_stat_* columns
.fill_table_header_modify_stats <-
  function(x, modify_stats = c(
             "modify_stat_N", "modify_stat_N_event",
             "modify_stat_N_unweighted"
           )) {
    modify_stats <-
      x$table_styling$header %>%
      select(any_of(modify_stats) & where(.single_value)) %>%
      names()

    x$table_styling$header <-
      x$table_styling$header %>%
      tidyr::fill(any_of(modify_stats), .direction = "downup")

    return(x)
  }

.single_value <- function(x) {
  if (length(unique(stats::na.omit(x))) == 1L) {
    return(TRUE)
  }
  FALSE
}

#' gtsummary wrapper for purrr::as_mapper
#'
#' This wrapper only accepts a function or formula notation function,
#' and returns an informative message when incorrect inputs passed
#'
#' @param x function or anon. function using formula notation.
#' @param context string indicating function and arg, e.g. `context = "foo(arg=)"`
#' @noRd
#' @keywords internal

gts_mapper <- function(x, context) {
  # checking input, and giving informative error msg
  if (!rlang::is_function(x) && !rlang::is_formula(x)) {
    paste(
      "Expecting a function in argument `{context}`,\n",
      "e.g. `fun = function(x) style_pvalue(x, digits = 2)`, or\n",
      "`fun = ~style_pvalue(., digits = 2)`"
    ) %>%
      stringr::str_glue() %>%
      rlang::abort()
  }

  purrr::as_mapper(x)
}

# All documentation of the global options was removed in v1.3.1.
# This messaging was added in v1.6.0
.get_deprecated_option <- function(x, default = NULL) {
  if (!is.null(getOption(x, default = NULL))) {
    paste(
      "Global option {.val {x}} is soft deprecated and will",
      "{.emph soon} be removed from {.pkg gtsummary}.\nThe functionality",
      "has been migrated to a function argument or a gtsummary theme.",
      "\n{.url https://www.danieldsjoberg.com/gtsummary/articles/themes.html}"
    ) %>%
      cli::cli_alert_danger()
  }
  getOption(x, default = default)
}

.assert_class <- function(x, class, arg_name = "x") {
  if (!inherits(x, class)) {
    cls_clps <-
      glue::glue_collapse(shQuote(class, type = "csh"), sep = ", ", last = ", or ")
    glue("Error in argument '{arg_name}='. Expecting object of class {cls_clps}") %>%
      stop(call. = FALSE)
  }
}

vec_paste0 <- function(..., collapse = NULL) {
  args <- vctrs::vec_recycle_common(...)
  rlang::inject(paste0(!!!args, collapse = collapse))
}

type_check <-
  list(
    is_string =
      list(
        msg = "Expecting a string as the passed value.",
        fn = function(x) is_string(x)
      ),
    is_string_summary_type =
      list(
        msg = "Expecting one of `c('categorical', 'dichotomous', 'continuous', 'continuous2')` as the passed value.",
        fn = function(x) is_string(x) && x %in% c("categorical", "dichotomous", "continuous", "continuous2")
      ),
    is_string_summary_sort =
      list(
        msg = "Expecting one of `c('frequency', 'alphanumeric')` as the passed value.",
        fn = function(x) is_string(x) && x %in% c("frequency", "alphanumeric")
      ),
    is_character =
      list(
        msg = "Expecting a character as the passed value.",
        fn = function(x) is.character(x)
      ),
    is_function =
      list(
        msg = "Expecting a function as the passed value.",
        fn = function(x) is.function(x)
      ),
    is_function_or_string =
      list(
        msg = "Expecting a function or a string of a function name.",
        fn = function(x) is_string(x) || is.function(x)
      ),
    is_string_or_na =
      list(
        msg = "Expecting a string or NA as the passed value.",
        fn = function(x) is_string(x) || is.na(x)
      ),
    is_named =
      list(
        msg = "Expecting a named vector or list as the passed value.",
        fn = function(x) is_named(x)
      ),
    digits =
      list(
        msg = "Expecting an integer, function, or a vector/list of intergers/functions as the passed value.",
        fn = function(x) rlang::is_integerish(x) || is.function(x) || purrr::every(x, ~ rlang::is_integerish(.x) || is.function(.x))
      )
  )
