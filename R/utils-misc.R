.extract_glue_elements <- function(x) {
  regmatches(x, gregexpr("\\{([^\\}]*)\\}", x)) |>
    unlist() %>%
    {
      substr(., 2, nchar(.) - 1)
    }
}

.ifelse1 <- function(test, yes, no) {
  if (test) {
    return(yes)
  }
  no
}


# used in the as_flex_table (and friends) functions for inserting calls
add_expr_after <- function(calls, add_after, expr, new_name = NULL) {
  # checking input
  if (!rlang::is_string(add_after) || !add_after %in% names(calls)) {
    cli::cli_abort("Argument {.arg add_after} must be one of {.val {names(calls)}}.")
  }

  # position to insert, and name of list
  index <- which(names(calls) == add_after)
  new_name <- new_name %||% "user_added"
  new_list <- list(expr) %>% set_names(new_name)

  # insert list
  append(calls, new_list, after = index)
}
