#' Remove rows
#'
#' Removes either the header, reference, or missing rows from a gtsummary table.
#'
#' @param x (`gtsummary`)\cr
#'   A gtsummary object
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to to remove rows from. Default is `everything()`
#' @param type (`string`)\cr
#'   Type of row to remove. Must be one of `c("header", "reference", "missing", "level", "all")`
#' @param level_value (`string`)
#'   When `type='level'` you can specify the *character* value of the level to remove.
#'   When `NULL` all levels are removed.
#'
#' @export
#' @return Modified gtsummary table
#'
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   dplyr::mutate(
#'     age60 = ifelse(age < 60, "<60", "60+")
#'   ) |>
#'   tbl_summary(by = trt, missing = "no", include = c(trt, age, age60)) |>
#'   remove_row_type(age60, type = "header")
remove_row_type <- function(x, variables = everything(),
                            type = c("header", "reference", "missing", "level", "all"),
                            level_value = NULL) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(remove_row_type = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(level_value, allow_empty = TRUE)
  type <- arg_match(type)

  if (!is.null(level_value) && type != "level") {
    cli::cli_inform("Argument {.code level_value} ignored when {.code type != 'level'}")
  }

  # convert variables input to character variable names ------------------------
  cards::process_selectors(
    scope_table_body(x$table_body),
    variables = {{ variables }}
  )


  # expression for selecting the appropriate rows ------------------------------
  if (type == "reference") {
    lst_expr <- list(
      variables = "reference_row",
      expr = expr(.data$reference_row %in% TRUE)
    )
  } else if (type == "header" && "header_row" %in% names(x$table_body)) {
    lst_expr <- list(
      variables = "header_row",
      expr = expr(.data$header_row %in% TRUE)
    )
  } else if (type == "header") {
    lst_expr <- list(
      variables = c("var_type", "row_type"),
      expr = expr(.data$var_type %in% c("categorical", "continuous2") & .data$row_type == "label")
    )

  } else if (type == "missing") {
    lst_expr <- list(
      variables = "row_type",
      expr = expr(.data$row_type == "missing")
    )
  } else if (type == "level") {
    if (!is.null(level_value)) {
      lst_expr <- list(
        variables = c("row_type", "label"),
        expr = expr(.data$row_type == "level" & .data$label %in% level_value)
      )
    } else {
      lst_expr <- list(
        variables = "row_type",
        expr = expr(.data$row_type == "level")
      )
    }
  } else if (type == "all") {
    lst_expr <- list(
      variables = "variable",
      expr = expr(!is.na(.data$variable))
    )
  }

  if (!all(lst_expr[["variables"]] %in% names(x$table_body))) {
    cli::cli_abort(
      "Column(s) {.val {lst_expr[['variables']] |> setdiff(names(x$table_body)) |> unique()}}
       are not present in {.code x$table_body}, and function cannot be used to remove these rows.",
      call = get_cli_abort_call()
    )
  }

  # removing selected rows -----------------------------------------------------
  # combined expression
  final_expr <- expr(!(.data$variable %in% .env$variables & !!lst_expr[["expr"]]))
  # removing rows, and returning updated gtsummary object
  x <- modify_table_body(x, dplyr::filter, !!final_expr)

  # return gtsummary object ----------------------------------------------------
  x$call_list <- updated_call_list
  x
}
