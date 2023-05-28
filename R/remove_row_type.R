#' Remove rows by type
#'
#' Removes either the header, reference, or missing rows from a gtsummary table.
#'
#' @param x gtsummary object
#' @param variables variables to to remove rows from. Default is `everything()`
#' @param type type of row to remove. Must be one of
#' @param level_value When `type='level'` you can specify the *character* value of the level to remove.
#' When `NULL` all levels are removed.
#' `c("header", "reference", "missing")`
#' @export
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#'
#' @examples
#' # Example 1 ----------------------------------
#' library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
#' remove_row_type_ex1 <-
#'   trial %>%
#'   select(trt, age) %>%
#'   mutate(
#'     age60 = case_when(age < 60 ~ "<60", age >= 60 ~ "60+")
#'   ) %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   remove_row_type(age60, type = "header")
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "remove_row_type_ex1.png", width = "60")`
#' }}
remove_row_type <- function(x, variables = everything(),
                            type = c("header", "reference", "missing", "level", "all"),
                            level_value = NULL) {
  updated_call_list <- c(x$call_list, list(remove_row_type = match.call()))
  # check inputs ---------------------------------------------------------------
  .assert_class(x, "gtsummary")
  type <- match.arg(type)

  if (!is.null(level_value) && type != "level") {
    cli::cli_inform("Argument {.code level_value} ignored when {.code type != 'level'}")
  }
  if (!is.null(level_value) && !is.character(level_value)) {
    cli::cli_abort("The {.code level_value} argument must be class {.cls character}")
  }

  # convert variables input to character variable names ------------------------
  variables <-
    .select_to_varnames(
      {{ variables }},
      data = switch(class(x)[1],
        "tbl_svysummary" = x$inputs$data$variables
      ) %||%
        x$inputs$data,
      var_info = x$table_body,
      arg_name = "variables"
    )

  # expression for selecting the appropriate rows ------------------------------
  if (type == "reference") {
    lst_expr <- list(
      variables = "reference_row",
      expr = expr(.data$reference_row %in% TRUE)
    )
  } else if (type == "header" && inherits(x, c("tbl_summary", "tbl_svysummary", "tbl_survfit"))) {
    lst_expr <- list(
      variables = c("var_type", "row_type"),
      expr = expr(.data$var_type %in% c("categorical", "continuous2") & .data$row_type == "label")
    )
  } else if (type == "header") {
    lst_expr <- list(
      variables = "header_row",
      expr = expr(.data$header_row %in% TRUE)
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
    glue("Cannot select '{type}' rows in this gtsummary table.") %>%
      abort()
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
