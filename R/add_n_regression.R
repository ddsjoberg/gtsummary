#' Add N to regression table
#'
#' @param x (`tbl_regression`, `tbl_uvregression`)\cr
#'   a `tbl_regression` or `tbl_uvregression` table
#' @param location (`character`)\cr
#'   location to place Ns. Select one or more of `c('label', 'level')`.
#'   Default is `'label'`.
#'
#'   When `"label"` total Ns are placed
#'   on each variable's label row. When `"level"` level counts are placed on the
#'   variable level for categorical variables, and total N on the variable's label
#'   row for continuous.
#' @inheritParams rlang::args_dots_empty
#'
#' @name add_n_regression
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   select(response, age, grade) |>
#'   tbl_uvregression(
#'     y = response,
#'     exponentiate = TRUE,
#'     method = glm,
#'     method.args = list(family = binomial),
#'     hide_n = TRUE
#'   ) |>
#'   add_n(location = "label")
#'
#' # Example 2 ----------------------------------
#' glm(response ~ age + grade, trial, family = binomial) |>
#'   tbl_regression(exponentiate = TRUE) |>
#'   add_n(location = "level")
NULL

#' @rdname add_n_regression
#' @export
add_n.tbl_regression <- function(x, location = "label", ...) {
  set_cli_abort_call()
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_nevent = match.call()))

  # process inputs -------------------------------------------------------------
  location <- arg_match(location, values = c("label", "level"), multiple = TRUE)

  if ("level" %in% location && !"n_obs" %in% x$table_styling$header$column) {
    cli::cli_abort(
      "Reporting N on level rows is not available for this model type.",
      call = get_cli_abort_call()
    )
  }
  if ("label" %in% location && !"N_obs" %in% x$table_styling$header$column) {
    cli::cli_abort(
      "Reporting N on label rows is not available for this model type.",
      call = get_cli_abort_call()
    )
  }

  x$table_body$stat_n <- NA_integer_
  if ("N_obs" %in% names(x$table_body)) {
    x$table_body$stat_n <- ifelse(x$table_body$row_type == "label",
                                  x$table_body$N_obs %>% as.integer(),
                                  x$table_body$stat_n
    )
  }
  if ("n_obs" %in% names(x$table_body)) {
    x$table_body$stat_n <- ifelse(x$table_body$row_type == "level",
                                  x$table_body$n_obs %>% as.integer(),
                                  x$table_body$stat_n
    )
  }
  x <-
    x |>
    modify_table_body(
      mutate,
      stat_n =
        dplyr::case_when(
          !"level" %in% .env$location & .data$row_type %in% "level" ~ NA_integer_,
          !"label" %in% .env$location & .data$row_type %in% "label" &
            .data$var_type %in% c("categorical", "dichotomous") ~ NA_integer_,
          TRUE ~ .data$stat_n
        )
    ) |>
    modify_table_body(dplyr::relocate, "stat_n", .after = "label") |>
    modify_table_styling(
      columns = all_of("stat_n"),
      label = "**N**",
      hide = FALSE,
      fmt_fun = label_style_number()
    )

  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname add_n_regression
add_n.tbl_uvregression <- add_n.tbl_regression
