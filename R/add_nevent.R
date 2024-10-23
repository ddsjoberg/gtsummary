#' Add event N
#'
#' @inheritParams add_n_regression
#' @name add_nevent_regression
#'
#' @examplesIf gtsummary:::is_pkg_installed("broom.helpers")
#' # Example 1 ----------------------------------
#' trial |>
#'   select(response, trt, grade) |>
#'   tbl_uvregression(
#'     y = response,
#'     exponentiate = TRUE,
#'     method = glm,
#'     method.args = list(family = binomial),
#'   ) |>
#'   add_nevent()
#'
#' # Example 2 ----------------------------------
#' glm(response ~ age + grade, trial, family = binomial) |>
#'   tbl_regression(exponentiate = TRUE) |>
#'   add_nevent(location = "level")
NULL

#' @rdname add_nevent_regression
#' @export
add_nevent <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_nevent")
}

#' @rdname add_nevent_regression
#' @export
add_nevent.tbl_regression <- function(x, location = "label", ...) {
  set_cli_abort_call()
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_nevent = match.call()))

  # process inputs -------------------------------------------------------------
  location <- arg_match(location, values = c("label", "level"), multiple = TRUE)

  if ("level" %in% location && !"n_event" %in% x$table_styling$header$column) {
    cli::cli_abort(
      "Reporting event N on level rows is not available for this model type.",
      call = get_cli_abort_call()
    )
  }
  if ("label" %in% location && !"N_event" %in% x$table_styling$header$column) {
    cli::cli_abort(
      "Reporting event N on label rows is not available for this model type.",
      call = get_cli_abort_call()
    )
  }

  x$table_body$stat_nevent <- NA_integer_
  if ("N_event" %in% names(x$table_body)) {
    x$table_body$stat_nevent <- ifelse(x$table_body$row_type == "label",
                                       x$table_body$N_event %>% as.integer(),
                                       x$table_body$stat_nevent
    )
  }
  if ("n_event" %in% names(x$table_body)) {
    x$table_body$stat_nevent <- ifelse(x$table_body$row_type == "level",
                                       x$table_body$n_event %>% as.integer(),
                                       x$table_body$stat_nevent
    )
  }

  x <-
    modify_table_body(
      x,
      dplyr::mutate,
      stat_nevent =
        dplyr::case_when(
          !"level" %in% .env$location & .data$row_type %in% "level" ~ NA_integer_,
          !"label" %in% .env$location & .data$row_type %in% "label" &
            .data$var_type %in% c("categorical", "dichotomous") ~ NA_integer_,
          TRUE ~ .data$stat_nevent
        )
    ) |>
    modify_table_body(dplyr::relocate, "stat_nevent", .before = "estimate") |>
    modify_table_styling(
      columns = all_of("stat_nevent"),
      label = "**Event N**",
      hide = FALSE,
      fmt_fun = label_style_number()
    )

  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  # add call list and return x
  x$call_list <- updated_call_list
  x
}

#' @rdname add_nevent_regression
#' @export
add_nevent.tbl_uvregression <- add_nevent.tbl_regression

# this function is used to fill in missing values in the
# x$table_styling$header$modify_stat_* columns
.fill_table_header_modify_stats <- function(x,
                                            modify_stats =
                                              c("modify_stat_N", "modify_stat_N_event",
                                                "modify_stat_N_unweighted")) {
  modify_stats <-
    x$table_styling$header |>
    select(any_of(modify_stats) & where(\(x) dplyr::n_distinct(x, na.rm = TRUE) == 1L)) %>%
    names()

  x$table_styling$header <-
    x$table_styling$header %>%
    tidyr::fill(any_of(modify_stats), .direction = "downup")

  return(x)
}
