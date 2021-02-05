#' Add number of events to a regression table
#'
#' Adds a column of the number of events to tables created with
#' [tbl_regression] or [tbl_uvregression].  Supported
#' model types are among GLMs with binomial distribution family (e.g.
#' [stats::glm], [lme4::glmer], and
#' [geepack::geeglm]) and Cox
#' Proportion Hazards regression models ([survival::coxph]).
#'
#' @param x `tbl_regression` or `tbl_uvregression` object
#' @param ... Additional arguments passed to or from other methods.
#' @export
#' @author Daniel D. Sjoberg
#' @seealso [add_nevent.tbl_regression], [add_nevent.tbl_uvregression],
#'  [add_nevent.tbl_survfit]

add_nevent <- function(x, ...) UseMethod("add_nevent")

#' Add event N to regression table
#'
#' @inheritParams add_n_regression
#' @name add_nevent_regression
#'
#' @examples
#' # Example 1 ----------------------------------
#' add_nevent.tbl_regression_ex1 <-
#'   trial %>%
#'   select(response, trt, grade) %>%
#'   tbl_uvregression(
#'     y = response,
#'     method = glm,
#'     method.args = list(family = binomial),
#'   ) %>%
#'   add_nevent()
#
#' # Example 2 ----------------------------------
#' add_nevent.tbl_regression_ex2 <-
#'   glm(response ~ age + grade, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   add_nevent(location = "level")
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_nevent.tbl_regression_ex1.png}{options: width=64\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_nevent.tbl_regression_ex2.png}{options: width=64\%}}
NULL

#' @rdname add_nevent_regression
#' @export
add_nevent.tbl_regression <- function(x, location = NULL, ...) {
  location <- match.arg(location, choices = c("label", "level"), several.ok = TRUE)

  if ("level" %in% location && !"n_event" %in% x$table_styling$header$column)
    abort("Reporting event N on level rows is not available for this model type.")
  if ("label" %in% location && !"N_event" %in% x$table_styling$header$column)
    abort("Reporting event N on label rows is not available for this model type.")

  x %>%
    modify_table_body(
      mutate,
      stat_nevent =
        case_when(
          .data$row_type == "label" ~ .data$N_event %>% as.integer(),
          .data$row_type == "level" ~ .data$n_event %>% as.integer()
        ) %>%
        as.integer(),
      stat_nevent = case_when(
        !"level" %in% .env$location & .data$row_type %in% "level" ~ NA_integer_,
        !"label" %in% .env$location & .data$row_type %in% "label" & .data$var_type == "categorical" ~ NA_integer_,
        TRUE ~ .data$stat_nevent
      )
    ) %>%
    modify_table_body(
      dplyr::relocate,
      .data$stat_nevent,
      .before = .data$estimate
    ) %>%
    modify_header(stat_nevent ~ "**Event N**")
}

#' @export
#' @rdname add_nevent_regression
add_nevent.tbl_uvregression <- add_nevent.tbl_regression

#' Add column with number of observed events
#'
#' \lifecycle{experimental}
#' For each `survfit()` object summarized with `tbl_survfit()` this function
#' will add the total number of events observed in a new column.
#'
#' @param x object of class 'tbl_survfit'
#' @param ... Not used
#' @export
#' @family tbl_survfit tools
#' @examples
#' library(survival)
#' fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
#' fit2 <- survfit(Surv(ttdeath, death) ~ trt, trial)
#'
#' # Example 1 ----------------------------------
#' add_nevent.tbl_survfit_ex1 <-
#'   list(fit1, fit2) %>%
#'   tbl_survfit(times = c(12, 24)) %>%
#'   add_n() %>%
#'   add_nevent()
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_nevent.tbl_survfit_ex1.png}{options: width=64\%}}

add_nevent.tbl_survfit <- function(x, ...) {

  # checking survfit is a standard (not multi-state)
  if (!purrr::every(x$meta_data$survfit, ~identical(class(.x), "survfit"))) {
    paste("Each of the `survfit()` objects must have class 'survfit' only.",
          "Multi-state models are not supported by this function.") %>%
      stringr::str_wrap() %>%
      stop(call. = FALSE)
  }

  # calculating event N --------------------------------------------------------
  x$table_body <-
    purrr::map2_dfr(
      x$meta_data$survfit, x$meta_data$variable,
      ~ tibble(
        nevent = broom::tidy(.x) %>% pull(.data$n.event) %>% sum(),
        variable = .y,
        row_type = "label"
      )
    ) %>%
    {left_join(
      x$table_body, .,
      by = c("variable", "row_type")
    )} %>%
    select(any_of(c("variable", "row_type", "label", "N", "nevent")), everything())

  # adding N to table_styling and assigning header label -----------------------
  x <-
    modify_table_styling(
      x,
      columns = "nevent",
      label = "**Event N**",
      fmt_fun = style_number,
      hide = FALSE
    )

  # adding indicator to output that add_n was run on this data
  x$call_list <- c(x$call_list, list(add_nevent = match.call()))

  x
}
