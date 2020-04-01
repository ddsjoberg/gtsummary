#' Add number of events to a regression table
#'
#' Adds a column of the number of events to tables created with
#' [tbl_regression] or [tbl_uvregression].  Supported
#' model types include GLMs with binomial distribution family (e.g.
#' [stats::glm], [lme4::glmer], and
#' [geepack::geeglm]) and Cox
#' Proportion Hazards regression models ([survival::coxph]).
#'
#' @param x `tbl_regerssion` or `tbl_uvregression` object
#' @param ... Additional arguments passed to or from other methods.
#' @export
#' @author Daniel D. Sjoberg
#' @seealso [add_nevent.tbl_regression], [add_nevent.tbl_uvregression],
#' [tbl_regression], [tbl_uvregression]

add_nevent <- function(x, ...) UseMethod("add_nevent")

#' Add number of events to a regression table
#'
#' @description
#' This function adds a column of the number of events to tables created with
#' [tbl_regression].  Supported
#' model types include GLMs with binomial distribution family (e.g.
#' [stats::glm], [lme4::glmer], and
#' [geepack::geeglm]) and Cox
#' Proportion Hazards regression models ([survival::coxph]).
#'
#' The number of events is added to the internal `.$table_body` tibble,
#' and not printed in the default output table (similar to N). The number
#' of events is accessible via the [inline_text] function for printing in a report.
#'
#' @param x `tbl_regression` object
#' @param ... Not used
#' @export
#' @author Daniel D. Sjoberg
#' @family tbl_regression tools
#' @export
#' @return A `tbl_regression` object
#' @examples
#' tbl_reg_nevent_ex <-
#'   glm(response ~ trt, trial, family = binomial) %>%
#'   tbl_regression() %>%
#'   add_nevent()
#' @section Example Output:
#' \if{html}{\figure{tbl_reg_nevent_ex.png}{options: width=50\%}}

add_nevent.tbl_regression <- function(x, ...) {
  # if model is a cox model, adding number of events as well
  if (inherits(x$model_obj, "coxph") && !inherits(x$model_obj, "clogit")) {
    x$nevent <- x$model_obj %>%
      survival::coxph.detail() %>%
      pluck("nevent") %>%
      sum()

    x$table_body <-
      x$table_body %>%
      mutate(nevent = x$nevent)
  }
  # generalized linear models, and GEE GLMs
  else if (
    # GLM or GEE
    (inherits(x$model_obj, c("glm", "geeglm"))) |
      # lme4 GLM
      (inherits(x$model_obj, c("glmerMod"))  &
        attr(class(x$model_obj), "package") %||% "NULL" == "lme4")) {
    # checking family (must be binomial)
    if (inherits(x$model_obj, c("glm", "geeglm"))) {
      if (x$model_obj$family$family != "binomial") {
        stop("Model type not supported")
      }
      formula <- x$model_obj$formula %>% stats::as.formula()
    }
    else if (inherits(x$model_obj, "glmerMod") &
      attr(class(x$model_obj), "package") %||% "NULL" == "lme4") {
      if (x$model_obj@resp$family$family != "binomial") {
        stop("Model type not supported")
      }
      formula <- x$model_obj@frame %>% attr("formula")
    }

    # grabbing name of outcome variable
    outcome_var <-
      formula %>%
      all.vars() %>%
      pluck(1)

    # calculating N event
    x$nevent <- x$model_obj %>%
      stats::model.frame() %>%
      pluck(outcome_var) %>%
      as.numeric() %>%
      {
        . == max(.)
      } %>%
      sum()

    # adding N event to output table
    x$table_body <-
      x$table_body %>%
      mutate(nevent = x$nevent)
  }
  else {
    stop("Model type not supported")
    return(x)
  }

  # column label
  x$table_header <-
    tibble(column = names(x$table_body)) %>%
    left_join(x$table_header, by = "column") %>%
    table_header_fill_missing()

  x$call_list <- c(x$call_list, list(add_nevent = match.call()))

  x
}

#' Add number of events to a regression table
#'
#' Adds a column of the number of events to tables created with
#' [tbl_uvregression].  Supported
#' model types include GLMs with binomial distribution family (e.g.
#' [stats::glm], [lme4::glmer], and
#' [geepack::geeglm]) and Cox
#' Proportion Hazards regression models ([survival::coxph]).
#'
#' @section Reporting Event N:
#' The number of events is added to the internal `.$table_body` tibble,
#' and printed to the right of the N column. The number of events is also
#' accessible via the [inline_text] function for printing in a report.
#'
#' @param x `tbl_uvregerssion` object
#' @param ... Not used
#' @author Daniel D. Sjoberg
#' @family tbl_uvregression tools
#' @export
#' @return A `tbl_uvregression` object
#' @examples
#' tbl_uv_nevent_ex <-
#'   trial[c("response", "trt", "age", "grade")] %>%
#'   tbl_uvregression(
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial)
#'   ) %>%
#'   add_nevent()
#' @section Example Output:
#' \if{html}{\figure{tbl_uv_nevent_ex.png}{options: width=50\%}}
#'
add_nevent.tbl_uvregression <- function(x, ...) {

  # adding nevent to each tbl_regression object
  x$tbls <- x$tbls %>%
    map(add_nevent.tbl_regression)

  # extracting nevent from each individual table and adding
  # it to the overall $table_body
  table_nevent <-
    x$tbls %>%
    map_dfr(
      ~ pluck(.x, "table_body") %>%
        select(c("variable", "var_type", "row_type", "label", "nevent")) %>%
        filter(.data$row_type == "label")
    )

  # merging nevent with the rest of $table_body
  x$table_body <-
    x$table_body %>%
    left_join(
      table_nevent,
      by = c("variable", "var_type", "row_type", "label")
    ) %>%
    select(
      .data$variable, .data$var_type, .data$row_type,
      .data$label, .data$N, .data$nevent, everything()
    )

  # column label
  x$table_header <-
    tibble(column = names(x$table_body)) %>%
    left_join(x$table_header, by = "column") %>%
    table_header_fill_missing()
  x <- modify_header_internal(x, nevent = "**Event N**")


  x
}
