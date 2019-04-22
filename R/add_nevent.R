#' Add number of events to a {gtsummary} regression table
#'
#' This function adds a column of the number of events to tables created with
#' \code{\link{tbl_regression}} or \code{\link{tbl_uvregression}}.  Supported
#' model types include GLMs with binomial distribution family (e.g.
#' \code{\link[stats]{glm}}, \code{\link[lme4]{glmer}}, and
#' \code{\link[geepack]{geeglm}}) and Cox
#' Proportion Hazards regression models (\code{\link[survival]{coxph}}).
#'
#' @param x `tbl_regerssion` or `tbl_uvregression` object
#' @param ... further arguments passed to or from other methods.
#' @export
#' @author Daniel D. Sjoberg
#' @seealso \code{\link{add_nevent.tbl_regression}}, \code{\link{add_nevent.tbl_uvregression}}, \code{\link{tbl_regression}}, \code{\link{tbl_uvregression}}

add_nevent <- function(x, ...) UseMethod("add_nevent")

#' Add number of events to a {gtsummary} regression table
#'
#' This function adds a column of the number of events to tables created with
#' \code{\link{tbl_regression}} or \code{\link{tbl_uvregression}}.  Supported
#' model types include GLMs with binomial distribution family (e.g.
#' \code{\link[stats]{glm}}, \code{\link[lme4]{glmer}}, and
#' \code{\link[geepack]{geeglm}}) and Cox
#' Proportion Hazards regression models (\code{\link[survival]{coxph}}).
#'
#' @section Reporting Event N:
#' The number of events is added to the internal `.$table_body` tibble,
#' and not printed in the default output table (similar to N). The number
#' of events is accessible via the `inline_text()` function for printing in a report.
#'
#' @param x `tbl_regression` object
#' @param ... not used
#' @export
#' @author Daniel D. Sjoberg
#' @family tbl_regression
#' @export
#' @examples
#' tbl_reg_nevent_ex <- glm(response ~ trt, trial, family = binomial) %>%
#'   tbl_regression() %>%
#'   add_nevent()
#' @section Figures:
#' \if{html}{\figure{tbl_reg_nevent_ex.png}{options: width=50\%}}
#'

add_nevent.tbl_regression <- function(x, ...) {
  # if model is a cox model, adding number of events as well
  if(class(x$model_obj)[1] == "coxph"){
    x$nevent = x$model_obj %>%
      survival::coxph.detail() %>%
      pluck("nevent") %>%
      sum()

    x$table_body <-
      x$table_body %>%
      mutate(nevent = x$nevent)
  }
  # generalized linear models, and GEE GLMs
  else if(
    # GLM or GEE
    (class(x$model_obj)[1] %in% c("glm", "geeglm")) |
    # lme4 GLM
    (class(x$model_obj)[1] == "glmerMod" & attr(class(x$model_obj),"package") %||% "NULL" == "lme4")) {
    #checking family (must be binomial)
    if(class(x$model_obj)[1] %in% c("glm", "geeglm")) {
      if(x$model_obj$family$family != "binomial") {
        stop("Model type not supported")
      }
      formula = x$model_obj$formula
    }
    else if(class(x$model_obj)[1] == "glmerMod" & attr(class(x$model_obj),"package") %||% "NULL" == "lme4") {
      if(x$model_obj@resp$family$family != "binomial") {
        stop("Model type not supported")
      }
      formula = x$model_obj@frame %>% attr("formula")
    }

    # grabbing name of outcome variable
    outcome_var <-
      formula %>%
      all.vars() %>%
      pluck(1)

    # calculating N event
    x$nevent = x$model_obj %>%
      stats::model.frame() %>%
      pluck(outcome_var) %>%
      as.numeric() %>%
      {. == max(.)} %>%
      sum()

    # adding N event to output table
    x$table_body <-
      x$table_body %>%
      mutate(nevent = x$nevent)
  }
  else{
    stop("Model type not supported")
    return(x)
  }

  x$gt_calls[["cols_hide_nevent"]] <-
    glue("cols_hide(columns = vars(nevent))")
  x
}

#' Add number of events to a {gtsummary} regression table
#'
#' This function adds a column of the number of events to tables created with
#' \code{\link{tbl_regression}} or \code{\link{tbl_uvregression}}.  Supported
#' model types include GLMs with binomial distribution family (e.g.
#' \code{\link[stats]{glm}}, \code{\link[lme4]{glmer}}, and
#' \code{\link[geepack]{geeglm}}) and Cox
#' Proportion Hazards regression models (\code{\link[survival]{coxph}}).
#'
#' @section Reporting Event N:
#' The number of events is added to the internal `.$table_body` tibble,
#' and printed to the right of the N column. The number of events is also
#' accessible via the `inline_text()` function for printing in a report.
#'
#' @param x `tbl_uvregerssion` object
#' @param ... not used
#' @author Daniel D. Sjoberg
#' @family tbl_uvregression
#' @export
#' @examples
#' tbl_uv_nevent_ex <-
#'   trial %>%
#'   tbl_uvregression(
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial)
#'   ) %>%
#'   add_nevent()
#' @section Figures:
#' \if{html}{\figure{tbl_uv_nevent_ex.png}{options: width=50\%}}
#'
add_nevent.tbl_uvregression <- function(x, ...) {

  # adding nevent to each tbl_regression object
  x$tbl_regression_list <-
    x$tbl_regression_list %>%
    map(add_nevent.tbl_regression)

  # extracting nevent from each individual table and adding
  # it to the overall $table_body
  table_nevent <-
    x$tbl_regression_list %>%
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
    )

  x$gt_calls[["cols_nevent"]] <-
    list("cols_move(columns = vars(nevent), after = vars(N))",
         "cols_label(nevent = md('**Event N**'))") %>%
    glue_collapse(sep = " %>% ")
  x
}
