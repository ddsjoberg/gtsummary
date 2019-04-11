#' Add number of events to a {gtsummary} regression table
#'
#' This function adds a columns of the number of events to tables creates with
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
#' @seealso \code{\link{tbl_regression}}, \code{\link{tbl_uvregression}}

add_nevent <- function(x, ..) UseMethod("add_nevent")

#' Add number of events to a {gtsummary} regression table
#'
#' This function adds a columns of the number of events to tables creates with
#' \code{\link{tbl_regression}}.  Supported
#' model types include GLMs with binomial distribution family (e.g.
#' \code{\link[stats]{glm}}, \code{\link[lme4]{glmer}}, and
#' \code{\link[geepack]{geeglm}}) and Cox
#' Proportion Hazards regression models (\code{\link[survival]{coxph}}).
#'
#' @param x `tbl_regerssion` object
#' @param ... not used
#' @export
#' @author Daniel D. Sjoberg
#' @family tbl_regression
#' @export
#' @examples
#' glm(response ~ trt, trial, family = binomial) %>%
#'   tbl_regression() %>%
#'   add_nevent() %>%
#'   purrr::pluck("table_body")

add_nevent.tbl_regression <- function(x, ..) {
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
#' This function adds a columns of the number of events to tables creates with
#' \code{\link{tbl_uvregression}}.  Supported
#' model types include GLMs with binomial distribution family (e.g.
#' \code{\link[stats]{glm}}, \code{\link[lme4]{glmer}}, and
#' \code{\link[geepack]{geeglm}}) and Cox
#' Proportion Hazards regression models (\code{\link[survival]{coxph}}).
#'
#' @param x `tbl_uvregerssion` object
#' @param ... not used
#' @export
#' @author Daniel D. Sjoberg
#' @family tbl_uvregression
#' @export

add_nevent.tbl_uvregerssion <- function(x, ..) {
  error("Function needs to be written")
  x
}
