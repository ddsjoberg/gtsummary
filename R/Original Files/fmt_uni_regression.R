#' Creates table of univariate regression results
#'
#' The `fmt_uni_regression` function arguments are similar to the \code{\link{fmt_regression}}
#' arguments. Review the `fmt_uni_regression`
#' \href{http://www.danieldsjoberg.com/clintable/articles/fmt_regression.html#fmt_uni_regression}{vignette}
#' for detailed examples.
#'
#' @param data Data frame to be used in univariate regression modeling.  Data frame
#' includes the outcome variable(s) and the independent variables.
#' @param method Regression method (e.g. \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#' \code{\link[survival]{coxph}}, and more).
#' @param y model outcome as a string (e.g. `y = 'recurrence'` or `y = 'Surv(time, recur)'`)
#' @param formula String that becomes the model formula.  Uses \code{\link[glue]{glue}} syntax.
#' Default is `"{y} ~ {.x}"`, where `{y}` is the dependent variable, and `{.x}`
#' represents a single covariate. For a random intercept, the formula may be
#' `formula = "{y} ~ {.x} + (1 | gear)"`.
#' @param method.args List of additional arguments passed on to the regression function defined by method.
#' @param exponentiate logical argument passed directly to `broom::tidy()`.
#' Default is `FALSE`
#' @param label list of labels to write in the output. `list(age60 = "Age > 60")`
#' @param show_yesno Vector of names of categorical and factor variables that
#' are `c("No", "Yes")`, `c("no", "yes")`, or `c("NO", "YES")` default to dichotomous printing
#' (i.e. only Yes shown). To force both levels to be shown include the column
#' name in `show_yesno`, e.g. `show_yesno = c("highgrade", "female")`
#' @param conf.level confidence level passed directly to \code{broom::tidy}.
#' Default is 0.95.
#' @param beta_fun function to round and format beta coefficients.  Default is \code{\link{fmt_beta}}
#' @param pvalue_fun function to round and format p-values.  Default is \code{\link{fmt_pvalue}}
#' @export
#' @examples
#' fmt_uni_regression(
#'   trial,
#'   method = "glm",
#'   y = "response",
#'   method.args = list(family = binomial),
#'   exponentiate = TRUE
#' )
#'
#' # rounding pvalues to 2 decimal places, and adding global p-values
#' fmt_uni_regression(
#'   trial,
#'   method = "glm",
#'   y = "response",
#'   method.args = list(family = binomial),
#'   exponentiate = TRUE,
#'   pvalue_fun = function(x) fmt_pvalue(x, digits = 2)
#' ) %>%
#'   add_global()
fmt_uni_regression <- function(data, method, y, method.args = NULL,
                               formula = "{y} ~ {.x}",
                               exponentiate = FALSE, label = NULL,
                               show_yesno = NULL, conf.level = 0.95,
                               beta_fun = fmt_beta, pvalue_fun = fmt_pvalue) {

  # data -----------------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data)) {
    stop(glue::glue(
      "'data' input must be a data frame."
    ))
  }

  # varnames -------------------------------------------------------------------
  # ".x" cannot be a variable name
  if (".x" %in% names(data)) {
    stop("'.x' is reserved and cannot be a column name in data frame.")
  }

  # will return call, and all object passed to in table1 call
  # the object func_inputs is a list of every object passed to the function
  func_inputs <- as.list(environment())

  # get all x vars
  x_vars <- names(data) %>%
    setdiff( # removing outcome variable(s)
      paste0(y, "~1") %>% stats::as.formula() %>% all.vars()
    ) %>%
    setdiff( # removing potential variables added to model formula (e.g. random intercepts)
      all.vars(stats::as.formula(formula)[[3]])
    )

  # bulding regression models
  models <-
    purrr::map(
      x_vars,
      ~ do.call(
        what = method,
        args = c(
          list(
            formula = glue::glue(formula) %>% stats::as.formula(),
            data = data
          ),
          method.args
        )
      )
    )
  names(models) <- x_vars

  # formatting regression models
  fmt_models <-
    purrr::map2(
      models, x_vars,
      ~ fmt_regression(
        .x,
        exponentiate = exponentiate,
        conf.level = conf.level,
        beta_fun = beta_fun,
        pvalue_fun = pvalue_fun,
        label = label,
        show_yesno = show_yesno
      ) %>%
        modify_header(label = c("Variable"))
    )
  names(fmt_models) <- x_vars

  # extracting model tables and stacking
  model_tbl <-
    purrr::map_dfr(
      fmt_models,
      ~ .x %>% purrr::pluck("model_tbl")
    )

  # creating a meta_data table (this will be used in subsequent functions, eg add_global)
  meta_data <-
    model_tbl %>%
    dplyr::filter_(~ row_type == "label") %>%
    dplyr::select(c("variable", "var_type", "label", "N")) %>%
    dplyr::mutate_(
      N_levels = ~ purrr::map2_int(
        variable, var_type,
        ~ ifelse(..2 == "categorical",
          model_tbl %>% dplyr::filter_(~ variable == ..1 & row_type == "level") %>% nrow(),
          NA_integer_
        )
      )
    )


  # deleting all headers except first
  header_n <- as.numeric(gsub("[[:alpha:]]", "", model_tbl$row_type[1]))
  model_tbl <-
    model_tbl %>%
    dplyr::filter_(~ !startsWith(row_type, "header") | dplyr::row_number() <= header_n) %>%
    dplyr::mutate_(
      N = ~ dplyr::case_when(
        row_type == "label" ~ N %>% as.character(),
        row_type == "header1" ~ "N",
        TRUE ~ NA_character_
      )
    )

  # returning named list of results
  results <- list(
    inputs = func_inputs,
    model_obj = models,
    fmt_models = fmt_models,
    meta_data = meta_data,
    model_tbl = model_tbl,
    call_list = list(fmt_uni_regression = match.call())
  )
  class(results) <- "fmt_uni_regression"
  return(results)
}
