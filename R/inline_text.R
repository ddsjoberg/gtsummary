#' Report statistics from gtsummary tables inline
#'
#'
#' @param x object created from a gtsummary function
#' @param ... further arguments passed to or from other methods.
#' @author Daniel D. Sjoberg
#' @seealso [inline_text.tbl_summary], [inline_text.tbl_regression],
#' [inline_text.tbl_uvregression], [inline_text.tbl_survival]
#' @export
inline_text <- function(x, ...) {
  UseMethod("inline_text")
}

#' Report statistics from summary tables inline
#'
#' Extracts and returns statistics from a `tbl_summary` object for
#' inline reporting in an R markdown document. Detailed examples in the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#inline_text}{tbl_summary vignette}
#'
#' @param x object created from  [tbl_summary]
#' @param variable variable name of statistic to present
#' @param level level of the variable to display for categorical variables.
#' Can also specify the 'Unknown' row.  Default is `NULL`
#' @param column column name to return from `x$table_body`.
#' Can also pass the level of a by variable.
#' @inheritParams tbl_regression
#' @param ... not used
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' t1 <- tbl_summary(trial)
#' t2 <- tbl_summary(trial, by = "trt") %>% add_p()
#'
#' inline_text(t1, variable = "age")
#' inline_text(t2, variable = "grade", level = "I", column = "Drug")
#' inline_text(t2, variable = "grade", column = "p.value")
inline_text.tbl_summary <-
  function(x, variable, level = NULL,
             column = ifelse(is.null(x$by), "stat_0", stop("Must specify column")),
             pvalue_fun = function(x) style_pvalue(x, prepend_p = TRUE), ...) {
    # checking column ----------------------------------------------------------
    # the follwing code converts the column input to a column name in x$table_body
    col_lookup_table <- tibble(
      input = names(x$table_body),
      column_name = names(x$table_body)
    )
    # adding levels if there is a by variable
    if (!is.null(x$by)) {
      col_lookup_table <-
        col_lookup_table %>%
        bind_rows(
          x$df_by %>% select(c("by_chr", "by_col")) %>% set_names(c("input", "column_name"))
        )
    }

    column <- col_lookup_table %>%
      filter(!!parse_expr(glue("input == '{column}'"))) %>%
      slice(1) %>%
      pull("column_name")

    if (length(column) == 0) {
      stop(glue(
        "No column selected.  Must be one of: ",
        "{paste(col_lookup_table$input, collapse = ', ')}"
      ))
    }



    # select variable ----------------------------------------------------------
    # grabbing rows matching variable
    result <-
      x$table_body %>%
      filter(!!parse_expr(glue("variable ==  '{variable}'")))

    if (nrow(result) == 0) {
      stop(glue(
        "Is the variable name spelled correctly? variable must be one of: ",
        "{pluck(x, 'meta_data', 'variable') %>% paste(collapse = ', ')}"
      ))
    }

    # select variable level ----------------------------------------------------
    if (is.null(level)) {
      result <- result %>% slice(1)
    }
    else {
      # if the length of this is 0, there are no levels to select.  Should we print an error here?
      levels_obs <- result %>%
        filter(!!parse_expr('row_type != "label"')) %>%
        pull("label")
      result <-
        result %>%
        filter(!!parse_expr(glue("label ==  '{level}'")))
    }

    if (nrow(result) == 0) {
      stop(glue(
        "Is the variable level spelled correctly? level must be one of: ",
        "{levels_obs %>% paste(collapse = ', ')}"
      ))
    }

    # select column ------------------------------------------------------------
    result <- result %>% pull(column)

    # return statistic ---------------------------------------------------------
    if (column %in% c("p.value", "q.value")) {
      return(pvalue_fun(result))
    }

    result
  }


#' Report statistics from regression summary tables inline
#'
#' Takes an object with class `tbl_regression`, and the
#' location of the statistic to report and returns the statistic for reporting
#' inline in an R markdown document.  Detailed examples in the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#inline_text}{tbl_regression vignette}
#'
#' @param x object created from  [tbl_regression]
#' @param variable variable name of statistic to present
#' @param level level of the variable to display for categorical variables.
#' Default is `NULL`, returning the top row in the table for the variable.
#' @param pattern statistics to return.  Uses [glue::glue] formatting.
#' Default is \code{"{estimate} ({conf.level }\% CI  {conf.low}, {conf.high}; {p.value})"}.  All columns from
#' `x$table_body` are available to print as well as the confidence level (conf.level).
#' Uses [glue::glue] formatting. See below for details.
#' @param estimate_fun function to style model coefficient estimates.
#' Columns 'estimate', 'conf.low', and 'conf.high' are formatted.
#' Default is `x$inputs$estimate_fun`
#' @param pvalue_fun function to style p-values and/or q-values.
#' Default is `function(x) style_pvalue(x, prepend_p = TRUE)`
#'
#' @section pattern argument:
#' The following items are available to print.  Use `print(x$table_body)` to
#' print the table the estimates are extracted from.
#' \itemize{
#'   \item `{estimate}` coefficient estimate formatted with 'estimate_fun'
#'   \item `{conf.low}` lower limit of confidence interval formatted with 'estimate_fun'
#'   \item `{conf.high}` upper limit of confidence interval formatted with 'estimate_fun'
#'   \item `{ci}` confidence interval formatted with x$estimate_fun
#'   \item `{p.value}` p-value formatted with 'pvalue_fun'
#'   \item `{N}` number of observations in model
#'   \item `{label}` variable/variable level label
#' }
#' @param ... not used
#' @author Daniel D. Sjoberg
#' @family tbl_regression tools
#' @export
#' @examples
#' inline_text_ex1 <-
#'   glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
#' inline_text(inline_text_ex1, variable = "age")
#' inline_text(inline_text_ex1, variable = "grade", level = "III")
inline_text.tbl_regression <-
  function(x, variable, level = NULL,
             pattern = "{estimate} ({conf.level*100}% CI {conf.low}, {conf.high}; {p.value})",
             estimate_fun = x$inputs$estimate_fun,
             pvalue_fun = function(x) style_pvalue(x, prepend_p = TRUE), ...) {
    # table_body preformatting -------------------------------------------------
    # this is only being performed for tbl_uvregression benefit
    # getting N on every row of the table
    n_vars <- names(x$table_body) %>% intersect(c("N", "nevent"))
    x$table_body <-
      left_join(
        x$table_body %>% select(-n_vars),
        x$table_body %>% filter(!!parse_expr('row_type == "label"')) %>% select(c("variable", n_vars)) %>% distinct(),
        by = "variable"
      )

    # select variable ----------------------------------------------------------
    # grabbing rows matching variable
    filter_expr <-
      result <-
      x$table_body %>%
      filter(!!parse_expr(glue("variable ==  '{variable}'")))

    if (nrow(result) == 0) {
      stop(glue(
        "Is the variable name spelled correctly? variable must be one of: ",
        "{pluck(x, 'meta_data', 'variable') %>% paste(collapse = ', ')}"
      ))
    }

    # select variable level ----------------------------------------------------
    if (is.null(level)) {
      result <- result %>% slice(1)
    }
    else {
      # if the length of this is 0, there are no levels to select.  Should we print an error here?
      levels_obs <- result %>%
        filter(!!parse_expr('row_type != "label"')) %>%
        pull("label")
      result <-
        result %>%
        filter(!!parse_expr(glue("label ==  '{level}'")))
    }

    if (nrow(result) == 0) {
      stop(glue(
        "Is the variable level spelled correctly? level must be one of: ",
        "{levels_obs %>% paste(collapse = ', ')}"
      ))
    }

    # calculating statistic ----------------------------------------------------
    pvalue_cols <- names(result) %>% intersect(c("p.value", "q.value"))
    result <-
      result %>%
      mutate_at(vars(one_of(c("estimate", "conf.low", "conf.high"))), estimate_fun) %>%
      mutate_at(vars(one_of(pvalue_cols)), pvalue_fun) %>%
      mutate(
        conf.level = x$inputs$conf.level,
        stat = glue(pattern)
      ) %>%
      pull("stat")

    result
  }


#' Report statistics from regression summary tables inline
#'
#' Extracts and returns statistics from a table created by the `tbl_uvregression`
#' function for inline reporting in an R markdown document.
#' Detailed examples in the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#inline_text}{tbl_regression vignette}
#'
#' @inherit inline_text.tbl_regression
#' @family tbl_uvregression tools
#' @export
#' @examples
#' inline_text_ex1 <-
#'   trial %>%
#'   dplyr::select(response, age, grade) %>%
#'   tbl_uvregression(
#'     method = glm,
#'     method.args = list(family = binomial),
#'     y = response,
#'     exponentiate = TRUE
#'   )
#'
#' inline_text(inline_text_ex1, variable = "age")
#' inline_text(inline_text_ex1, variable = "grade", level = "III")
inline_text.tbl_uvregression <- inline_text.tbl_regression


#' Report statistics from survival summary tables inline
#'
# 'Extracts and returns statistics from a table created by the `tbl_survival`
#' for inline reporting in an R markdown document.

#'
#' @param x object created from  [tbl_survival]
#' @param strata if `tbl_survival` estimates are stratified, level of the stratum
#' to report. Default is `NULL` when `tbl_survival` have no specified strata.
#' @param time time for which to return survival probability
#' @param prob probability for which to return survival time
#' @param pattern statistics to return.  Uses [glue::glue] formatting.
#' Default is \code{'{estimate} ({conf.level*100}\% {ci})'}.  All columns from
#' `x$table_long` are available to print as well as the confidence level (conf.level).
#' Uses [glue::glue] formatting. See below for details.
#' @param estimate_fun function to round/style estimate and lower/upper confidence
#' interval estimates.  Notably, this does not style the 'ci' column, which is
#' pre-styled in 'x'. Default is x$estimate_fun
#' @param ... not used
#'
#' @section pattern argument:
#' The following items are available to print.  Use `print(x$table_long)` to
#' print the table the estimates are extracted from.
#' \itemize{
#'   \item `{label}` time or prob label
#'   \item `{estimate}` survival or survival time estimate formatted with 'estimate_fun'
#'   \item `{conf.low}` lower limit of confidence interval formatted with 'estimate_fun'
#'   \item `{conf.high}` upper limit of confidence interval formatted with 'estimate_fun'
#'   \item `{ci}` confidence interval formatted with x$estimate_fun (pre-formatted)
#'   \item `{time}/{prob}` time or survival quantile (numeric)
#'   \item `{n.risk}` number at risk at 'time' (within stratum if applicable)
#'   \item `{n.event}` number of observed events at 'time' (within stratum if applicable)
#'   \item `{n}` number of observations (within stratum if applicable)
#'   \item `{variable}` stratum variable (if applicable)
#'   \item `{level}` stratum level (if applicable )
#'   \item `{groupname}` label_level from original `tbl_survival()` call
#' }
#' @author Karissa Whiting
#' @family tbl_survival tools
#' @export
#' @examples
#' library(survival)
#' surv_table <-
#'   survfit(Surv(ttdeath, death) ~ trt, trial) %>%
#'   tbl_survival(times = c(12, 24))
#'
#' inline_text(surv_table,
#'   strata = "Drug",
#'   time = 12
#' )
inline_text.tbl_survival <-
  function(x, strata = NULL,
             time = NULL, prob = NULL,
             pattern = "{estimate} ({conf.level*100}% CI {ci})",
             estimate_fun = x$estimate_fun,
             ...) {

    # input checks ---------------------------------------------------------------
    if (c(is.null(time), is.null(prob)) %>% sum() != 1) {
      stop("One and only one of 'time' and 'prob' must be specified.")
    }
    if (!is.null(time)) {
      if (time < 0) stop("Must specify a positive 'time'.")
    }
    if (!is.null(prob)) {
      if (prob < 0 | prob > 1) stop("Must specify a 'prob' between 0 and 1.")
    }

    # creating a var that is either time or prob (the fixed variable)
    fixed_val <- time %||% prob

    if (length(fixed_val) != 1) stop("'time' or 'prob' must be length 1")

    if (!is.null(time)) {
      result <-
        x$table_long %>%
        mutate(fixed_var = time)
    }
    if (!is.null(prob)) {
      result <-
        x$table_long %>%
        mutate(fixed_var = prob)
    }



    # select strata ------------------------------------------------------------
    # if multiple strata exist in tbl_survival, grab rows matching specified strata
    if ("strata" %in% names(x$table_long)) {
      if (is.null(strata)) {
        stop(glue(
          "Must specify one of the following strata: ",
          "{pluck(x, 'table_long', 'level') %>% unique() %>% paste(collapse = ', ')}"
        ))
      }

      result <-
        result %>%
        filter(!!parse_expr(glue("level == '{strata}'")))

      if (nrow(result) == 0) {
        stop(glue(
          "Is the strata name spelled correctly? strata must be one of: ",
          "{pluck(x, 'table_long', 'level') %>% unique() %>% paste(collapse = ', ')}"
        ))
      }
    } else {
      if (!is.null(strata)) {
        warning(glue("Ignoring strata = '{strata}'. No strata in tbl_survival. "))
      }
    }

    # select time --------------------------------------------------------------
    # when specified timpoint is not in tbl_survival,
    # return result for closest time and give warning
    display_fixed <- result$fixed_var[which.min(abs(result$fixed_var - fixed_val))]
    if (!fixed_val %in% result$fixed_var) {
      message(glue(
        "Specified 'time' or 'prob' not in 'x': '{fixed_val}'. ",
        "Displaying nearest estimate: {display_fixed}"
      ))
    }

    result <-
      result %>%
      filter(.data$fixed_var == display_fixed)

    # formatting result and returning ------------------------------------------
    result <-
      result %>%
      mutate_at(
        vars(c("estimate", "conf.low", "conf.high")),
        estimate_fun
      ) %>%
      mutate(
        conf.level = x$survfit$conf.int,
        stat = glue(pattern)
      ) %>%
      pull("stat")

    result
  }
