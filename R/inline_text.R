#' Report statistics from gtsummary tables inline
#'
#' @param x object created from a gtsummary funciton
#' @param ... further arguments passed to or from other methods.
#' @author Daniel Sjoberg
#' @seealso \link{inline_text.tbl_summary}, \link{tbl_summary}, \link{tbl_regression}, \link{tbl_uvregression}
#' @export
inline_text <- function(x, ...) UseMethod("inline_text")

#' Report statistics from summary tables inline
#'
#' Functions takes an object with class `tbl_summary`, and the
#' location of the statistic to report and returns the statistic for reporting
#' inline in an R markdown document
#'
#' @param x object created from  \link{tbl_summary}
#' @param variable variable name of statistic to present
#' @param level level of the variable to display for categorical variables.
#' Can also specify the 'Unknown' row.  Default is `NULL`
#' @param column name column to return.  Default is `stat_0`, when
#' no by variable is passed to `tbl_summary()`.  See below for details.
#' @param pvalue_fun function for rounding/formatting p-values.
#' Default is \code{\link{style_pvalue}}.
#' The function must have a single input (the numeric, exact p-value),
#' and return a string that is the rounded/formatted p-value (e.g.
#' \code{pvalue_fun = function(x) style_pvalue(x, digits = 2)} or equivalently,
#'  \code{purrr::partial(style_pvalue, digits = 2)}).
#' @param ... not used
#' @author Daniel Sjoberg
#' @export
#'
#' @section Selecting Columns:
#' Using the `column` argument, select the column to return.  The argument
#' accepts any column name in the `.$table_body` data frame.  Overall statistics
#' are saved in `stat_0`.  When a `by` argument is passed to `tbl_summary()`, the
#' first by level (when ordered) is `stat_1`, the second is `stat_2`, and so on.
#' A helper function `by_level` is available to aid in selecting the correct
#' column.  If a `by` variable has levels 'Drug' and 'Placebo',
#' then `by_level('Drug')` would return `stat_1`

inline_text.tbl_summary <-
  function(x, variable, level = NULL,
           column = ifelse(is.null(x$by), "stat_0", stop("Must specify column")),
           pvalue_fun = purrr::partial(style_pvalue, prepend_p = TRUE), ...) {
    # evaluating column --------------------------------------------------------
    # the by column cna be passed as a string, or as a function that results
    # in a string.  Here, we evaluate the possible function to ensuer a string
    column <- glue("{column}")
    print(column)

    # select variable ----------------------------------------------------------
    # grabbing rows matching variable
    filter_expr <-
      result <-
      x$table_body %>%
      filter(!!parse_expr(glue("variable ==  '{variable}'")))

    # select variable level ----------------------------------------------------
    if(is.null(level)) {
      result <- result %>% slice(1)
    }
    else {
      result <-
        result %>% filter(!!parse_expr(glue("label ==  '{level}'")))
    }

    if(nrow(result) == 0)
      stop("No statistic selected. Is the variable name and/or level spelled correctly?")

    # select column ------------------------------------------------------------
    result <- result %>% pull(column)

    # return statistic ---------------------------------------------------------
    if(column %in% c("pvalue", "qvalue")) {
      return(pvalue_fun(result))
    }

    result
  }


#' Report statistics from regression summary tables inline
#'
#' Functions takes an object with class `tbl_regression`, and the
#' location of the statistic to report and returns the statistic for reporting
#' inline in an R markdown document
#'
#' @param x object created from  \link{tbl_regression}
#' @param variable variable name of statistic to present
#' @param level level of the variable to display for categorical variables.
#' Default is `NULL`, returning the top row in the table for the variable.
#' @param pattern statistics to return.  Uses \link[glue]{glue} formatting.
#' Default is "{coef} ({conf.level}% CI {ll}, {ul}; {pvalue})".  All columns from
#' `.$table_body` are available to print as well as the confidence level (conf.level)
#' @param coef_fun function to style model coefficients.
#' Columns 'coef', 'll', and 'ul' are formatted. Default is `x$inputs$coef_fun`
#' @param pvalue_fun function to style p-values and/or q-values.
#' Default is `function(x) style_pvalue(x, prepend_p = TRUE)`
#' @param ... not used
#' @author Daniel Sjoberg
#' @export

inline_text.tbl_regression <-
  function(x, variable, level = NULL,
           pattern = "{coef} ({conf.level*100}% CI {ll}, {ul}; {pvalue})",
           coef_fun = x$inputs$coef_fun,
           pvalue_fun = function(x) style_pvalue(x, prepend_p = TRUE), ...) {
    # table_body preformatting -------------------------------------------------
    # this is only being performed for tbl_uvregression benefit
    # getting N on every row of the table
    x$table_body <-
      dplyr::left_join(
        x$table_body %>% select(-"N"),
        x$table_body %>% filter_('row_type == "label"') %>% select(c("variable", "N")),
        by = "variable"
      )

    # select variable ----------------------------------------------------------
    # grabbing rows matching variable
    filter_expr <-
      result <-
      x$table_body %>%
      filter(!!parse_expr(glue("variable ==  '{variable}'")))

    # select variable level ----------------------------------------------------
    if(is.null(level)) {
      result <- result %>% slice(1)
    }
    else {
      result <-
        result %>% filter(!!parse_expr(glue("label ==  '{level}'")))
    }

    if(nrow(result) == 0)
      stop("No statistic selected. Is the variable name and/or level spelled correctly?")

    # calculating statistic ----------------------------------------------------
    pvalue_cols <- names(result) %>% intersect(c("pvalue", "qvalue"))
    result <-
      result %>%
      mutate_at(vars(one_of(c("coef", "ll", "ul"))), coef_fun) %>%
      mutate_at(vars(one_of(pvalue_cols)), pvalue_fun) %>%
      mutate_(
        conf.level = ~x$inputs$conf.level,
        stat = ~glue(pattern)
      ) %>%
      pull("stat")

    result
  }


#' Report statistics from regression summary tables inline
#'
#' Functions takes an object with class `tbl_uvregression`, and the
#' location of the statistic to report and returns the statistic for reporting
#' inline in an R markdown document
#'
#' @inherit inline_text.tbl_regression
#' @export

inline_text.tbl_uvregression <- inline_text.tbl_regression


## helper function -------------------------------------------------------------
#' Convert level of by variable to column name
#'
#' To be used in the column argument of
#'
#' @param by_chr a level of the by variable inline_text.tbl_regression and
#' inline_text.tbl_uvregression only
#' @author Daniel Sjoberg
#' @export
by_level <-  function(by_chr) {
  # this function can only be run in environment
  # where there is an object x that is atbl_summary class
  result <-
    parse(text="x$df_by") %>% # hiding this in a string so i don't get a global variables warning
    eval() %>%
    dplyr::filter(!!parse_expr(glue::glue("by_chr ==  '{by_chr}'"))) %>%
    dplyr::pull("by_col")

  if(length(result) == 0) {
    glue::glue("'{x$df_by$by_chr}'") %>%
      paste(collapse = ", ") %>%
      {glue::glue("No by column selected. Choose one of: {.}")} %>%
      stop()
  }

  result
}
