#' Report statistics from gtsummary tables inline
#'
#' @param x object created from a gtsummary function
#' @param ... further arguments passed to or from other methods.
#' @author Daniel D. Sjoberg
#' @seealso \code{\link{inline_text.tbl_summary}},
#' \code{\link{inline_text.tbl_regression}},
#' \code{\link{inline_text.tbl_uvregression}}
#' @export
inline_text <- function(x, ...) UseMethod("inline_text")

#' Report statistics from summary tables inline
#'
#' Functions takes an object with class `tbl_summary`, and the
#' location of the statistic to report and returns the statistic for reporting
#' inline in an R markdown document.  Detailed examples in the
#' \href{https://github.com/pages/ddsjoberg/gtsummary/articles/tbl_summary.html}{`tbl_summary` vignette}
#'
#' @param x object created from  \link{tbl_summary}
#' @param variable variable name of statistic to present
#' @param level level of the variable to display for categorical variables.
#' Can also specify the 'Unknown' row.  Default is `NULL`
#' @param column name column name to return from `x$table_body`.
#' Can also pass the level of a by variable.
#' @inheritParams tbl_regression
#' @param ... not used
#' @family tbl_summary
#' @author Daniel D. Sjoberg
#' @export


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
        "{paste(col_lookup_table, collapse = ', ')}"
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
      levels_obs <- result %>% filter(!!parse_expr('row_type != "label"')) %>% pull("label")
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
    if (column %in% c("pvalue", "qvalue")) {
      return(pvalue_fun(result))
    }

    result
  }


#' Report statistics from regression summary tables inline
#'
#' Functions takes an object with class `tbl_regression`, and the
#' location of the statistic to report and returns the statistic for reporting
#' inline in an R markdown document.  Detailed examples in the
#' \href{https://github.com/pages/ddsjoberg/gtsummary/articles/tbl_regression.html}{`tbl_regression` vignette}
#'
#' @param x object created from  \link{tbl_regression}
#' @param variable variable name of statistic to present
#' @param level level of the variable to display for categorical variables.
#' Default is `NULL`, returning the top row in the table for the variable.
#' @param pattern statistics to return.  Uses \link[glue]{glue} formatting.
#' Default is \code{"{coef} ({conf.level }\% CI  {ll}, {ul}; {pvalue})"}.  All columns from
#' `.$table_body` are available to print as well as the confidence level (conf.level)
#' @param coef_fun function to style model coefficients.
#' Columns 'coef', 'll', and 'ul' are formatted. Default is `x$inputs$coef_fun`
#' @param pvalue_fun function to style p-values and/or q-values.
#' Default is `function(x) style_pvalue(x, prepend_p = TRUE)`
#' @param ... not used
#' @author Daniel D. Sjoberg
#' @family tbl_regression
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
      left_join(
        x$table_body %>% select(-"N"),
        x$table_body %>% filter(!!parse_expr('row_type != "label"')) %>% select(c("variable", "N")) %>% distinct(),
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
      levels_obs <- result %>% filter(!!parse_expr('row_type != "label"')) %>% pull("label")
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
    pvalue_cols <- names(result) %>% intersect(c("pvalue", "qvalue"))
    result <-
      result %>%
      mutate_at(vars(one_of(c("coef", "ll", "ul"))), coef_fun) %>%
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
#' Functions takes an object with class `tbl_uvregression`, and the
#' location of the statistic to report and returns the statistic for reporting
#' inline in an R markdown document. Detailed examples in the
#' \href{https://github.com/pages/ddsjoberg/gtsummary/articles/tbl_regression.html}{`tbl_regression` vignette}
#'
#' @inherit inline_text.tbl_regression
#' @family tbl_uvregression
#' @export

inline_text.tbl_uvregression <- inline_text.tbl_regression
