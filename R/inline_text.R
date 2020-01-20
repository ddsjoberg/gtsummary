#' Report statistics from gtsummary tables inline
#'
#' @param x Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @return A string reporting results from a gtsummary table
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
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#'
#' @param x Object created from  [tbl_summary]
#' @param variable Variable name of statistic to present
#' @param level Level of the variable to display for categorical variables.
#' Can also specify the 'Unknown' row.  Default is `NULL`
#' @param column Column name to return from `x$table_body`.
#' Can also pass the level of a by variable.
#' @param pattern String indicating the statistics to return.
#' Uses [glue::glue] formatting. Default is pattern shown in `tbl_summary()` output
#' @inheritParams tbl_regression
#' @param ... Not used
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A string reporting results from a gtsummary table
#' @examples
#' t1 <- tbl_summary(trial)
#' t2 <- tbl_summary(trial, by = trt) %>% add_p()
#'
#' inline_text(t1, variable = age)
#' inline_text(t2, variable = grade, level = "I", column = "Drug A",
#' pattern = "{n}/{N} ({p})%")
#' inline_text(t2, variable = grade, column = "p.value")
inline_text.tbl_summary <-
  function(x, variable, column = NULL, level = NULL, pattern = NULL,
           pvalue_fun = function(x) style_pvalue(x, prepend_p = TRUE), ...) {
    # create rlang::enquo() inputs ---------------------------------------------
    variable <- rlang::enquo(variable)
    column <- rlang::enquo(column)
    level <- rlang::enquo(level)

    # checking variable input --------------------------------------------------
    variable <-
      var_input_to_string(
        data = vctr_2_tibble(x$meta_data$variable), arg_name = "variable",
        select_single = TRUE, select_input = !!variable
      )

    # selecting variable rwo from meta_data
    meta_data <- x$meta_data %>%
      filter(.data$variable == !!variable)

    # setting defaults ---------------------------------------------------------
    pattern_arg_null <- is.null(pattern)
    pattern <- pattern %||% meta_data$stat_display
    # selecting default column, if column is NULL
    if (rlang::quo_is_null(column) && is.null(x$by)) {
      column <- rlang::quo("stat_0")
    }
    else if (rlang::quo_is_null(column) && !is.null(x$by)) {
      stop("Must specify `column` argument.", call. = FALSE)
    }

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
          x$df_by[c("by_chr", "by_col")] %>% set_names(c("input", "column_name"))
        )
    }

    # selecting proper column name
    column <-
      var_input_to_string(
        data = vctr_2_tibble(col_lookup_table$input), arg_name = "column",
        select_single = TRUE, select_input = !!column
      )

    column <- col_lookup_table %>%
      filter(.data$input == !!column) %>%
      slice(1) %>%
      pull(.data$column_name)


    # select value from table --------------------------------------------------
    # if user passed a pattern AND column is stat_0, stat_1, etc, then replacing
    # table_body object with rebuilt version using pattern
    if (pattern_arg_null == FALSE && startsWith(column, "stat_")) {
      result <-
        df_stats_to_tbl(
          data = x$inputs$data, variable = variable,
          summary_type = meta_data$summary_type, by = x$by,
          var_label = meta_data$var_label, stat_display = pattern,
          df_stats = meta_data$df_stats[[1]], missing = "no", missing_text = "Unknown"
        )
    }
    else {
      result <-
        x$table_body %>%
        filter(.data$variable == !!variable)
    }

    # select variable level ----------------------------------------------------
    if (rlang::quo_is_null(level)) {
      result <- result %>% slice(1)
    }
    else {
      level <-
        var_input_to_string(
          data = vctr_2_tibble(filter(result, .data$row_type != "label") %>%
                                 pull(.data$label)),
          arg_name = "level", select_single = TRUE, select_input = !!level
        )

      result <-
        result %>%
        filter(.data$label == !!level)
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
#' location of the statistic to report and returns statistics for reporting
#' inline in an R markdown document.  Detailed examples in the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#'
#' @param x Object created from  [tbl_regression]
#' @param variable Variable name of statistics to present
#' @param level Level of the variable to display for categorical variables.
#' Default is `NULL`, returning the top row in the table for the variable.
#' @param pattern String indicating the statistics to return.
#' Uses [glue::glue] formatting.
#' Default is \code{"{estimate} ({conf.level }\% CI  {conf.low}, {conf.high}; {p.value})"}.
#' All columns from `x$table_body` are available to print as well as the
#' confidence level (conf.level). See below for details.
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
#' @param ... Not used
#' @author Daniel D. Sjoberg
#' @family tbl_regression tools
#' @export
#' @return A string reporting results from a gtsummary table
#' @examples
#' inline_text_ex1 <-
#'   glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
#' inline_text(inline_text_ex1, variable = age)
#' inline_text(inline_text_ex1, variable = grade, level = "III")
inline_text.tbl_regression <-
  function(x, variable, level = NULL,
           pattern = "{estimate} ({conf.level*100}% CI {conf.low}, {conf.high}; {p.value})",
           estimate_fun = x$fmt_fun$estimate,
           pvalue_fun = function(x) style_pvalue(x, prepend_p = TRUE), ...) {
    # setting quos -------------------------------------------------------------
    variable <- rlang::enquo(variable)
    level <- rlang::enquo(level)

    # table_body preformatting -------------------------------------------------
    # this is only being performed for tbl_uvregression benefit
    # getting N on every row of the table
    n_vars <- names(x$table_body) %>% intersect(c("N", "nevent"))
    x$table_body <-
      left_join(
        x$table_body %>% select(-n_vars),
        x$table_body %>% filter(.data$row_type == "label") %>% select(c("variable", n_vars)) %>% distinct(),
        by = "variable"
      )

    # select variable ----------------------------------------------------------
    variable <-
      var_input_to_string(
        data = vctr_2_tibble(unique(x$table_body$variable)), arg_name = "variable",
        select_single = TRUE, select_input = !!variable
      )

    # grabbing rows matching variable
    filter_expr <-
      result <-
      x$table_body %>%
      filter(.data$variable ==  !!variable)

    # select variable level ----------------------------------------------------
    if (rlang::quo_is_null(level)) {
      result <- result %>% slice(1)
    }
    else {
      level <-
        var_input_to_string(
          data = vctr_2_tibble(filter(result, .data$row_type != "label") %>%
                                 pull(.data$label)),
          arg_name = "level", select_single = TRUE, select_input = !!level
        )

      result <-
        result %>%
        filter(.data$label == !!level)
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
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#'
#' @param x Object created from [tbl_uvregression]
#' @inherit inline_text.tbl_regression
#' @family tbl_uvregression tools
#' @export
#' @return A string reporting results from a gtsummary table
#' @examples
#' inline_text_ex1 <-
#'   trial[c("response", "age", "grade")] %>%
#'   tbl_uvregression(
#'     method = glm,
#'     method.args = list(family = binomial),
#'     y = response,
#'     exponentiate = TRUE
#'   )
#'
#' inline_text(inline_text_ex1, variable = age)
#' inline_text(inline_text_ex1, variable = grade, level = "III")
inline_text.tbl_uvregression <- inline_text.tbl_regression


#' Report statistics from survival summary tables inline
#'
# 'Extracts and returns statistics from a table created by [tbl_survival]
#' for inline reporting in an R markdown document.
#'
#' @param x Object created from  [tbl_survival]
#' @param strata If `tbl_survival` estimates are stratified, level of the stratum
#' to report. Default is `NULL` when `tbl_survival` have no specified strata.
#' @param time Time for which to return survival probability
#' @param prob Probability for which to return survival time.  For median
#' survival use `prob = 0.50`
#' @param pattern String indicating the statistics to return.  Uses
#' [glue::glue] formatting.
#' Default is \code{'{estimate} ({conf.level*100}\% {ci})'}.  All columns from
#' `x$table_long` are available to print as well as the
#' confidence level (conf.level). See below for details.
#' @param estimate_fun function to round/style estimate and lower/upper
#' confidence interval estimates.  Note, this does not style the 'ci' column,
#' which is a string. Default is x$estimate_fun
#' @param ... Not used
#'
#' @section pattern argument:
#' The following items are available to print.  Use `print(x$table_long)` to
#' print the table the estimates are extracted from.
#' \itemize{
#'   \item `{label}` 'time' or 'prob' label
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
#' @return A string reporting results from a gtsummary table
#' @export
#' @examples
#' library(survival)
#' surv_table <-
#'   survfit(Surv(ttdeath, death) ~ trt, trial) %>%
#'   tbl_survival(times = c(12, 24))
#'
#' inline_text(surv_table,
#'   strata = "Drug A",
#'   time = 12
#' )
inline_text.tbl_survival <-
  function(x, strata = NULL,
           time = NULL, prob = NULL,
           pattern = "{estimate} ({conf.level*100}% CI {ci})",
           estimate_fun = x$fmt_fun$estimate,
           ...) {

    # input checks -------------------------------------------------------------
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
    display_fixed <-
      result$fixed_var[which.min(abs(result$fixed_var - fixed_val))]
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
