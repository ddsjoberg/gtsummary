#' Report statistics from gtsummary tables inline
#'
#' @param x Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @return A string reporting results from a gtsummary table
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @seealso [inline_text.tbl_summary], [inline_text.tbl_svysummary],
#' [inline_text.tbl_regression], [inline_text.tbl_uvregression],
#' [inline_text.tbl_survfit], [inline_text.tbl_cross], [inline_text.gtsummary]
#'
#' @export
inline_text <- function(x, ...) {
  UseMethod("inline_text")
}

#' Report statistics from summary tables inline
#'
#' @param x gtsummary object
#' @param variable Variable name of statistic to present
#' @param level Level of the variable to display for categorical variables.
#' Default is `NULL`
#' @param column Column name to return from `x$table_body`.
#' @param pattern String indicating the statistics to return.
#' Uses [glue::glue] formatting. Default is `NULL`
#' @param ... Not used
#' @export
#' @name inline_text.gtsummary
#'
#' @section column + pattern:
#'
#' Some gtsummary tables report multiple statistics in a single cell,
#' e.g. `"{mean} ({sd})"` in `tbl_summary()` or `tbl_svysummary()`.
#' We often need to report just the mean or the SD, and that can be accomplished
#' by using both the `column=` and `pattern=` arguments. When both of these
#' arguments are specified, the column argument selects the column to report
#' statistics from, and the pattern argument specifies which statistics to report,
#' e.g. `inline_text(x, column = "stat_1", pattern = "{mean}")` reports just the
#' mean from a `tbl_summary()`.

inline_text.gtsummary <- function(x, variable,
                                  level = NULL, column = NULL, pattern = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  column <- rlang::enquo(column)
  column_is_null <- tryCatch(is.null(eval_tidy(column)), error = function(e) FALSE)
  level <- rlang::enquo(level)
  level_is_null <- tryCatch(is.null(eval_tidy(level)), error = function(e) FALSE)

  # adding raw stats if user will use them -------------------------------------
  if (!is.null(pattern) && !column_is_null) {
    if (is.null(x$meta_data) || !"df_stats" %in% names(x$meta_data) ||
        !all(c("label", "col_name") %in% names(x$meta_data$df_stats[[1]]))) {
      paste(
        "When both `column=` and `pattern=` are specified, the gtsummary",
        "object must have a `x$meta_data` table with column 'df_stats',",
        "and the 'df_stats' data frame must have columns 'label' and 'col_name'."
      ) %>%
        abort()
    }

    x <- df_stats_to_table_body(x)
  }

  # convert gtsummary object to tibble -----------------------------------------
  # removing merging and other styling
  x$table_styling$cols_merge <- filter(x$table_styling$cols_merge, FALSE)
  x$table_styling$text_format <- filter(x$table_styling$text_format, FALSE)
  # keeping all columns
  x$table_styling$header$hide <- FALSE

  df_gtsummary <- as_tibble(x, col_labels = FALSE)

  # variable selection ---------------------------------------------------------
  variable <-
    .select_to_varnames({{ variable }},
      var_info = x$table_body,
      arg_name = "variable",
      select_single = TRUE
    )

  df_gtsummary <- filter(df_gtsummary, .data$variable %in% .env$variable)

  # check if tbl contains duplicate variable names
  if ("row_type" %in% names(df_gtsummary) &&
      nrow(df_gtsummary %>% filter(.data$row_type %in% "label")) > 1)
    glue("Variable '{variable}' likely not unique in gtsummary table, and",
         "the cell(s) you wish to display may not be accessible.",
         "This may occur when gtsummary tables with repeated variable",
         "names are combined using `tbl_stack()`.", .sep = " ") %>%
    stringr::str_wrap() %>%
    inform()

  # level selection ------------------------------------------------------------
  if (!level_is_null && "" %in% df_gtsummary$label) {
    paste(
      "There is a blank level, which may cause issues selecting",
      "the specified value. Blank levels cannot be selected."
    ) %>%
      inform()
  }

  # if level not provided, keep the first row
  if (level_is_null) {
    df_gtsummary <- filter(df_gtsummary, dplyr::row_number() == 1)
  } # if there is a level, drop first label row, keeping the levels only
  else {
    df_gtsummary <-
      filter(df_gtsummary, !(.data$row_type %in% "label" & dplyr::row_number() == 1))
    level <-
      .select_to_varnames(!!level,
        var_info = df_gtsummary$label,
        arg_name = "level",
        select_single = TRUE
      )
    df_gtsummary <- filter(df_gtsummary, .data$label %in% .env$level)
  }

  # assert we've selected one line of table ------------------------------------
  if (nrow(df_gtsummary) != 1L) abort("Criteria must select exactly one row.")

  # cell/pattern selection -----------------------------------------------------
  if (!column_is_null) {
    column <-
      .select_to_varnames(!!column,
        data = df_gtsummary,
        arg_name = "column",
        select_single = TRUE
      )
  }

  # check pattern argument input
  if (!is.null(pattern) && !rlang::is_string(pattern)) {
    abort("`pattern=` argument must be a string.")
  }

  # if column selected and not pattern, return column
  if (!column_is_null && is.null(pattern)) {
    return(dplyr::pull(df_gtsummary, all_of(column)))
  }

  # if using pattern and column name, rename the columns that comprise column name
  if (!is.null(pattern) && !column_is_null) {
    df_gtsummary <-
      df_gtsummary %>%
      bind_cols(
        select(., starts_with(paste0("raw_", column, "_"))) %>%
        dplyr::rename_with(
          .fn = ~stringr::str_remove(., fixed(paste0("raw_", column, "_"))),
          .cols = starts_with(paste0("raw_", column, "_"))
        )
      )
  }

  # if no column and pattern, return pattern
  if (!is.null(pattern)) {
    return(glue::glue_data(df_gtsummary, pattern) %>% as.character())
  }

  # must select column or pattern
  if (column_is_null && is.null(pattern)) {
    abort("Both `column=` and `pattern=` cannot be NULL")
  }
}

# function to apply the fmt_fun attr in df_stats
.apply_attr_fmt_fun <- function(x) {
  map(
    x,
    function(.x) {
      if (!is.null(attr(.x, "fmt_fun"))) {
        return(unname(do.call(attr(.x, "fmt_fun"), list(.x))))
      }
      return(.x)
    }
  )
}

#' Report statistics from summary tables inline
#'
#' Extracts and returns statistics from a `tbl_summary` object for
#' inline reporting in an R markdown document. Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
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
#' t1 <- trial[c("trt", "grade")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p()
#'
#' inline_text(t1, variable = grade, level = "I", column = "Drug A", pattern = "{n}/{N} ({p})%")
#' inline_text(t1, variable = grade, column = "p.value")
inline_text.tbl_summary <- function(x, variable, column = NULL, level = NULL,
                                    pattern = NULL, pvalue_fun = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  # setting defaults ---------------------------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("pkgwide-fn:prependpvalue_fun") %||%
    (function(x) style_pvalue(x, prepend_p = TRUE)) %>%
    gts_mapper("inline_text(pvalue_fun=)")
  x <- modify_fmt_fun(x, any_of("p.value") ~ pvalue_fun)

  # create rlang::enquo() inputs ---------------------------------------------
  variable <- rlang::enquo(variable)
  column <- rlang::enquo(column)
  level <- rlang::enquo(level)

  # checking column ----------------------------------------------------------
  column_is_null <- tryCatch(is.null(eval_tidy(column)), error = function(e) FALSE)
  if (!column_is_null) {
    # the following code converts the column input to a column name in x$table_body
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
      .select_to_varnames(
        select = !!column,
        var_info = col_lookup_table$input,
        arg_name = "column",
        select_single = TRUE
      )

    column <-
      col_lookup_table %>%
      filter(.data$input == !!column) %>%
      slice(1) %>%
      pull(.data$column_name)
  }
  else if (column_is_null && is.null(x$by)) column <- "stat_0"

  # call generic inline_text() function ----------------------------------------
  inline_text.gtsummary(
    x = x,
    variable = !!variable,
    level = !!level,
    column = !!column,
    pattern = pattern
  )
}

#' @name inline_text.tbl_summary
#' @export
inline_text.tbl_svysummary <- inline_text.tbl_summary

#' Report statistics from regression summary tables inline
#'
#' Takes an object with class `tbl_regression`, and the
#' location of the statistic to report and returns statistics for reporting
#' inline in an R markdown document.  Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
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
#' The following items (and more) are available to print.  Use `print(x$table_body)` to
#' print the table the estimates are extracted from.
#' \itemize{
#'   \item `{estimate}` coefficient estimate formatted with 'estimate_fun'
#'   \item `{conf.low}` lower limit of confidence interval formatted with 'estimate_fun'
#'   \item `{conf.high}` upper limit of confidence interval formatted with 'estimate_fun'
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
           estimate_fun = NULL, pvalue_fun = NULL, ...) {
    check_dots_empty(error = function(e) inform(c(e$message, e$body)))
    # setting defaults ---------------------------------------------------------
    pvalue_fun <-
      pvalue_fun %||%
      get_theme_element("pkgwide-fn:prependpvalue_fun") %||%
      (function(x) style_pvalue(x, prepend_p = TRUE)) %>%
      gts_mapper("inline_text(pvalue_fun=)")
    x <- modify_fmt_fun(x, any_of("p.value") ~ pvalue_fun)

    if (!is.null(estimate_fun)) {
      estimate_fun <- estimate_fun %>% gts_mapper("inline_text(estimate_fun=)")
      x <- modify_fmt_fun(x, any_of(c("estimate", "conf.low", "conf.high")) ~ estimate_fun)
    }
    x <-
      modify_table_body(x, ~ .x %>% mutate(conf.level = x$inputs$conf.level)) %>%
      modify_fmt_fun(conf.level ~ as.numeric)

    # call generic inline_text() function ----------------------------------------
    inline_text.gtsummary(
      x = x,
      variable = {{ variable }},
      level = {{ level }},
      pattern = pattern
    )
  }


#' Report statistics from regression summary tables inline
#'
#' Extracts and returns statistics from a table created by the `tbl_uvregression`
#' function for inline reporting in an R markdown document.
#' Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#'
#' @param x Object created from [tbl_uvregression]
#' @inherit inline_text.tbl_regression
#' @family tbl_uvregression tools
#' @export
#' @return A string reporting results from a gtsummary table
#' @examples
#' \donttest{
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
#' }
inline_text.tbl_uvregression <- inline_text.tbl_regression


#' Report statistics from survival summary tables inline
#'
#' \lifecycle{deprecated}
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
#' @keywords internal
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
           estimate_fun = NULL,
           ...) {
    check_dots_empty(error = function(e) inform(c(e$message, e$body)))
    if (is.null(x$table_styling)) x <- .convert_table_header_to_styling(x)

    # setting defaults ---------------------------------------------------------
    if (is.null(estimate_fun)) {
      estimate_fun <-
        x$table_styling$fmt_fun %>%
        filter(.data$column == "estimate") %>%
        dplyr::slice_tail() %>%
        pull("fmt_fun") %>%
        purrr::pluck(1)
    }

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

#' Report statistics from survfit tables inline
#'
#' \lifecycle{maturing}
#' Extracts and returns statistics from a `tbl_survfit` object for
#' inline reporting in an R markdown document. Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#'
#' @param x Object created from  [tbl_survfit]
#' @param time time for which to return survival probabilities.
#' @param prob probability with values in (0,1)
#' @param column column to print from `x$table_body`.
#' Columns may be selected with `time=` or `prob=` as well.
#' @param variable Variable name of statistic to present.
#' @param pattern String indicating the statistics to return.
#' @param level Level of the variable to display for categorical variables.
#' Can also specify the 'Unknown' row.  Default is `NULL`
#' @param estimate_fun Function to round and format estimate and confidence limits.
#' Default is the same function used in `tbl_survfit()`
#' @inheritParams tbl_regression
#' @param ... Not used
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A string reporting results from a gtsummary table
#' @examples
#' library(survival)
#' # fit survfit
#' fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
#' fit2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
#'
#' # sumarize survfit objects
#' tbl1 <-
#'   tbl_survfit(
#'     fit1,
#'     times = c(12, 24),
#'     label = "Treatment",
#'     label_header = "**{time} Month**"
#'   ) %>%
#'   add_p()
#'
#' tbl2 <-
#'   tbl_survfit(
#'     fit2,
#'     probs = 0.5,
#'     label_header = "**Median Survival**"
#'   )
#'
#' # report results inline
#' inline_text(tbl1, time = 24, level = "Drug B")
#' inline_text(tbl1, column = p.value)
#' inline_text(tbl2, prob = 0.5)
inline_text.tbl_survfit <-
  function(x, variable = NULL, level = NULL,
           pattern = NULL, time = NULL, prob = NULL, column = NULL,
           estimate_fun = x$inputs$estimate_fun, pvalue_fun = NULL, ...) {
    check_dots_empty(error = function(e) inform(c(e$message, e$body)))
    # quoting inputs -------------------------------------------------------------
    variable <- rlang::enquo(variable)
    column <- rlang::enquo(column)

    # setting defaults ---------------------------------------------------------
    pvalue_fun <-
      pvalue_fun %||%
      get_theme_element("pkgwide-fn:prependpvalue_fun") %||%
      (function(x) style_pvalue(x, prepend_p = TRUE)) %>%
      gts_mapper("inline_text(pvalue_fun=)")
    estimate_fun <-
      estimate_fun %>%
      gts_mapper("inline_text(estimate_fun=)")

    # applying formatting functions --------------------------------------------
    x <- modify_fmt_fun(x, any_of("p.value") ~ pvalue_fun)
    x$meta_data$df_stats <-
      x$meta_data$df_stats %>%
      purrr::map(
        function(.x) {
          for (v in names(.x)) {
            if (v %in% c("estimate", "conf.high", "conf.low")) {
              attr(.x[[v]], "fmt_fun") <- estimate_fun
            }
          }
          .x
        }
      )

    # checking inputs ----------------------------------------------------------
    if (c(!is.null(time), !is.null(prob), !rlang::quo_is_null(column)) %>% sum() != 1) {
      stop("One and only one of `time=`, `prob=`, and `column=` must be specified.", call. = FALSE)
    }

    # selecting column ---------------------------------------------------------
    if (!is.null(time)) {
      if (!time %in% x$inputs$times) {
        glue("`time=` must be one of {quoted_list(x$inputs$times)}") %>% abort()
      }
      column <- which(x$inputs$times %in% time) %>% {
        paste0("stat_", .)
      }
    }
    if (!is.null(prob)) {
      if (!prob %in% x$inputs$probs) {
        glue("`prob=` must be one of {quoted_list(x$inputs$probs)}") %>% abort()
      }
      column <- which(x$inputs$probs %in% prob) %>% {
        paste0("stat_", .)
      }
    }
    column <- x$table_body %>%
      select({{ column }}) %>%
      names()

    # select variable
    variable_is_null <- tryCatch(is.null(eval_tidy(variable)), error = function(e) FALSE)
    if (variable_is_null) variable <- x$table_body$variable[1]

    # call generic inline_text() function ----------------------------------------
    inline_text.gtsummary(
      x = x,
      variable = !!variable,
      level = {{ level }},
      column = column,
      pattern = pattern
    )
  }


#' Report statistics from cross table inline
#'
#' \lifecycle{maturing}
#' Extracts and returns statistics from a `tbl_cross` object for
#' inline reporting in an R markdown document. Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#' @param x a `tbl_cross` object
#' @param row_level Level of the row variable to display.
#' Can also specify the 'Unknown' row. Default is `NULL`
#' @param col_level Level of the column variable to display. Default is `NULL`
#' Can also specify "`p.value`" for the p-value and "`stat_0`" for Total column.
#' @inheritParams inline_text.tbl_summary
#'
#' @return A string reporting results from a gtsummary table
#' @family tbl_cross tools
#' @export
#' @examples
#' \donttest{
#' tbl_cross <-
#'   tbl_cross(trial, row = trt, col = response) %>%
#'   add_p()
#'
#' inline_text(tbl_cross, row_level = "Drug A", col_level = "1")
#' inline_text(tbl_cross, row_level = "Total", col_level = "1")
#' inline_text(tbl_cross, col_level = "p.value")
#' }
inline_text.tbl_cross <-
  function(x, col_level = NULL, row_level = NULL,
           pvalue_fun = NULL, ...) {
    check_dots_empty(error = function(e) inform(c(e$message, e$body)))

    # check arguments ----------------------------------------------------------
    if (is.null(col_level) | (is.null(row_level) & !identical("p.value", col_level))) {
      stop("Please specify both `col_level=` and `row_level=` arguments")
    }

    # setting defaults ---------------------------------------------------------
    pvalue_fun <-
      pvalue_fun %||%
      get_theme_element("pkgwide-fn:prependpvalue_fun") %||%
      (function(x) style_pvalue(x, prepend_p = TRUE)) %>%
      gts_mapper("inline_text(pvalue_fun=)")

    # row_level ----------------------------------------------------------------
    # converting row_level to a string
    row_level <-
      .select_to_varnames(
        select = {{ row_level }},
        var_info = x$table_body$label,
        arg_name = "row_level",
        select_single = TRUE
      )

    # assessing if user selected total row
    if (!is.null(row_level) && row_level == x$inputs$margin_text && "..total.." %in% x$meta_data$variable) {
      variable <- "..total.."
      row_level <- NULL
    }
    else {
      variable <- x$inputs$row
    }

    # col_level ----------------------------------------------------------------
    col_lookup_table <-
      tibble(
        input = names(x$table_body),
        column_name = names(x$table_body)
      ) %>%
      bind_rows(
        x$df_by[c("by_chr", "by_col")] %>% set_names(c("input", "column_name"))
      )

    if ("stat_0" %in% names(x$table_body)) {
      col_lookup_table <-
        col_lookup_table %>%
        bind_rows(
          tibble(
            input = x$inputs$margin_text,
            column_name = "stat_0"
          )
        )
    }

    # selecting proper column name
    col_level <-
      .select_to_varnames(
        select = {{ col_level }},
        var_info = col_lookup_table$input,
        arg_name = "col_level",
        select_single = TRUE
      )

    col_level <- col_lookup_table %>%
      filter(.data$input == col_level) %>%
      slice(1) %>%
      pull(.data$column_name)

    # replacing passed data with, tbl_data (only data used in table) -----------
    x$inputs$data <- x$tbl_data

    # evaluating inline_text for tbl_summary -----------------------------------
    expr(
      inline_text.tbl_summary(x,
        variable = !!variable, level = !!row_level,
        column = {{ col_level }}, pvalue_fun = !!pvalue_fun
      )
    ) %>%
      eval()
  }








df_stats_to_table_body <- function(x) {
  # transpose stats to long format for table_body ------------------------------
  df_raw_stats <-
    purrr::pmap_dfr(
      list(x$meta_data$df_stats, x$meta_data$var_label),
      function(.x, .y) {
        .x$row_type <-
          ifelse("variable_levels" %in% names(.x), "level", "label")

        .x %>%
          select(-any_of(c("by", "stat_display", "variable_levels",
                           "col_label", "strata"))) %>%
          tidyr::pivot_wider(id_cols = any_of(c("variable", "label", "row_type")),
                             names_from = .data$col_name,
                             names_glue = "raw_{col_name}_{.value}",
                             values_from = -any_of(c("variable", "label", "row_type", "col_name")))  %>%
          select(any_of(c("variable", "row_type", "label")), everything())
      }
    )

  # prepare fmt_fun to be applied to the new columns ---------------------------
  df_fmt_fun <-
    x$meta_data$df_stats %>%
    purrr::map_dfr(
      function(df_stats) {
        tibble(
          colname =
            names(df_stats) %>%
            setdiff(c("variable", "by", "stat_display", "col_label", "strata",
                      "variable_levels", "label", "col_name"))
        ) %>%
          mutate(
            variable = unique(df_stats$variable),
            raw_colname = map(.data$colname, ~paste("raw", unique(df_stats$col_name), .x, sep = "_")),
            fmt_fun =
              map(
                .data$colname,
                ~ attr(df_stats[[.x]], "fmt_fun") %||%
                  x$inputs$estimate_fun %||%
                  style_sigfig
              )
          ) %>%
          unnest(.data$raw_colname) %>%
          select(-.data$colname)
      }
    ) %>%
    nest(raw_colname = .data$raw_colname) %>%
    dplyr::rowwise() %>%
    mutate(raw_colname = unlist(.data$raw_colname) %>% unname() %>% list()) %>%
    ungroup()

  expr_fmt_fun <-
    map(
      seq_len(nrow(df_fmt_fun)),
      ~expr(
        modify_table_styling(
          columns = !!df_fmt_fun$raw_colname[[.x]],
          rows = variable %in% !!df_fmt_fun$variable[.x],
          fmt_fun = !!df_fmt_fun$fmt_fun[[.x]]
        )
      )
    )

  # apply updates to gtsummary table -------------------------------------------
  # merge in columns of raw stats
  x <-
    modify_table_body(
      x,
      ~left_join(
        .x,
        df_raw_stats,
        by = c("variable", "row_type", "label")
      )
    )

  # apply formatting functions to new columns
  expr_fmt_fun %>%
    purrr::reduce(function(x, y) expr(!!x %>% !!y), .init = expr(!!x)) %>%
    eval()
}
