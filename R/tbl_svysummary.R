#' Create a table of summary statistics from a survey object
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' The `tbl_svysummary` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables taking into account survey weights and design.
#' It is similar to [tbl_summary()].
#'
#' @param data A survey object created with created with [survey::svydesign()]
#' @inheritParams tbl_summary
#'
#' @details
#' See [tbl_summary()] for more details on the different arguments.
#'
#' @section statistic argument:
#' The statistic argument specifies the statistics presented in the table. The
#' input is a list of formulas that specify the statistics to report. For example,
#' `statistic = list(age ~ "{mean} ({sd})")` would report the mean and
#' standard deviation for age; `statistic = list(all_continuous() ~ "{mean} ({sd})")`
#' would report the mean and standard deviation for all continuous variables.
#'  A statistic name that appears between curly brackets
#' will be replaced with the numeric statistic (see [glue::glue]).
#'
#' For categorical variables the following statistics are available to display.
#' \itemize{
#'   \item `{n}` frequency
#'   \item `{N}` denominator, or cohort size
#'   \item `{p}` formatted percentage
#'   \item `{n_unweighted}` unweighted frequency
#'   \item `{N_unweighted}` unweighted denominator
#'   \item `{p_unweighted}` unweighted formatted percentage
#' }
#' For continuous variables the following statistics are available to display.
#' \itemize{
#'   \item `{median}` median
#'   \item `{mean}` mean
#'   \item `{sd}` standard deviation
#'   \item `{var}` variance
#'   \item `{min}` minimum
#'   \item `{max}` maximum
#'   \item `{p##}` any integer percentile, where `##` is an integer from 0 to 100
#' }
#'
#' Unlike [tbl_summary()], It's not possible to pass a custom function.
#'
#' For both categorical and continuous variables, statistics on the number of
#' missing and non-missing observations and their proportions are available to
#' display.
#' \itemize{
#'   \item `{N_obs}` total number of observations
#'   \item `{N_miss}` number of missing observations
#'   \item `{N_nonmiss}` number of non-missing observations
#'   \item `{p_miss}` percentage of observations missing
#'   \item `{p_nonmiss}` percentage of observations not missing
#'   \item `{N_obs_unweighted}` unweighted total number of observations
#'   \item `{N_miss_unweighted}` unweighted number of missing observations
#'   \item `{N_nonmiss_unweighted}` unweighted number of non-missing observations
#'   \item `{p_miss_unweighted}` unweighted percentage of observations missing
#'   \item `{p_nonmiss_unweighted}` unweighted percentage of observations not missing
#' }
#'
#' Note that for categorical variables, `{N_obs}`, `{N_miss}` and `{N_nonmiss}` refer
#' to the total number, number missing and number non missing observations
#' in the denominator, not at each level of the categorical variable.
#' @export
#' @importFrom stats as.formula weights
#' @return A `tbl_svysummary` object
#' @family tbl_svysummary tools
#' @author Joseph Larmarange
#' @examples
#' # A simple weighted dataset
#' tbl_svysummary_ex1 <-
#'   survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) %>%
#'   tbl_svysummary(by = Survived, percent = "row")
#'
#' # A dataset with a complex design
#' data(api, package = "survey")
#' tbl_svysummary_ex2 <-
#'   survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) %>%
#'   tbl_svysummary(by = "both", include = c(cname, api00, api99, both))
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_svysummary_ex1.png}{options: width=31\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_svysummary_ex2.png}{options: width=45\%}}
tbl_svysummary <- function(data, by = NULL, label = NULL, statistic = NULL,
                        digits = NULL, type = NULL, value = NULL,
                        missing = NULL, missing_text = NULL, sort = NULL,
                        percent = NULL, include = NULL) {
  # checking for survey package ------------------------------------------------
  assert_package("survey", "tbl_svysummary")

  # eval -----------------------------------------------------------------------
  include <- select(data$variables, {{ include }}) %>% names()

  # default selection for include
  if (length(include) == 0) {
    # look at data$call
    if (is.null(data$call)) {
      include <- names(data$variables)
    } else {
      exclude <- c(
        all.vars(data$call$id),
        all.vars(data$call$probs),
        all.vars(data$call$strata),
        all.vars(data$call$fpc),
        all.vars(data$call$weights)
      )
      include <- setdiff(names(data$variables), exclude)
    }
  }

  # setting defaults from gtsummary theme --------------------------------------
  label <- label %||%
    get_theme_element("tbl_svysummary-arg:label") %||%
    get_theme_element("tbl_summary-arg:label")
  statistic <- statistic %||%
    get_theme_element("tbl_svysummary-arg:statistic") %||%
    get_theme_element("tbl_summary-arg:statistic")
  digits <- digits %||%
    get_theme_element("tbl_svysummary-arg:digits") %||%
    get_theme_element("tbl_summary-arg:digits")
  type <- type %||%
    get_theme_element("tbl_svysummary-arg:type") %||%
    get_theme_element("tbl_summary-arg:type")
  value <- value %||%
    get_theme_element("tbl_svysummary-arg:value") %||%
    get_theme_element("tbl_summary-arg:value")
  missing <- missing %||%
    get_theme_element("tbl_svysummary-arg:missing") %||%
    get_theme_element("tbl_summary-arg:missing", default = "ifany")
  missing_text <- missing_text %||%
    get_theme_element("tbl_svysummary-arg:missing_text") %||%
    get_theme_element("tbl_summary-arg:missing_text",
                      default = translate_text("Unknown"))
  sort <- sort %||%
    get_theme_element("tbl_svysummary-arg:sort") %||%
    get_theme_element("tbl_summary-arg:sort")
  percent <- percent %||%
    get_theme_element("tbl_svysummary-arg:percent") %||%
    get_theme_element("tbl_summary-arg:percent", default = "column")

  # converting bare arguments to string ----------------------------------------
  by <- var_input_to_string(data = data$variables, select_input = !!rlang::enquo(by),
                            arg_name = "by", select_single = TRUE)

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing, choices = c("ifany", "always", "no"))
  percent <- match.arg(percent, choices = c("column", "row", "cell"))

  # checking input data --------------------------------------------------------
  tbl_summary_data_checks(data$variables)

  # removing ordered class from factor by variables ----------------------------
  if (!is.null(by) && inherits(data$variables[[by]], "ordered") && inherits(data$variables[[by]], "factor")) {
    data$variables[[by]] <- factor(data$variables[[by]], ordered = FALSE)
  }

  # deleting obs with missing by values ----------------------------------------
  # saving variable labels
  if (!is.null(by) && sum(is.na(data$variables[[by]])) > 0) {
    message(glue(
      "{sum(is.na(data$variables[[by]]))} observations missing `{by}` have been removed. ",
      "To include these observations, use `forcats::fct_explicit_na()` on `{by}` ",
      "column before passing to `tbl_svysummary()`."
    ))
    lbls <- purrr::map(data$variables, ~ attr(.x, "label"))
    data <- data[!is.na(data$variables[[by]]), ]

    # re-applying labels---I think this will NOT be necessary after dplyr 0.9.0
    for (i in names(lbls)) {
      attr(data$variables[[i]], "label") <- lbls[[i]]
    }

    rm(lbls, i)
  }

  # will return call, and all object passed to in tbl_summary call -------------
  # the object func_inputs is a list of every object passed to the function
  tbl_summary_inputs <- as.list(environment())

  # removing variables with unsupported variable types from data ---------------
  classes_expected <- c("character", "factor", "numeric", "logical", "integer", "difftime")
  data$variables <- removing_variables_with_unsupported_types(data$variables, include, classes_expected)

  # checking function inputs ---------------------------------------------------
  tbl_summary_input_checks(
    data$variables, by, label, type, value, statistic,
    digits, missing, missing_text, sort
  )

  # generate meta_data --------------------------------------------------------
  meta_data <- generate_metadata(data$variables, value, by, classes_expected, type, label, statistic, digits, percent, sort, survey = data)

  # calculating summary statistics ---------------------------------------------
  table_body <-
    meta_data %>%
    mutate(
      tbl_stats = pmap(
        list(.data$summary_type, .data$variable,
             .data$var_label, .data$stat_display, .data$df_stats),
        function(summary_type, variable, var_label, stat_display, df_stats) {
          df_stats_to_tbl(
            data = data, variable = variable, summary_type = summary_type, by = by,
            var_label = var_label, stat_display = stat_display,
            df_stats = df_stats, missing = missing, missing_text = missing_text
          )
        }
      )
    ) %>%
    pull(.data$tbl_stats) %>%
    purrr::reduce(bind_rows)

  # table of column headers ----------------------------------------------------
  table_header <-
    tibble(column = names(table_body)) %>%
    table_header_fill_missing() %>%
    mutate(
      # adding footnote of statistics on display (unless theme indicates a no print)
      footnote = ifelse(
        startsWith(.data$column, "stat_"),
        footnote_stat_label(meta_data),
        .data$footnote
      )
    )

  # returning all results in a list --------------------------------------------
  results <- list(
    table_body = table_body,
    table_header = table_header,
    meta_data = meta_data,
    inputs = tbl_summary_inputs,
    N = round(sum(weights(data))),
    call_list = list(tbl_summary = match.call())
  )
  results$by <- by
  results$df_by <- df_by(data, by)

  # assigning a class of tbl_summary (for special printing in Rmarkdown)
  class(results) <- c("tbl_svysummary", "gtsummary")

  # adding headers
  if (is.null(by)) {
    results <- modify_header_internal(
      results,
      stat_0 = "**N = {N}**",
      label = paste0("**", translate_text("Characteristic"), "**")
    )
  } else {
    results <- modify_header_internal(
      results,
      stat_by = "**{level}**, N = {n}",
      label = paste0("**", translate_text("Characteristic"), "**")
    )
  }

  return(results)
}

#' Test if data is a survey object
#' @noRd
is_survey <- function(data) {
  return(inherits(data, "survey.design") | inherits(data, "svyrep.design"))
}

# summarize_categorical for survey design --------------------------------------------------------
#' @importFrom survey svytable
summarize_categorical_survey <- function(data, variable, by, class, dichotomous_value, sort, percent) {
  df_stats <- summarize_categorical(data$variables, variable, by, class, dichotomous_value, sort, percent) %>%
    rename(n_unweighted = .data$n, N_unweighted = .data$N, p_unweighted = .data$p)

  if (is.null(by)) {
    svy_table <- survey::svytable(stats::as.formula(paste("~", variable)), data) %>%
      as_tibble() %>%
      set_names("variable_levels", "n")
  } else {
    svy_table <- survey::svytable(stats::as.formula(paste("~", by, "+", variable)), data) %>%
      as_tibble() %>%
      set_names("by", "variable_levels", "n")
  }

  svy_table <- svy_table %>%
    mutate(
      variable = as.character(variable),
    )

  # calculating percent
  group_by_percent <- switch(
    percent,
    "cell" = "",
    "column" = ifelse(!is.null(by), "by", ""),
    "row" = "variable_levels"
  )

  svy_table <- svy_table %>%
    group_by(!!!syms(group_by_percent)) %>%
    mutate(
      N = sum(.data$n),
      # if the Big N is 0, there is no denom so making percent NA
      p = ifelse(.data$N == 0, NA, .data$n / .data$N)
    ) %>%
    ungroup()

  if (!is.null(dichotomous_value)) {
    svy_table <- svy_table %>%
      filter(.data$variable_levels == !!dichotomous_value) %>%
      select(-.data$variable_levels)
  }

  suppressMessages(
    df_stats <- df_stats %>%
      left_join(svy_table)
  )

  attr(df_stats$p, "fmt_fun") <- attr(df_stats$p_unweighted, "fmt_fun")
  attr(df_stats$N, "fmt_fun") <- attr(df_stats$N_unweighted, "fmt_fun")
  attr(df_stats$n, "fmt_fun") <- attr(df_stats$n_unweighted, "fmt_fun")

  # returning final object
  df_stats
}

# summarize_continuous for survey designs ---------------------------------------------------------
summarize_continuous_survey <- function(data, variable, by, stat_display, digits) {
  # extracting function calls
  fns_names_chr <- extracting_function_calls_from_stat_display(stat_display, variable)

  # preparing df_stats
  if (is.null(by)) {
    df_stats <- tibble(variable = variable)
  } else {
    df_stats <- tibble(by = df_by(data, by)[["by"]], variable = variable)
  }

  # calculating stats for each var and by level
  for (f in fns_names_chr) {
    suppressMessages(
      df_stats <- df_stats %>%
        left_join(compute_survey_stat(data, variable, by, f))
    )
  }

  # adding formatting function as attr to summary statistics columns
  df_stats <- adding_formatting_as_attr(df_stats, digits, fns_names_chr)

  # returning final object
  df_stats
}

#' @importFrom survey svymean svyvar svyquantile svyby
compute_survey_stat <- function(data, variable, by, f) {
  args <- list(
    design = data,
    na.rm = TRUE,
    keep.var = FALSE
  )
  fun <- NULL
  if (f == "mean") {
    fun <- survey::svymean
  }
  if (f %in% c("var", "sd")) {
    fun <- survey::svyvar
  }
  if (f == "median") {
    fun <- survey::svyquantile
    args$quantiles <- .5
  }
  if (f == "min") {
    fun <- survey::svyquantile
    args$quantiles <- 0
  }
  if (f == "max") {
    fun <- survey::svyquantile
    args$quantiles <- 1
  }
  if (f %in% paste0("p", 0:100)) {
    fun <- survey::svyquantile
    args$quantiles <- as.numeric(stringr::str_replace(f, pattern = "^p", "")) / 100
  }

  if (is.null(fun))
    stop(paste0("'", f, "' statistic is not supported for survey objects."), call. = FALSE)

  if (is.null(by)) {
    args$x <- as.formula(paste("~", variable))
    stat <- do.call(fun, args)
    stat <- tibble(variable, stat[1]) %>%
      set_names(c("variable", f))
  } else {
    args$formula <- as.formula(paste("~", variable))
    args$by <- as.formula(paste("~", by))
    args$FUN <- fun
    stat <- do.call(survey::svyby, args)
    stat <- stat %>%
      as_tibble() %>%
      select(1:2) %>%
      set_names(c("by", f)) %>%
      mutate(variable = variable)
  }

  if (f == "sd")
    stat$sd <- sqrt(stat$sd)

  stat
}

# df_stats_fun_survey -----------------------------------------------------------
# this function creates df_stats in the tbl_svysummary meta data table
# and includes the number of missing values
df_stats_fun_survey <- function(summary_type, variable, class, dichotomous_value, sort,
                         stat_display, digits, data, by, percent) {
  # first table are the standard stats
  t1 <- switch(
    summary_type,
    "continuous" = summarize_continuous_survey(data = data, variable = variable,
                                               by = by, stat_display = stat_display,
                                               digits = digits),
    "categorical" = summarize_categorical_survey(data = data, variable = variable,
                                                 by = by, class = class,
                                                 dichotomous_value = dichotomous_value,
                                                 sort = sort, percent = percent),
    "dichotomous" = summarize_categorical_survey(data = data, variable = variable,
                                                 by = by, class = class,
                                                 dichotomous_value = dichotomous_value,
                                                 sort = sort, percent = percent)
  )

  # adding the N_obs and N_missing, etc
  data_is_na <- data
  # for svytable, we need to be sure that the factor has two levels
  data_is_na$variables <- mutate_at(data$variables, vars(all_of(variable)), ~ factor(is.na(.), c(F, T)))

  t2 <- summarize_categorical_survey(data = data_is_na,
                                     variable = variable,
                                     by = by, class = "logical",
                                     dichotomous_value = TRUE,
                                     sort = "alphanumeric", percent = "column") %>%
    rename(p_miss = .data$p,
           N_obs = .data$N,
           N_miss = .data$n,
           p_miss_unweighted = .data$p_unweighted,
           N_obs_unweighted = .data$N_unweighted,
           N_miss_unweighted = .data$n_unweighted) %>%
    mutate(N_nonmiss = .data$N_obs - .data$N_miss,
           p_nonmiss = 1 - .data$p_miss,
           N_nonmiss_unweighted = .data$N_obs_unweighted - .data$N_miss_unweighted,
           p_nonmiss_unweighted = 1 - .data$p_miss_unweighted)

  # returning table will all stats
  merge_vars <- switch(!is.null(by), c("by", "variable")) %||% "variable"
  return <- left_join(t1, t2, by = merge_vars)

  # setting fmt_fun for percents and integers
  attr(return$p_nonmiss, "fmt_fun") <- attr(return$p_miss, "fmt_fun")
  attr(return$N_nonmiss, "fmt_fun") <- attr(return$N_miss, "fmt_fun")
  attr(return$p_nonmiss_unweighted, "fmt_fun") <- attr(return$p_miss_unweighted, "fmt_fun")
  attr(return$N_nonmiss_unweighted, "fmt_fun") <- attr(return$N_miss_unweighted, "fmt_fun")

  return
}

# calculate_missing_row for survey objects ------------------------------------------------
calculate_missing_row_survey <- function(data, variable, by, missing_text) {
  # converting variable to TRUE/FALSE for missing
  data$variables <-
    data$variables %>%
    select(c(variable, by)) %>%
    mutate(
      !!variable := is.na(.data[[variable]])
    )

  # passing the T/F variable through the functions to format as we do in
  # the tbl_summary output
  summarize_categorical_survey(
    data = data, variable = variable, by = by, class = "logical",
    dichotomous_value = TRUE, sort = "alphanumeric", percent = "column"
  ) %>%
    {df_stats_to_tbl(
      data = data, variable = variable, summary_type = "dichotomous", by = by,
      var_label = missing_text, stat_display = "{n}", df_stats = .,
      missing = "no", missing_text = "Doesn't Matter -- Text should never appear")} %>%
    # changing row_type to missing
    mutate(row_type = "missing")
}
