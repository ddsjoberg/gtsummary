#' Create a table of summary statistics from a survey object
#'
#' The `tbl_svysummary` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables taking into account survey weights and design.
#' It is similar to [tbl_summary()].
#'
#' @param data A survey object created with created with `survey::svydesign()`
#' @inheritParams tbl_summary
#'
#' @inheritSection tbl_summary type argument
#' @inheritSection tbl_summary select helpers
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
#'   \item `{sum}` sum
#' }
#'
#' Unlike [tbl_summary()], it is not possible to pass a custom function.
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
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @author Joseph Larmarange
#' @examplesIf broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE)
#' # A simple weighted dataset
#' tbl_svysummary_ex1 <-
#'   survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) %>%
#'   tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age))
#'
#' # Example 2 ----------------------------------
#' # A dataset with a complex design
#' data(api, package = "survey")
#' tbl_svysummary_ex2 <-
#'   survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) %>%
#'   tbl_svysummary(by = "both", include = c(api00, stype))
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_svysummary_ex1.png}{options: width=31\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_svysummary_ex2.png}{options: width=36\%}}
tbl_svysummary <- function(data, by = NULL, label = NULL, statistic = NULL,
                           digits = NULL, type = NULL, value = NULL,
                           missing = NULL, missing_text = NULL, sort = NULL,
                           percent = NULL, include = everything()) {
  # checking for survey package ------------------------------------------------
  assert_package("survey", "tbl_svysummary()")

  # test if data is a survey object
  if (!is_survey(data))
    stop("'data' should be a survey object (see svydesign()).", call. = FALSE)

  # converting bare arguments to string ----------------------------------------
  by <-
    .select_to_varnames(
      select = {{ by }},
      data = data$variables,
      arg_name = "by",
      select_single = TRUE
    )

  include <-
    .select_to_varnames(
      select = {{ include }},
      data = data$variables,
      arg_name = "include"
    ) %>%
    union(by)
  if ("n" %in% include) {
    paste("Cannot summarize a column called 'n'. Rename it or remove",
          "is from the summary with `include = -n`") %>%
      abort()
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
      default = translate_text("Unknown")
    )
  sort <- sort %||%
    get_theme_element("tbl_svysummary-arg:sort") %||%
    get_theme_element("tbl_summary-arg:sort")
  percent <- percent %||%
    get_theme_element("tbl_svysummary-arg:percent") %||%
    get_theme_element("tbl_summary-arg:percent", default = "column")

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing, choices = c("ifany", "always", "no"))
  percent <- match.arg(percent, choices = c("column", "row", "cell"))

  # checking input data --------------------------------------------------------
  tbl_summary_data_checks(data$variables)

  # if by is numeric, convert into a factor -------------------------------
  if (!is.null(by) && is.numeric(data$variables[[by]])) {
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
  tbl_summary_inputs$exclude <- NULL # should not be exported

  # checking function inputs ---------------------------------------------------
  tbl_summary_input_checks(
    data$variables, by, label, type, value, statistic,
    digits, missing, missing_text, sort
  )

  # removing variables not selected for summary --------------------------------
  data$variables <- select(data$variables, !!include)

  # generate meta_data --------------------------------------------------------
  meta_data <- generate_metadata(data$variables, value, by, type, label, statistic, digits, percent, sort, survey = data)

  # calculating summary statistics ---------------------------------------------
  table_body <-
    meta_data %>%
    mutate(
      tbl_stats = pmap(
        list(
          .data$summary_type, .data$variable,
          .data$var_label, .data$stat_display, .data$df_stats
        ),
        function(summary_type, variable, var_label, stat_display, df_stats) {
          df_stats_to_tbl(
            data = data, variable = variable, summary_type = summary_type, by = by,
            var_label = var_label, stat_display = stat_display,
            df_stats = df_stats, missing = missing, missing_text = missing_text
          )
        }
      )
    ) %>%
    select(var_type = .data$summary_type, .data$var_label, .data$tbl_stats) %>%
    unnest(.data$tbl_stats) %>%
    select(.data$variable, .data$var_type, .data$var_label, everything())

  # table of column headers ----------------------------------------------------
  x <-
    .create_gtsummary_object(
      table_body = table_body,
      meta_data = meta_data,
      inputs = tbl_summary_inputs,
      N = nrow(data),
      call_list = list(tbl_summary = match.call()),
      by = by,
      df_by = df_by(data, by)
    )

  # adding "modify_stat_" information ------------------------------------------
  if (is.null(by)) {
    x$table_styling$header$modify_stat_N <-
      pluck(x, "meta_data", "df_stats", 1, "N_obs", 1)
    x$table_styling$header$modify_stat_N_unweighted <-
      pluck(x, "meta_data", "df_stats", 1, "N_unweighted", 1)
  }
  else {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::left_join(
        x$df_by %>%
          select(column = .data$by_col,
                 modify_stat_n = .data$n,
                 modify_stat_N = .data$N,
                 modify_stat_p = .data$p,
                 modify_stat_n_unweighted = .data$n_unweighted,
                 modify_stat_N_unweighted = .data$N_unweighted,
                 modify_stat_p_unweighted = .data$p_unweighted,
                 modify_stat_level = .data$by_chr),
        by = "column"
      ) %>%
      tidyr::fill(c(.data$modify_stat_N, .data$modify_stat_N_unweighted), .direction = "updown")
  }

  # adding headers and footnote ------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = all_stat_cols(),
      footnote = footnote_stat_label(meta_data)
    ) %>%
    modify_header(
      label = paste0("**", translate_text("Characteristic"), "**"),
      all_stat_cols() ~
        ifelse(is.null(by),
               "**N = {style_number(N)}**",
               "**{level}**, N = {style_number(n)}")
    )




  # assign class and return final tbl ------------------------------------------
  class(x) <- c("tbl_svysummary", class(x))

  # running any additional mods
  x <-
    get_theme_element("tbl_svysummary-fn:addnl-fn-to-run", default = identity) %>%
    do.call(list(x))

  # returning tbl_svysummary table
  x
}

#' Test if data is a survey object
#' @noRd
is_survey <- function(data) {
  return(inherits(data, "survey.design") | inherits(data, "svyrep.design"))
}

# summarize_categorical for survey design --------------------------------------
summarize_categorical_survey <- function(data, variable, by,
                                         dichotomous_value, sort, percent, stat_display) {
  df_stats <-
    summarize_categorical(
      data = data$variables, variable = variable, by = by,
      dichotomous_value = dichotomous_value,
      sort = sort, percent = percent, stat_display = stat_display
    ) %>%
    rename(n_unweighted = .data$n, N_unweighted = .data$N, p_unweighted = .data$p)

  # if there is a dichotomous value, it needs to be present as a level of the variable for svytable
  if (!is.null(dichotomous_value) && !dichotomous_value %in% unique(data$variables[[variable]])) {
    data$variables[[variable]] <- as.factor(data$variables[[variable]])
    levels(data$variables[[variable]]) <- c(levels(data$variables[[variable]]), dichotomous_value)
  }

  if (is.null(by)) {
    svy_table <-
      survey::svytable(c_form(right = variable), data) %>%
      as_tibble() %>%
      set_names("variable_levels", "n")
  } else {
    svy_table <-
      survey::svytable(c_form(right = c(by, variable)), data) %>%
      as_tibble() %>%
      set_names("by", "variable_levels", "n")
  }

  svy_table <- svy_table %>%
    mutate(
      variable = as.character(variable),
    )

  # calculating percent
  group_by_percent <- switch(percent,
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

  df_stats <-
    df_stats %>%
    mutate(stat_display = .env$stat_display) %>%
    select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())

  # returning final object
  df_stats
}

# summarize_continuous for survey designs ---------------------------------------------------------
summarize_continuous_survey <- function(data, variable, by, stat_display,
                                        digits, summary_type) {
  # extracting function calls
  fns_names_chr <-
    extracting_function_calls_from_stat_display(stat_display, variable) %>%
    # removing stats that are calculated later
    setdiff(c(
      "p_miss", "N_obs", "N_miss", "p_miss_unweighted", "N_obs_unweighted",
      "N_miss_unweighted", "N_nonmiss", "p_nonmiss", "N_nonmiss_unweighted",
      "p_nonmiss_unweighted"
    ))


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

  # adding stat_display to the data frame
  if (summary_type == "continuous2") {
    return <-
      left_join(
        df_stats,
        tibble(
          variable_levels = map_chr(stat_display, ~ stat_label_match(.x) %>% unlist()),
          stat_display = .env$stat_display
        ),
        by = character()
      ) %>%
      select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())
  }
  else {
    return <-
      df_stats %>%
      mutate(stat_display = .env$stat_display) %>%
      select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())
  }

  # returning final object
  return
}

compute_survey_stat <- function(data, variable, by, f) {
  # difftime variable needs to be transformed into numeric for svyquantile
  if (inherits(data$variables[[variable]], "difftime")) {
    data$variables[[variable]] <- unclass(data$variables[[variable]])
  }

  args <- list(
    design = data,
    na.rm = TRUE,
    keep.var = FALSE
  )

  # if all values are NA, turn na.rm to FALSE to avoid error
  if (all(is.na(data$variables[[variable]]))) {
    args$na.rm <- FALSE
  }

  fun <- NULL
  if (f == "mean") {
    fun <- survey::svymean
  }
  if (f == "sum") {
    fun <- survey::svytotal
  }
  if (f %in% c("var", "sd")) {
    fun <- survey::svyvar
  }
  if (f == "median") {
    fun <- svyquantile_version
    args$quantiles <- .5
  }
  if (f == "min") {
    fun <- svymin
  }
  if (f == "max") {
    fun <- svymax
  }
  if (f %in% paste0("p", 0:100)) {
    fun <- svyquantile_version
    args$quantiles <- as.numeric(stringr::str_replace(f, pattern = "^p", "")) / 100
  }

  if (is.null(fun)) {
    stop(paste0("'", f, "' statistic is not supported for survey objects."), call. = FALSE)
  }

  if (is.null(by)) {
    args$x <- c_form(right = variable)
    stat <- do.call(fun, args)
    stat <- tibble(variable, stat[1]) %>%
      set_names(c("variable", f))
  } else {
    args$formula <- c_form(right = variable)
    args$by <- c_form(right = by)
    args$FUN <- fun
    stat <- do.call(survey::svyby, args)
    stat <- stat %>%
      as_tibble() %>%
      select(1:2) %>%
      set_names(c("by", f)) %>%
      mutate(variable = variable)
  }

  if (f == "sd") {
    stat$sd <- sqrt(stat$sd)
  }

  stat
}

# df_stats_fun_survey -----------------------------------------------------------
# this function creates df_stats in the tbl_svysummary meta data table
# and includes the number of missing values
df_stats_fun_survey <- function(summary_type, variable, dichotomous_value, sort,
                                stat_display, digits, data, by, percent, var_label) {
  # first table are the standard stats
  t1 <- switch(summary_type,
    "continuous" = summarize_continuous_survey(
      data = data, variable = variable,
      by = by, stat_display = stat_display,
      digits = digits, summary_type = summary_type
    ),
    "continuous2" = summarize_continuous_survey(
      data = data, variable = variable,
      by = by, stat_display = stat_display,
      digits = digits, summary_type = summary_type
    ),
    "categorical" = summarize_categorical_survey(
      data = data, variable = variable,
      by = by,
      dichotomous_value = dichotomous_value,
      sort = sort, percent = percent,
      stat_display = stat_display
    ),
    "dichotomous" = summarize_categorical_survey(
      data = data, variable = variable,
      by = by,
      dichotomous_value = dichotomous_value,
      sort = sort, percent = percent,
      stat_display = stat_display
    )
  )

  # adding the N_obs and N_missing, etc
  data_is_na <- data
  # for svytable, we need to be sure that the factor has two levels
  data_is_na$variables <- mutate_at(data$variables, vars(all_of(variable)), ~ factor(is.na(.), c(F, T)))

  t2 <- summarize_categorical_survey(
    data = data_is_na,
    variable = variable,
    by = by,
    dichotomous_value = TRUE,
    sort = "alphanumeric", percent = "column",
    stat_display = "{n}"
  ) %>%
    select(-.data$stat_display) %>%
    rename(
      p_miss = .data$p,
      N_obs = .data$N,
      N_miss = .data$n,
      p_miss_unweighted = .data$p_unweighted,
      N_obs_unweighted = .data$N_unweighted,
      N_miss_unweighted = .data$n_unweighted
    ) %>%
    mutate(
      N_nonmiss = .data$N_obs - .data$N_miss,
      p_nonmiss = 1 - .data$p_miss,
      N_nonmiss_unweighted = .data$N_obs_unweighted - .data$N_miss_unweighted,
      p_nonmiss_unweighted = 1 - .data$p_miss_unweighted
    )

  # returning table will all stats
  merge_vars <- switch(!is.null(by),
    c("by", "variable")
  ) %||% "variable"
  return <- left_join(t1, t2, by = merge_vars)

  # adding variables needed for inlin_text()
  if ("by" %in% names(return)) {
    return$label <- return$by
    return <-
      return %>%
      left_join(df_by(data, by)[c("by", "by_col")], by = "by") %>%
      rename(col_name = .data$by_col)
  }
  else if ("variable_levels" %in% names(return)) {
    return$label <- as.character(return$variable_levels)
    return$col_name <- "stat_0"
  }
  else {
    return$label <- var_label
    return$col_name <- "stat_0"
  }

  return <- adding_formatting_as_attr(
    df_stats = return, data = data, variable = variable,
    summary_type = summary_type, stat_display = stat_display,
    digits = digits
  )

  return
}


# helper for generating formulas for survey tests -------------------------------------
c_form <- function(left = NULL, right = 1) {
  # quoting to take into account complex names
  if (!is.null(left)) left <- paste0("`", left, "`")
  right <- paste0("`", right, "`")
  left <- paste(left, collapse = "+")
  right <- paste(right, collapse = "+")
  stats::as.formula(paste(left, "~", right))
}

.extract_data_frame <- function(x) {
  if (is.data.frame(x)) return(x)
  x$variables # return survey object data frame
}

# Min and Max Values for survey design
svymin <- function(x, design, na.rm = FALSE, ...) {
  x <- all.vars(x)
  min(design$variables[[x]], na.rm = na.rm)
}

svymax <- function(x, design, na.rm = FALSE, ...) {
  x <- all.vars(x)
  max(design$variables[[x]], na.rm = na.rm)
}

# function chooses which quantile function to sue based on the survey pkg version
svyquantile_version <- function(...) {
  fn <-
    ifelse(
      utils::packageVersion("survey") >= "4.1",
      "survey::oldsvyquantile",
      "survey::svyquantile"
    ) %>%
    rlang::parse_expr() %>%
    eval()

  fn(...)
}
