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
#'   \item `{p}` percentage
#'   \item `{p.std.error}` standard error of the sample proportion computed with [survey::svymean()]
#'   \item `{deff}` design effect of the sample proportion computed with [survey::svymean()]
#'   \item `{n_unweighted}` unweighted frequency
#'   \item `{N_unweighted}` unweighted denominator
#'   \item `{p_unweighted}` unweighted formatted percentage
#' }
#' For continuous variables the following statistics are available to display.
#' \itemize{
#'   \item `{median}` median
#'   \item `{mean}` mean
#'   \item `{mean.std.error}` standard error of the sample mean computed with [survey::svymean()]
#'   \item `{deff}` design effect of the sample mean computed with [survey::svymean()]
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
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_svysummary_ex1.png", width = "31")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_svysummary_ex2.png", width = "36")`
#' }}
tbl_svysummary <- function(data, by = NULL, label = NULL, statistic = NULL,
                           digits = NULL, type = NULL, value = NULL,
                           missing = NULL, missing_text = NULL, sort = NULL,
                           percent = NULL, include = everything()) {
  # checking for survey package ------------------------------------------------
  assert_package("survey", "tbl_svysummary()")

  # test if data is a survey object
  if (!is_survey(data)) {
    stop("'data' should be a survey object (see svydesign()).", call. = FALSE)
  }

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
    paste(
      "Cannot summarize a column called 'n'. Rename it or remove",
      "it from the summary with `include = -n`"
    ) %>%
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
      "To include these observations, use `forcats::fct_na_value_to_level()` on `{by}` ",
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

  # checking function inputs ---------------------------------------------------
  tbl_summary_input_checks(data$variables, by, missing_text, include)

  # removing variables not selected for summary --------------------------------
  data$variables <- select(data$variables, !!include)

  # generate meta_data ---------------------------------------------------------
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
    select(var_type = "summary_type", "var_label", "tbl_stats") %>%
    unnest("tbl_stats") %>%
    select("variable", "var_type", "var_label", everything())

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
      pluck(x, "meta_data", "df_stats", 1, "N_obs_unweighted", 1)

    x$table_styling$header <-
      x$table_styling$header %>%
      mutate(
        modify_stat_n = .data$modify_stat_N,
        modify_stat_p = .data$modify_stat_n / .data$modify_stat_N,
        modify_stat_n_unweighted = .data$modify_stat_N_unweighted,
        modify_stat_p_unweighted = .data$modify_stat_n_unweighted / .data$modify_stat_N_unweighted,
        modify_stat_level = ifelse(.data$column %in% "stat_0", translate_text("Overall"), NA_character_)
      )
  } else {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::left_join(
        x$df_by %>%
          select(
            column = "by_col",
            modify_stat_n = "n",
            modify_stat_N = "N",
            modify_stat_p = "p",
            modify_stat_n_unweighted = "n_unweighted",
            modify_stat_N_unweighted = "N_unweighted",
            modify_stat_p_unweighted = "p_unweighted",
            modify_stat_level = "by_chr"
          ),
        by = "column"
      ) %>%
      tidyr::fill("modify_stat_N", "modify_stat_N_unweighted", .direction = "updown")
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
        ifelse(
          is.null(by),
          get_theme_element("tbl_svysummary-str:header-noby",
                            default = "**N = {style_number(N)}**"
          ),
          get_theme_element("tbl_svysummary-str:header-withby",
                            default = "**{level}**, N = {style_number(n)}"
          )
        )
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
    rename(n_unweighted = "n", N_unweighted = "N", p_unweighted = "p")

  # convert to factor if not already a factor
  if (!is.factor(data$variables[[variable]])) {
    data$variables[[variable]] <- as.factor(data$variables[[variable]])
  }
  # if there is a dichotomous value, it needs to be present as a level of the variable for svytable
  if (!is.null(dichotomous_value) && !dichotomous_value %in% levels(data$variables[[variable]])) {
    data$variables[[variable]] <- as.factor(data$variables[[variable]])
    levels(data$variables[[variable]]) <- c(levels(data$variables[[variable]]), dichotomous_value)
  }
  # if no level (e.g. when only NA), convert to binary variable
  if (length(levels(data$variables[[variable]])) == 0) {
    levels(data$variables[[variable]]) <- c(TRUE, FALSE)
    dichotomous_value <- TRUE
  }
  # if one level, it will produce an error with svymean
  # need to add a second level
  level_to_be_removed <- NULL
  if (length(levels(data$variables[[variable]])) == 1) {
    l <- levels(data$variables[[variable]])
    levels(data$variables[[variable]]) <- c(l, paste0("not_", l))
    level_to_be_removed <- paste0("not_", l)
  }

  if (!is.null(by) && is.character(data$variables[[by]])) {
    data$variables[[by]] <- as.factor(data$variables[[by]])
  }

  if (is.null(by)) {
    if (percent %in% c("column", "cell")) {
      svy_p <- survey::svymean(c_form(right = variable), data, na.rm = TRUE, deff = TRUE) %>%
        as_tibble(rownames = "var_level") %>%
        mutate(
          variable_levels = str_sub(.data$var_level, stringr::str_length(variable) + 1)
        ) %>%
        select(p = "mean", p.std.error = "SE", "deff", "variable_levels")
    } else {
      # this will have p=1 for all and p.std.error=0 for all
      svy_p <- tibble(
        variable_levels = levels(data$variables[[variable]]),
        p = 1,
        p.std.error = 0,
        deff = NaN
      )
    }
    svy_table <-
      survey::svytable(c_form(right = variable), data) %>%
      as_tibble() %>%
      set_names("variable_levels", "n") %>%
      left_join(svy_p, by = c("variable_levels"))
  } else {
    if (percent == "column") {
      svy_p <- survey::svyby(c_form(right = variable), c_form(right = by), data, survey::svymean, na.rm = TRUE, deff = TRUE) %>%
        as_tibble() %>%
        tidyr::pivot_longer(!one_of(by)) %>%
        mutate(
          stat = case_when(
            str_starts(.data$name, paste0("se.", variable)) | str_starts(.data$name, paste0("se.`", variable, "`")) ~ "p.std.error",
            str_starts(.data$name, paste0("DEff.", variable)) | str_starts(.data$name, paste0("DEff.`", variable, "`")) ~ "deff",
            TRUE ~ "p"
          ),
          name = stringr::str_remove_all(.data$name, "se\\.") %>%
            stringr::str_remove_all("DEff\\.") %>%
            str_remove_all(variable) %>%
            str_remove_all("`")
        ) %>%
        tidyr::pivot_wider(names_from = "stat", values_from = "value") %>%
        set_names(c("by", "variable_levels", "p", "p.std.error", "deff"))
    } else if (percent == "row") {
      svy_p <- survey::svyby(c_form(right = by), c_form(right = variable), data, survey::svymean, na.rm = TRUE, deff = TRUE) %>%
        as_tibble() %>%
        tidyr::pivot_longer(!one_of(variable)) %>%
        mutate(
          stat = case_when(
            str_starts(.data$name, paste0("se.", by)) | str_starts(.data$name, paste0("se.`", by, "`")) ~ "p.std.error",
            str_starts(.data$name, paste0("DEff.", by)) | str_starts(.data$name, paste0("DEff.`", by, "`")) ~ "deff",
            TRUE ~ "p"
          ),
          name = stringr::str_remove_all(.data$name, "se\\.") %>%
            stringr::str_remove_all("DEff\\.") %>%
            str_remove_all(by) %>%
            str_remove_all("`")
        ) %>%
        tidyr::pivot_wider(names_from = "stat", values_from = "value") %>%
        set_names(c("variable_levels", "by", "p", "p.std.error", "deff"))
    } else if (percent == "cell") {
      inttemp <- expand.grid(
        by = levels(data$variables[[by]]),
        variable_levels = levels(data$variables[[variable]])
      ) %>%
        mutate(
          var_level = paste0("interaction(", .env$by, ", ", variable, ")", .data$by, ".", .data$variable_levels)
        )

      svy_p <- survey::svymean(c_inter(by, variable), data, na.rm = TRUE, deff = TRUE) %>%
        as_tibble(rownames = "var_level") %>%
        dplyr::left_join(inttemp, by = "var_level") %>%
        select(p = "mean", p.std.error = "SE", "by", "deff", "variable_levels")
    }

    svy_table <-
      survey::svytable(c_form(right = c(by, variable)), data) %>%
      as_tibble() %>%
      set_names("by", "variable_levels", "n") %>%
      left_join(svy_p, by = c("by", "variable_levels"))
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
      p = if_else(.data$N == 0, NA_real_, .data$p), # re-introducing NA where relevant
      p.std.error = if_else(.data$N == 0, NA_real_, .data$p.std.error),
      deff = if_else(.data$N == 0, NA_real_, .data$deff)
    ) %>%
    ungroup()

  if (!is.null(level_to_be_removed)) {
    svy_table <- svy_table %>%
      filter(.data$variable_levels != level_to_be_removed)
  }

  if (!is.null(dichotomous_value)) {
    svy_table <- svy_table %>%
      filter(.data$variable_levels == !!dichotomous_value) %>%
      select(-"variable_levels")
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
      dplyr::cross_join(
        df_stats,
        tibble(
          variable_levels = map_chr(stat_display, ~ stat_label_match(.x) %>% unlist()),
          stat_display = .env$stat_display
        )
      ) %>%
      select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())
  } else {
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
  if (f == "mean.std.error") {
    fun <- svymean.std.error
  }
  if (f == "deff") {
    fun <- svymean.deff
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
    select(-"stat_display", -"p.std.error", -"deff") %>%
    rename(
      p_miss = "p",
      N_obs = "N",
      N_miss = "n",
      p_miss_unweighted = "p_unweighted",
      N_obs_unweighted = "N_unweighted",
      N_miss_unweighted = "n_unweighted"
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
      rename(col_name = "by_col")
  } else if ("variable_levels" %in% names(return)) {
    return$label <- as.character(return$variable_levels)
    return$col_name <- "stat_0"
  } else {
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

c_inter <- function(f1, f2) {
  stats::as.formula(paste0("~interaction(", f1, ",", f2, ")"))
}

.extract_data_frame <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
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

# mean standard error
svymean.std.error <- function(x, design, na.rm = FALSE, ...) {
  survey::svymean(x = x, design = design, na.rm = na.rm, ...) %>% survey::SE()
}

# mean design effects
svymean.deff <- function(x, design, na.rm = FALSE, ...) {
  survey::svymean(x = x, design = design, na.rm = na.rm, deff = TRUE) %>%
    survey::deff()
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
