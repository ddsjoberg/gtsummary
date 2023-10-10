#' Add CI Column
#'
#' Add a new column with the confidence intervals for proportions, means, etc.
#'
#' @param x A `tbl_summary` or a `tbl_svysummary` object
#' @param statistic Formula indicating how the confidence interval will be displayed.
#' Default is `list(all_categorical() ~ "{conf.low}%, {conf.high}%", all_continuous() ~ "{conf.low}, {conf.high}")`
#' @param method Confidence interval method. Default is
#' `list(all_categorical() ~ "wilson", all_continuous() ~ "t.test")` for `tbl_summary` objects
#' and `list(all_categorical() ~ "svyprop", all_continuous() ~ "svymean")` for `tbl_svysummary`
#' objects.
#' See details below.
#' @param conf.level Confidence level. Default is `0.95`
#' @param style_fun Function to style upper and lower bound of confidence
#' interval. Default is
#' `list(all_categorical() ~ purrr::partial(style_sigfig, scale =  100), all_continuous() ~ style_sigfig)`.
#' @param pattern string indicating the pattern to use to merge the CI with
#' the statistics cell. The default is NULL, where no columns are merged.
#' The two columns that will be merged are the statistics column,
#' represented by `"{stat}"` and the CI column represented by `"{ci}"`,
#' e.g. `pattern = "{stat} ({ci})"` will merge the two columns with the CI
#' in parentheses.
#' @param df For `tbl_svysummary()`, the number of degrees of freedom used
#' to estimate confidence intervals. By default, will use `survey::degf()`.
#' @param ... Not used
#' @inheritParams tbl_summary
#'
#' @section method argument:
#'
#' **for `tbl_summary` tables**
#'
#' Must be one of
#' `c("wilson", "wilson.no.correct", "exact", "asymptotic")` for categorical
#' variables, and `c("t.test", "wilcox.test")` for continuous variables.
#'
#' Methods `c("wilson", "wilson.no.correct")` are calculated with
#' `prop.test(correct = c(TRUE, FALSE))`.
#' The default method, `"wilson"`, includes the Yates continuity correction.
#' Methods `c("exact", "asymptotic")` are calculated with `Hmisc::binconf(method=)`.
#'
#' Confidence intervals for means are calculated using `t.test()` and
#' `wilcox.test()` for pseudo-medians.
#'
#' **for `tbl_svysummary` tables**
#'
#' Must be one of
#' `c("svyprop", "svyprop.logit", "svyprop.likelihood", "svyprop.asin", "svyprop.beta", "svyprop.mean", "svyprop.xlogit")`
#' for categorical variables, and
#' `c("svymean", "svymedian", "svymedian.mean", "svymedian.beta", "svymedian.xlogit", "svymedian.asin", "svymedian.score")`
#' for continuous variables.
#'
#' Confidence intervals for proportions are computed with `survey::svyciprop()`.
#' See the help file of this function for details on the different methods
#' available to compute CIs. The default method `"svyprop"` is equivalent
#' to `"svyprop.logit"`, corresponding to a call to `survey::svyciprop()` with
#' `method = "logit"`.
#'
#' Confidence intervals for means (method `"svymean"`) are computed using
#' `confint(svymean())`.
#'
#' Confidence intervals for medians are computed with `survey::svyquantile()`.
#' See the help file of this function for details on the different methods
#' available to compute CIs. The default method `"svymedian"` is equivalent
#' to `"svymedian.mean"`, corresponding to a call to `surevy::svyquantile()`
#' with `method = "mean"`.
#'
#' @return gtsummary table
#' @rdname add_ci
#' @export
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#'
#' @family tbl_summary tbl_svysummary tools
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' add_ci_ex1 <-
#'   trial %>%
#'   select(marker, response, trt) %>%
#'   tbl_summary(
#'     missing = "no",
#'     statistic = all_continuous() ~ "{mean} ({sd})"
#'   ) %>%
#'   add_ci()
#'
#' # Example 2 ----------------------------------
#' add_ci_ex2 <-
#'   trial %>%
#'   select(response, grade) %>%
#'   tbl_summary(
#'     statistic = all_categorical() ~ "{p}%",
#'     missing = "no"
#'   ) %>%
#'   add_ci(pattern = "{stat} ({ci})") %>%
#'   modify_footnote(everything() ~ NA)
#'
#' # Example 3 ----------------------------------
#' data(api, package = "survey")
#' add_ci_ex3 <-
#'   survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) %>%
#'   tbl_svysummary(
#'     include = c(api00, hsg, stype),
#'     statistic = hsg ~ "{mean} ({sd})"
#'   ) %>%
#'   add_ci(
#'     method = api00 ~ "svymedian"
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_ci_ex1.png", width = "50")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_ci_ex2.png", width = "45")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_ci_ex3.png", width = "45")`
#' }}
add_ci <- function(x, ...) {
  UseMethod("add_ci")
}

#' @rdname add_ci
#' @export
add_ci.tbl_summary <- function(x,
                               method = NULL,
                               include = everything(),
                               statistic = NULL,
                               conf.level = 0.95,
                               style_fun = NULL,
                               pattern = NULL,
                               df = NULL,
                               ...) {
  rlang::check_dots_used()

  # resolving arguments --------------------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = x$table_body,
      arg_name = "include"
    )
  summary_type <-
    x$meta_data %>%
    filter(.data$variable %in% .env$include) %>%
    select("variable", "summary_type") %>%
    tibble::deframe()

  if (inherits(x, "tbl_summary")) {
    default_method <- list(
      all_categorical() ~ "wilson",
      all_continuous() ~ "t.test"
    )
  }
  if (inherits(x, "tbl_svysummary")) {
    default_method <- list(
      all_categorical() ~ "svyprop",
      all_continuous() ~ "svymean"
    )
  }

  if (inherits(x, "tbl_svysummary") && is.null(df)) {
    df <- survey::degf(x$inputs$data)
  }
  if (inherits(x, "tbl_summary") && !is.null(df)) {
    cli::cli_alert_danger("{.arg df} is not used for `tbl_summary` objects.")
  }

  method <-
    .formula_list_to_named_list(
      x = default_method,
      var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include, ]),
      arg_name = "method"
    ) %>%
    utils::modifyList(
      val =
        .formula_list_to_named_list(
          x = method,
          var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include, ]),
          arg_name = "method",
          type_check = chuck(type_check, "is_string", "fn"),
          type_check_msg = chuck(type_check, "is_string", "msg")
        ) %||%
          list()
    )

  style_fun <-
    .formula_list_to_named_list(
      x = list(
        all_categorical() ~ function(x) style_sigfig(x, scale = 100),
        all_continuous() ~ style_sigfig
      ),
      var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include, ]),
      arg_name = "style_fun"
    ) %>%
    utils::modifyList(
      val =
        .formula_list_to_named_list(
          x = style_fun,
          var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include, ]),
          arg_name = "style_fun",
          type_check = chuck(type_check, "is_function", "fn"),
          type_check_msg = chuck(type_check, "is_function", "msg")
        ) %||%
          list()
    )

  statistic <-
    .formula_list_to_named_list(
      x = list(
        all_categorical() ~ "{conf.low}%, {conf.high}%",
        all_continuous() ~ "{conf.low}, {conf.high}"
      ),
      var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include, ]),
      arg_name = "statistic"
    ) %>%
    utils::modifyList(
      val =
        .formula_list_to_named_list(
          x = statistic,
          var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include, ]),
          arg_name = "statistic",
          type_check = chuck(type_check, "is_character", "fn"),
          type_check_msg = chuck(type_check, "is_character", "msg")
        ) %||%
          list()
    )

  updated_call_list <- c(x$call_list, list(add_ci = match.call()))

  # check inputs ---------------------------------------------------------------
  if (!is.null(pattern)) {
    if (!rlang::is_string(pattern)) {
      stop("The `pattern=` argument must be a string.", call. = FALSE)
    }
    pattern_elements <-
      stringr::str_extract_all(pattern, "\\{.*?\\}") %>%
      map(stringr::str_remove_all, pattern = fixed("}")) %>%
      map(stringr::str_remove_all, pattern = fixed("{")) %>%
      unlist() %>%
      unique()
    if (!rlang::is_empty(pattern_elements %>% setdiff(c("stat", "ci")))) {
      stop("All `pattern=` elements in curly brackets must be 'stat' and 'ci'", call. = FALSE)
    }
    if (!setequal(pattern_elements, c("stat", "ci"))) {
      paste(
        "The `pattern=` argument should",
        "include reference to both '{stat}' and '{ci}'"
      ) %>%
        stop(call. = FALSE)
    }
  }

  # adding new column with CI --------------------------------------------------
  if (inherits(x, "tbl_summary")) {
    single_ci_fn <- single_ci
  }
  else if (inherits(x, "tbl_svysummary")) {
    single_ci_fn <- single_ci_svy
  }
  x <-
    x %>%
    add_stat(
      fns = all_of(include) ~ purrr::partial(single_ci_fn,
                                             method = method,
                                             conf.level = conf.level,
                                             statistic = statistic,
                                             style_fun = style_fun,
                                             summary_type = summary_type,
                                             df = df
      ),
      location = list(all_of(include) ~ "label", all_categorical(FALSE) ~ "level")
    ) %>%
    # moving the CI cols to after the original stat cols (when `by=` variable present)
    # also renaming CI columns
    modify_table_body(
      function(.x) {
        # rename ci columns
        .x <-
          .x %>%
          dplyr::rename_with(
            .fn = ~ vec_paste0(
              "ci_",
              stringr::str_replace(., pattern = "_ci$", replacement = "")
            ),
            .cols = matches("^stat_\\d+_ci$")
          )

        # reorder the columns
        stat_cols_in_order <-
          .x %>%
          select(all_stat_cols()) %>%
          names() %>%
          lapply(function(x) c(x, paste0("ci_", x))) %>%
          unlist()

        .x %>%
          dplyr::relocate(all_of(stat_cols_in_order), .after = all_of(stat_cols_in_order[1]))
      }
    )

  if (is.null(pattern)) {
    x <-
      x %>%
      # updating CI column headers and footnotes
      modify_header(matches("^ci_stat_\\d+$") ~ paste0("**", conf.level * 100, "% CI**")) %>%
      modify_footnote(
        update = matches("^ci_stat_\\d+$") ~ translate_text("CI = Confidence Interval"),
        abbreviation = TRUE
      )
  } else {
    # get the stat column index numbers, eg get the 1 and 2 from stat_1 and stat_2
    stat_column_names <-
      x$table_body %>%
      select(all_stat_cols()) %>%
      names()
    chr_index <- stringr::str_replace(stat_column_names, pattern = "^stat_", "")

    # create list of column merging expressions
    cols_merge_expr <-
      chr_index %>%
      map(
        ~ expr(modify_table_styling(
          columns = !!glue("stat_{.x}"),
          rows = !is.na(!!sym(paste0("ci_stat_", .x))),
          cols_merge_pattern =
            !!glue::glue_data(
              .x = list(stat = paste0("{stat_", .x, "}"), ci = paste0("{ci_stat_", .x, "}")),
              pattern
            )
        ))
      )

    # merge columns
    x <-
      cols_merge_expr %>%
      purrr::reduce(~ rlang::inject(!!.x %>% !!.y), .init = x) %>%
      modify_footnote(
        update = all_stat_cols() ~ translate_text("CI = Confidence Interval"),
        abbreviation = TRUE
      )

    # updating header using `pattern=` argument
    x$table_styling$header <-
      x$table_styling$header %>%
      rowwise() %>%
      mutate(
        label =
          ifelse(
            .data$column %in% stat_column_names,
            glue::glue_data(
              .x = list(stat = .data$label, ci = paste0("**", conf.level * 100, "% CI**")),
              pattern
            ),
            .data$label
          )
      ) %>%
      ungroup()
  }


  # return gtsummary table -----------------------------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list
  x
}

# function to add CI for one variable (tbl_summary)
single_ci <- function(variable, by, tbl, method, conf.level,
                      style_fun, statistic, summary_type, ...) {
  if (method[[variable]] %in% c(
    "wilson", "wilson.no.correct",
    "exact", "asymptotic"
  ) &&
    summary_type[[variable]] %in% c("categorical", "dichotomous")) {
    df_single_ci <-
      tbl$meta_data %>%
      filter(.data$variable %in% .env$variable) %>%
      purrr::pluck("df_stats", 1) %>%
      dplyr::rowwise() %>%
      mutate(
        ci =
          calculate_prop_ci(
            x = .data$n, n = .data$N,
            statistic = statistic[[variable]],
            method = method[[variable]],
            conf.level = conf.level,
            style_fun = style_fun[[variable]]
          )
      )
  } else if (method[[variable]] %in% c("t.test", "wilcox.test") &&
    summary_type[[variable]] %in% c("continuous", "continuous2")) {
    df_single_ci <-
      tbl$inputs$data %>%
      dplyr::group_by_at(tbl$by) %>%
      tidyr::nest(data = -all_of(tbl$by)) %>%
      dplyr::rowwise() %>%
      mutate(
        ci =
          calculate_mean_ci(
            data = .data$data,
            variable = variable,
            statistic = statistic[[variable]],
            method = method[[variable]],
            conf.level = conf.level,
            style_fun = style_fun[[variable]],
            tbl = tbl
          )
      )
    if (is.null(tbl$by)) {
      df_single_ci <-
        df_single_ci %>%
        mutate(col_name = "stat_0") %>%
        select(any_of(c("col_name", "variable_levels", "ci")))
    } else {
      df_single_ci <-
        df_single_ci %>%
        dplyr::rename(by = all_of(tbl$by)) %>%
        left_join(
          tbl$df_by %>% select("by", col_name = "by_col"),
          by = "by"
        ) %>%
        select(any_of(c("by", "col_name", "ci")))
    }
  } else {
    glue(
      "Error with variable '{variable}'. Method '{method[[variable]]}' ",
      "cannot be applied to summary type '{summary_type[[variable]]}'."
    ) %>%
      stop(call. = FALSE)
  }

  df_single_ci %>%
    tidyr::pivot_wider(
      id_cols = any_of("variable_levels"),
      values_from = "ci",
      names_from = "col_name"
    ) %>%
    select(all_stat_cols()) %>%
    dplyr::rename_with(.fn = ~ vec_paste0(., "_ci"))
}

calculate_mean_ci <- function(data, variable, statistic,
                              method, conf.level, style_fun, tbl) {
  if (method %in% "t.test") {
    if (!"mean" %in%
      names(tbl$meta_data[tbl$meta_data$variable %in% variable, ]$df_stats[[1]])) {
      paste(
        "{.code add_ci()} added mean CI for {.val {variable}};",
        "however, no mean is shown in the {.code tbl_summary()} table."
      ) %>%
        cli::cli_alert_danger()
    }
    df_ci <-
      stats::t.test(data[[variable]], conf.level = conf.level) %>%
      broom::tidy()
  } else if (method %in% "wilcox.test") {
    if (!"median" %in%
      names(tbl$meta_data[tbl$meta_data$variable %in% variable, ]$df_stats[[1]])) {
      paste(
        "{.code add_ci()} added pseudo-median CI for {.val {variable}};",
        "however, no median is shown in the {.code tbl_summary()} table."
      ) %>%
        cli::cli_alert_danger()
    }
    df_ci <-
      stats::wilcox.test(data[[variable]], conf.level = conf.level, conf.int = TRUE) %>%
      broom::tidy()
  }

  # round and format CI
  df_ci %>%
    select(all_of(c("conf.low", "conf.high"))) %>%
    dplyr::mutate_all(style_fun) %>%
    glue::glue_data(statistic) %>%
    as.character()
}

calculate_prop_ci <- function(x, n, statistic, method, conf.level, style_fun) {
  # calculate CI
  if (method %in% c("wilson", "wilson.no.correct")) {
    df_ci <-
      stats::prop.test(
        x = x, n = n,
        conf.level = conf.level,
        correct = isTRUE(method == "wilson")
      ) %>%
      broom::tidy()
  } else if (method %in% c("exact", "asymptotic")) {
    assert_package("Hmisc", fn = 'add_ci(method = c("exact", "asymptotic"))')
    df_ci <-
      Hmisc::binconf(
        x = x, n = n,
        method = method, alpha = 1 - conf.level
      ) %>%
      as.data.frame() %>%
      set_names(c("estimate", "conf.low", "conf.high"))
  }

  # round and format CI
  df_ci %>%
    select(all_of(c("conf.low", "conf.high"))) %>%
    dplyr::mutate_all(style_fun) %>%
    glue::glue_data(statistic) %>%
    as.character()
}

#' @rdname add_ci
#' @export
add_ci.tbl_svysummary <- add_ci.tbl_summary

# function to add CI for one variable (tbl_svysummary)
single_ci_svy <- function(variable, by, tbl, method, conf.level,
                          style_fun, statistic, summary_type, df, ...) {
  assert_package("survey", fn = "add_ci.tbl_svysummary())")

  if (method[[variable]] %in% c("svymean") &&
    summary_type[[variable]] %in% c("continuous", "continuous2")) {
    df_single_ci <-
      tbl$meta_data %>%
      filter(.data$variable %in% .env$variable) %>%
      purrr::pluck("df_stats", 1) %>%
      dplyr::rowwise() %>%
      mutate(
        ci = calculate_svymean_ci(
          variable = .data$variable,
          by = .env$by,
          level = .data[["by"]],
          tbl = .env$tbl,
          statistic = statistic[[variable]],
          conf.level = conf.level,
          style_fun = style_fun[[variable]],
          df = df
        )
      )

    if (!"mean" %in%
      names(tbl$meta_data[tbl$meta_data$variable %in% variable, ]$df_stats[[1]])) {
      paste(
        "{.code add_ci()} added mean CI for {.val {variable}};",
        "however, no mean is shown in the {.code tbl_svysummary()} table."
      ) %>%
        cli::cli_alert_danger()
    }
  } else if (stringr::str_starts(method[[variable]], "svymedian") &&
    summary_type[[variable]] %in% c("continuous", "continuous2")) {
    svymedian_method <- stringr::str_sub(method[[variable]], start = 11L)
    if (svymedian_method == "") svymedian_method <- "mean"
    df_single_ci <-
      tbl$meta_data %>%
      filter(.data$variable %in% .env$variable) %>%
      purrr::pluck("df_stats", 1) %>%
      dplyr::rowwise() %>%
      mutate(
        ci = calculate_svymedian_ci(
          variable = .data$variable,
          by = .env$by,
          level = .data[["by"]],
          tbl = .env$tbl,
          method = svymedian_method,
          statistic = statistic[[variable]],
          conf.level = conf.level,
          style_fun = style_fun[[variable]],
          df = df
        )
      )

    if (!"median" %in%
      names(tbl$meta_data[tbl$meta_data$variable %in% variable, ]$df_stats[[1]])) {
      paste(
        "{.code add_ci()} added median CI for {.val {variable}};",
        "however, no median is shown in the {.code tbl_svysummary()} table."
      ) %>%
        cli::cli_alert_danger()
    }
  } else if (stringr::str_starts(method[[variable]], "svyprop") &&
    summary_type[[variable]] %in% c("categorical", "dichotomous")) {
    svyprop_method <- stringr::str_sub(method[[variable]], start = 9L)
    if (svyprop_method == "") svyprop_method <- "logit"

    md <-
        tbl$meta_data %>%
        filter(.data$variable %in% .env$variable) %>%
        purrr::pluck("df_stats", 1)
    if (!"variable_levels" %in% names(md)) # if dichotomous
      md <- md %>%
        dplyr::mutate(variable_levels =
                        tbl$meta_data %>%
                        filter(.data$variable %in% .env$variable) %>%
                        purrr::pluck("dichotomous_value", 1)
                      )

    df_single_ci <-
      md %>%
      dplyr::rowwise() %>%
      mutate(
        ci = calculate_svyprop_ci(
          variable = .data$variable,
          variable_levels = .data$variable_levels,
          by = .env$by,
          level = .data[["by"]],
          tbl = .env$tbl,
          method = svyprop_method,
          statistic = statistic[[variable]],
          conf.level = conf.level,
          style_fun = style_fun[[variable]],
          df = df
        )
      )
  } else {
    glue(
      "Error with variable '{variable}'. Method '{method[[variable]]}' ",
      "cannot be applied to summary type '{summary_type[[variable]]}'."
    ) %>%
      stop(call. = FALSE)
  }

  df_single_ci %>%
    tidyr::pivot_wider(
      id_cols = any_of("variable_levels"),
      values_from = "ci",
      names_from = "col_name"
    ) %>%
    select(all_stat_cols()) %>%
    dplyr::rename_with(.fn = ~ paste0(., "_ci"))
}

calculate_svymean_ci <- function(variable, by, level, tbl,
                                 statistic, conf.level, style_fun, df) {
  if (is.null(by) || is.na(level)) {
    design <- tbl$inputs$data
  } else {
    design <- subset(
      tbl$inputs$data,
      tbl$inputs$data$variables[[by]] == level
    )
  }

  df_ci <-
    survey::svymean(
      c_form(right = variable),
      design = design,
      na.rm = TRUE
    ) %>%
    stats::confint(level = conf.level, df = df) %>%
    dplyr::as_tibble() %>%
    set_names(c("conf.low", "conf.high"))

  # round and format CI
  df_ci %>%
    select(all_of(c("conf.low", "conf.high"))) %>%
    dplyr::mutate_all(style_fun) %>%
    glue::glue_data(statistic) %>%
    as.character()
}

calculate_svymedian_ci <- function(variable, by, level, tbl, method,
                                   statistic, conf.level, style_fun, df) {
  if (is.null(by) || is.na(level)) {
    design <- tbl$inputs$data
  } else {
    design <- subset(
      tbl$inputs$data,
      tbl$inputs$data$variables[[by]] == level
    )
  }

  df_ci <-
    survey::svyquantile(
      c_form(right = variable),
      design = design,
      quantiles = .5,
      alpha = 1 - conf.level,
      interval.type = method,
      ci = TRUE,
      na.rm = TRUE,
      df = df
    ) %>%
    purrr::pluck(1) %>%
    dplyr::as_tibble() %>%
    set_names(c("estimate", "conf.low", "conf.high", "se"))

  # round and format CI
  df_ci %>%
    select(all_of(c("conf.low", "conf.high"))) %>%
    dplyr::mutate_all(style_fun) %>%
    glue::glue_data(statistic) %>%
    as.character()
}

calculate_svyprop_ci <- function(variable, variable_levels, by, level, tbl,
                                 method, statistic, conf.level, style_fun, df) {
  percent <- tbl$inputs$percent

  if (is.null(by) || is.na(level)) {
    design <- tbl$inputs$data
    design$variables[["..binary..svyciprop.."]] <-
      design$variables[[variable]] == variable_levels
  } else if (percent == "column") {
    design <- subset(
      tbl$inputs$data,
      tbl$inputs$data$variables[[by]] == level
    )
    design$variables[["..binary..svyciprop.."]] <-
      design$variables[[variable]] == variable_levels
  } else if (percent == "row") {
    design <- subset(
      tbl$inputs$data,
      tbl$inputs$data$variables[[variable]] == variable_levels
    )
    design$variables[["..binary..svyciprop.."]] <-
      design$variables[[by]] == level
  } else {
    design <- subset(
      tbl$inputs$data,
      !is.na(tbl$inputs$data$variables[[variable]])
    )
    design$variables[["..binary..svyciprop.."]] <-
      design$variables[[by]] == level &
      design$variables[[variable]] == variable_levels
  }

  df_ci <-
    survey::svyciprop(
      c_form(right = "..binary..svyciprop.."),
      design = design,
      method = method,
      level = conf.level,
      df = df
    ) %>%
    purrr::attr_getter("ci")() %>%
    tibble::as_tibble_row() %>%
    set_names(c("conf.low", "conf.high"))

  # round and format CI
  df_ci %>%
    select(all_of(c("conf.low", "conf.high"))) %>%
    dplyr::mutate_all(style_fun) %>%
    glue::glue_data(statistic) %>%
    as.character()
}
