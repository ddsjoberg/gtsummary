#' Add CI Column
#'
#' Add a new column with the confidence intervals for proportions, means, etc.
#'
#' @param x A `tbl_summary` object
#' @param statistic Formula indicating how the confidence interval will be displayed.
#' Default is `list(all_categorical() ~ "{conf.low}%, {conf.high}%", all_continuous() ~ "{conf.low}, {conf.high}")`
#' @param method Confidence interval method. Default is
#' `list(all_categorical() ~ "wilson", all_continuous() ~ "t.test")`.
#' Must be one of
#' `c("wilson", "wilson.no.correct", "exact", "asymptotic")` for categorical
#' variables, and `c("t.test", "wilcox.test")` for continuous variables.
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
#' @param ... Not used
#' @inheritParams tbl_summary
#'
#' @section method argument:
#' Methods `c("wilson", "wilson.no.correct")` are calculated with
#' `prop.test(correct = c(TRUE, FALSE))`.
#' The default method, `"wilson"`, includes the Yates continuity correction.
#' Methods `c("exact", "asymptotic")` are calculated with `Hmisc::binconf(method=)`.
#' Confidence intervals for means are calculated using `t.test()` and
#' `wilcox.test()` for pseudo-medians.
#'
#' @return gtsummary table
#' @rdname add_ci
#' @export
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#'
#' @family tbl_summary tools
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' add_ci_ex1 <-
#'   trial %>%
#'   select(marker, response, trt) %>%
#'   tbl_summary(missing = "no",
#'               statistic = all_continuous() ~ "{mean} ({sd})") %>%
#'   add_ci()
#'
#' # Example 2 ----------------------------------
#' add_ci_ex2 <-
#'   trial %>%
#'     select(response, grade) %>%
#'     tbl_summary(statistic = all_categorical() ~ "{p}%",
#'                 missing = "no") %>%
#'     add_ci(pattern = "{stat} ({ci})") %>%
#'     modify_footnote(everything() ~ NA)
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
                               pattern = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
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

  method <-
    .formula_list_to_named_list(
      x = list(all_categorical() ~ "wilson", all_continuous() ~ "t.test"),
      var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include,]),
      arg_name = "method"
    ) %>%
    purrr::update_list(
      !!!.formula_list_to_named_list(
        x = method,
        var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include,]),
        arg_name = "method",
        type_check = chuck(type_check, "is_string", "fn"),
        type_check_msg = chuck(type_check, "is_string", "msg")
      )
    )

  style_fun <-
    .formula_list_to_named_list(
      x = list(all_categorical() ~ purrr::partial(style_sigfig, scale =  100),
               all_continuous() ~ style_sigfig),
      var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include,]),
      arg_name = "style_fun"
    ) %>%
    purrr::update_list(
      !!!.formula_list_to_named_list(
        x = style_fun,
        var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include,]),
        arg_name = "style_fun",
        type_check = chuck(type_check, "is_function", "fn"),
        type_check_msg = chuck(type_check, "is_function", "msg")
      )
    )

  statistic <-
    .formula_list_to_named_list(
      x = list(all_categorical() ~ "{conf.low}%, {conf.high}%",
               all_continuous() ~ "{conf.low}, {conf.high}"),
      var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include,]),
      arg_name = "statistic"
    ) %>%
    purrr::update_list(
      !!!.formula_list_to_named_list(
        x = statistic,
        var_info = meta_data_to_var_info(x$meta_data[x$meta_data$variable %in% include,]),
        arg_name = "statistic",
        type_check = chuck(type_check, "is_character", "fn"),
        type_check_msg = chuck(type_check, "is_character", "msg")
      )
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
      paste("The `pattern=` argument should",
            "include reference to both '{stat}' and '{ci}'") %>%
        stop(call. = FALSE)
    }
  }

  # adding new column with CI --------------------------------------------------
  x <-
    x %>%
    add_stat(
      fns = everything() ~ purrr::partial(single_ci,
                                          method = method,
                                          conf.level = conf.level,
                                          statistic = statistic,
                                          style_fun = style_fun,
                                          summary_type = summary_type),
      location = list(everything() ~ "label", all_categorical(FALSE) ~ "level")
    ) %>%
    # moving the CI cols to after the original stat cols (when `by=` variable present)
    # also renaming CI columns
    modify_table_body(
      function(.x) {
        cols_to_order <-
          .x %>%
          select(all_stat_cols(), matches("^stat_\\d+_ci$")) %>%
          names()

        .x %>%
          dplyr::relocate(all_of(sort(cols_to_order)),
                          .before = all_of(sort(cols_to_order)[1])) %>%
          dplyr::rename_with(
            .fn = ~paste0(
              "ci_",
              stringr::str_replace(., pattern = "_ci$", replacement = "")),
            .cols = matches("^stat_\\d+_ci$")
          )
      }
    )

  if (is.null(pattern)) {
    x <-
      x %>%
      # updating CI column headers and footnotes
      modify_header(matches("^ci_stat_\\d+$") ~ paste0("**", conf.level*100, "% CI**")) %>%
      modify_footnote(
        update = matches("^ci_stat_\\d+$") ~ translate_text("CI = Confidence Interval"),
        abbreviation = TRUE)
  }
  else {
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
        ~expr(modify_table_styling(
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
      purrr::reduce(~rlang::inject(!!.x %>% !!.y), .init = x) %>%
      modify_footnote(
        update = all_stat_cols() ~ translate_text("CI = Confidence Interval"),
        abbreviation = TRUE)

    # updating header using `pattern=` argument
    x$table_styling$header <-
      x$table_styling$header %>%
      rowwise() %>%
      mutate(
        label =
          ifelse(
            .data$column %in% stat_column_names,
            glue::glue_data(
              .x = list(stat = .data$label, ci = paste0("**", conf.level*100, "% CI**")),
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

# function to add CI for one variable
single_ci <- function(variable, by, tbl, method, conf.level,
                      style_fun, statistic, summary_type, ...) {
  if (method[[variable]] %in% c("wilson", "wilson.no.correct",
                                "exact", "asymptotic") &&
      summary_type[[variable]] %in% c("categorical", "dichotomous")) {
    df_single_ci <-
      tbl$meta_data %>%
      filter(.data$variable %in% .env$variable) %>%
      purrr::pluck("df_stats", 1) %>%
      dplyr::rowwise() %>%
      mutate(
        ci =
          calculate_prop_ci(x = .data$n, n = .data$N,
                            statistic = statistic[[variable]],
                            method = method[[variable]],
                            conf.level = conf.level,
                            style_fun = style_fun[[variable]])

      )
  }
  else if (method[[variable]] %in% c("t.test", "wilcox.test") &&
           summary_type[[variable]] %in% c("continuous", "continuous2")) {
    df_single_ci <-
      tbl$inputs$data %>%
      dplyr::group_by_at(tbl$by) %>%
      tidyr::nest(data = -all_of(tbl$by)) %>%
      dplyr::rowwise() %>%
      mutate(
        ci =
          calculate_mean_ci(data = .data$data,
                            variable = variable,
                            statistic = statistic[[variable]],
                            method = method[[variable]],
                            conf.level = conf.level,
                            style_fun = style_fun[[variable]],
                            tbl = tbl)
      )
    if (is.null(tbl$by)) {
      df_single_ci <-
        df_single_ci %>%
        mutate(col_name = "stat_0") %>%
        select(any_of(c("col_name", "variable_levels", "ci")))
    }
    else {
      df_single_ci <-
        df_single_ci %>%
        dplyr::rename(by = all_of(tbl$by)) %>%
        left_join(
          tbl$df_by %>% select("by", col_name = "by_col"),
          by = "by"
        ) %>%
        select(any_of(c("by", "col_name", "ci")))
    }
  }
  else {
    glue("Error with variable '{variable}'. Method '{method[[variable]]}' ",
         "cannot be applied to summary type '{summary_type[[variable]]}'.") %>%
      stop(call. = FALSE)
  }

  df_single_ci %>%
    tidyr::pivot_wider(
      id_cols = any_of("variable_levels"),
      values_from = "ci",
      names_from = "col_name"
    ) %>%
    select(all_stat_cols()) %>%
    dplyr::rename_with(.fn = ~paste0(., "_ci"))
}

calculate_mean_ci <- function(data, variable, statistic,
                              method, conf.level, style_fun, tbl) {
  if (method %in% "t.test") {
    if (!"mean" %in%
        names(tbl$meta_data[tbl$meta_data$variable %in% variable, ]$df_stats[[1]])) {
      paste("{.code add_ci()} added mean CI for {.val {variable}};",
            "however, no mean is shown in the {.code tbl_summary()} table.") %>%
        cli::cli_alert_danger()
    }
    df_ci <-
      stats::t.test(data[[variable]], conf.level = conf.level) %>%
      broom::tidy()
  }
  else if (method %in% "wilcox.test") {
    if (!"median" %in%
        names(tbl$meta_data[tbl$meta_data$variable %in% variable, ]$df_stats[[1]])) {
      paste("{.code add_ci()} added pseudo-median CI for {.val {variable}};",
            "however, no median is shown in the {.code tbl_summary()} table.") %>%
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
      stats::prop.test(x = x, n = n,
                       conf.level = conf.level,
                       correct = isTRUE(method == "wilson")) %>%
      broom::tidy()
  }
  else if (method %in% c("exact", "asymptotic")) {
    assert_package("Hmisc", fn = 'add_ci(method = c("exact", "asymptotic"))')
    df_ci <-
      Hmisc::binconf(x = x, n = n,
                     method = method, alpha = 1 - conf.level) %>%
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

