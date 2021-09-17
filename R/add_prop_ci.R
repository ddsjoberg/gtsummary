#' Add Proportion CIs
#'
#' Add a new column with the confidence intervals for proportions.
#'
#' @param x A `tbl_summary` object
#' @param pattern String indicating how the confidence interval will be placed.
#' Default is `"{conf.low}%, {conf.high}%"`
#' @param method Confidence interval method. Must be one of
#' `c("wilson", "wilson.no.correct", "exact", "asymptotic")`. Default is `"wilson"`.
#' See details below.
#' @param conf.level Confidence level. Default is `0.95`
#' @param ci_fun Function to style upper and lower bound of confidence
#' interval. Default is the function that styled the proportion in the
#' original `tbl_summary()` call.
#' @param ... Not used
#'
#' @section method argument:
#' Methods `c("wilson", "wilson.no.correct")` are calculated with `prop.test()`.
#' The default method, `"wilson"`, includes the Yates continuity correction.
#' Methods `c("exact", "asymptotic")` are calculated with `Hmisc::binconf(method=)`.
#'
#' @return gtsummary table
#' @rdname add_prop_ci
#' @export
#'
#' @family tbl_summary tools
#' @examples
#' # Example 1 ----------------------------------
#' add_prop_ci_ex1 <-
#'   trial %>%
#'   select(response, trt) %>%
#'   tbl_summary(missing = "no") %>%
#'   add_prop_ci()
#'
#' # Example 2 ----------------------------------
#' add_prop_ci_ex2 <-
#'   trial %>%
#'     select(response, trt) %>%
#'     tbl_summary(statistic = all_categorical() ~ "{p}%",
#'                 missing = "no") %>%
#'     add_prop_ci() %>%
#'     modify_cols_merge(
#'       rows = !is.na(prop_ci_stat_0),
#'       pattern = "{stat_0} ({prop_ci_stat_0})"
#'     ) %>%
#'     modify_footnote(everything() ~ NA)
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_prop_ci_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_prop_ci_ex2.png}{options: width=45\%}}
add_prop_ci <- function(x, ...) {
  UseMethod("add_prop_ci")
}

#' @rdname add_prop_ci
#' @export
add_prop_ci.tbl_summary <- function(x,
                                    pattern = "{conf.low}%, {conf.high}%",
                                    method = c("wilson", "wilson.no.correct",
                                               "exact", "asymptotic"),
                                    conf.level = 0.95,
                                    ci_fun = NULL, ...) {
  # resolving arguments --------------------------------------------------------
  method <- match.arg(method)
  if(!is.null(ci_fun)) ci_fun <- gts_mapper(ci_fun, "add_prop_ci(ci_fun=)")

  # checking inputs ------------------------------------------------------------
  if (!rlang::is_string(pattern))
    stop("`pattern=` must be a string.", call. = FALSE)
  updated_call_list <- c(x$call_list, list(add_prop_ci = match.call()))

  # adding new column with CI --------------------------------------------------
  x <-
    x %>%
    add_stat(
      fns = all_categorical() ~ purrr::partial(single_prop_ci,
                                               method = method,
                                               conf.level = conf.level,
                                               pattern = pattern,
                                               ci_fun = ci_fun),
      location = list(all_dichotomous() ~ "label", all_categorical(FALSE) ~ "level")
    ) %>%
    # moving the CI cols to after the original stat cols (when `by=` variable present)
    # also renaming CI columns
    modify_table_body(
      function(.x) {
        cols_to_order <-
          .x %>%
          select(all_stat_cols(), matches("^stat_\\d+_prop_ci$")) %>%
          names()

        .x %>%
          dplyr::relocate(all_of(sort(cols_to_order)),
                          .before = all_of(sort(cols_to_order)[1])) %>%
          dplyr::rename_with(
            .fn = ~paste0(
              "prop_ci_",
              stringr::str_replace(., pattern = "_prop_ci$", replacement = "")),
            .cols = matches("^stat_\\d+_prop_ci$")
          )
      }
    ) %>%
    # updating CI column headers and footnotes
    modify_header(matches("^prop_ci_stat_\\d+$") ~ paste0("**", conf.level*100, "% CI**")) %>%
    modify_footnote(
      update = matches("^prop_ci_stat_\\d+$") ~ translate_text("CI = Confidence Interval"),
      abbreviation = TRUE)

  # return gtsummary table -----------------------------------------------------
  x$call_list <- updated_call_list
  x
}

# function to add CI for one variable
single_prop_ci <- function(variable, by, tbl, method, conf.level, ci_fun, pattern, ...) {
  ci_fun <-
    ci_fun %||%
    attr(tbl$meta_data[tbl$meta_data$variable %in% variable, ]$df_stats[[1]]$p, "fmt_fun")

  tbl$meta_data %>%
    filter(.data$variable %in% .env$variable) %>%
    purrr::pluck("df_stats", 1) %>%
    dplyr::rowwise() %>%
    mutate(
      ci =
        calculate_prop_ci(x = .data$n, n = .data$N,
                          pattern = pattern,
                          method = method,
                          conf.level = conf.level,
                          ci_fun = ci_fun)
    ) %>%
    select(any_of(c("col_name", "variable_levels", "ci"))) %>%
    tidyr::pivot_wider(
      id_cols = any_of("variable_levels"),
      values_from = .data$ci,
      names_from = .data$col_name
    ) %>%
    select(all_stat_cols()) %>%
    dplyr::rename_with(.fn = ~paste0(., "_prop_ci"))
}

calculate_prop_ci <- function(x, n, pattern, method, conf.level, ci_fun) {
  # calculate CI
  if (method %in% c("wilson", "wilson.no.correct")) {
    df_ci <-
      stats::prop.test(x = x, n = n,
                       conf.level = conf.level,
                       correct = isTRUE(method == "wilson")) %>%
      broom::tidy()
  }
  else if (method %in% c("exact", "asymptotic")) {
    assert_package("Hmisc", fn = 'add_prop_ci(method = c("exact", "asymptotic"))')
    df_ci <-
      Hmisc::binconf(x = x, n = n,
                     method = method, alpha = 1 - conf.level) %>%
      as.data.frame() %>%
      set_names(c("estimate", "conf.low", "conf.high"))
  }

  # round and format CI
  df_ci %>%
    select(all_of(c("conf.low", "conf.high"))) %>%
    dplyr::mutate_all(ci_fun) %>%
    glue::glue_data(pattern) %>%
    as.character()
}

