#' Add Proportion CIs
#'
#' Add a new column with the confidence intervals for proportions.
#'
#' @param x A `tbl_summary` object
#' @param pattern String indicating how the confidence interval will be placed.
#' Default is `"{conf.low}%, {conf.high}%"`
#' @param method Confidence interval method. Must be one of
#' `c("wilson", "exact", "asymptotic")`. Default is `"wilson"`.
#' See `Hmisc::binconf(method=)` for details.
#' @param conf.level Confidence level. Default is `0.95`
#' @param ci_fun Function to style upper and lower bound of confidence
#' interval. Default is the function that styled the proportion in the
#' original `tbl_summary()` call.
#'
#' @return gtsummary table
#' @export
#'
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
#'     modify_table_styling(
#'       columns = stat_0,
#'       rows = !is.na(ci),
#'       cols_merge_pattern = "{stat_0} ({ci})"
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

add_prop_ci <- function(x,
                        pattern = "{conf.low}%, {conf.high}%",
                        method = c("wilson", "exact", "asymptotic"),
                        conf.level = 0.95,
                        ci_fun = NULL) {
  # resolving arguments --------------------------------------------------------
  updated_call_list <- c(x$call_list, list(add_prop_ci = match.call()))
  method <- match.arg(method)
  if(!is.null(ci_fun)) ci_fun <- gts_mapper(ci_fun, "add_prop_ci(ci_fun=)")

  # checking inputs ------------------------------------------------------------
  assert_package("Hmisc", fn = "add_prop_ci()")
  if (!inherits(x, "tbl_summary"))
    stop("`x=` must be class 'tbl_summary'", call. = FALSE)
  if (!is.null(x$df_by))
    stop("`add_prop_ci()` cannot be run after `tbl_summary()` with the `by=` argument", call. = FALSE)
  if (!is.null(ci_fun) && !rlang::is_function(ci_fun))
    stop("`ci_fun=` must be a function.", call. = FALSE)
  if (!rlang::is_string(pattern))
    stop("`pattern=` must be a string.", call. = FALSE)

  # adding new column with CI --------------------------------------------------
  x <-
    x %>%
    add_stat(
      fns = all_categorical() ~ purrr::partial(single_prop_ci,
                                               method = method,
                                               alpha = 1 - conf.level,
                                               pattern = pattern,
                                               ci_fun = ci_fun),
      location = list(all_dichotomous() ~ "label", all_categorical(FALSE) ~ "level")
    ) %>%
    modify_header(ci = paste0("**", conf.level*100, "% CI**")) %>%
    modify_footnote(update = ci ~ "CI = Confidence Interval", abbreviation = TRUE)

  # return gtsummary table -----------------------------------------------------
  x$call_list <- updated_call_list
  x
}

single_prop_ci <- function(variable, by, tbl, method, alpha, ci_fun, pattern, ...) {
  ci_fun <-
    ci_fun %||%
    attr(tbl$meta_data[tbl$meta_data$variable %in% variable, ]$df_stats[[1]]$p, "fmt_fun")

  tbl$meta_data %>%
    filter(.data$variable %in% .env$variable) %>%
    purrr::pluck("df_stats", 1) %>%
    dplyr::rowwise() %>%
    mutate(
      ci =
        Hmisc::binconf(x = .data$n, n = .data$N,
                       method = method, alpha = alpha) %>%
        as.data.frame() %>%
        set_names(c("estimate", "conf.low", "conf.high")) %>%
        dplyr::mutate_all(ci_fun) %>%
        glue::glue_data(pattern)
    ) %>%
    select(.data$ci)
}



