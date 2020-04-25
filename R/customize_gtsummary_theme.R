#' Customize gtsummary themes
#'
#' With the [set_gtsummary_theme()] function, you can set various themes for
#' the gtsummary package. To construct a personalized theme, create a named list
#' of at least one of the following theme elements. Example below.
#' @name customize_gsummary_theme
#' @keywords internal
#' @examples
#' # create custom theme
#' my_theme <- list(
#'   "pkgwide-str:theme_name" = "My Template",
#'   "tbl_summary-str:label" = "{var_label}, {stat_label}",
#'   "tbl_summary-lgl:show_stat_footnote" = FALSE,
#'   "tbl_summary-arg:statistic" = list(
#'     all_continuous() ~ "{median} ({p25} - {p75})",
#'     all_categorical() ~ "{n} ({p})"
#'   ),
#'   "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2)
#' )
#'
#' # setting the theme
#' set_gtsummary_theme(my_theme)
#'
#' custom_theme_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p()
#'
#' # reset gtsummary theme
#' gtsummary_reset_theme()
#'
#' @section Theme Elements:
#'
#' The following fields are available to set:
#' ## Function Elements
#' The following are system defaults that can be modified.
#' ### `Package-wide`
#' - `pkgwide-str:theme_name` optional name of theme
#' - `pkgwide-str:print_engine` string indicating the default print engine
#' - `pkgwide-fn:pvalue_fun` function to style p-values throughout package
#' - `pkgwide-fn:prependpvalue_fun` function to style p-values throughout package that include a "p" prefix, e.g. "p<0.001" or "p=0.12"
#' ### `as_gt`
#' - `as_gt-expr:addl_cmds` expression of {gt} commands appended to the end of each `as_gt()` call
#' ### `tbl_summary`
#' - `tbl_summary-fn:percent_fun` function to style percentages
#' - `tbl_summary-str:continuous_stat` glue string defining the default continuous summary statistics to display
#' - `tbl_summary-str:categorical_stat` glue string defining the default categorical and dichotomous summary statistics to display
#' ### `add_p.tbl_summary`
#' - `add_p.tbl_summary-attr:test.continuous_by2` default test for continuous variables with a 2-level by variable
#' - `add_p.tbl_summary-attr:test.continuous` default test for continuous variables with a 3- or more level by variable
#' - `add_p.tbl_summary-attr:test.categorical` default test for categorical/dichotomous variables
#' - `add_p.tbl_summary-attr:test.categorical.low_count` default test for categorical/dichotomous variables with minimum expected count <5
#' - `add_p.tbl_summary-attr:test.categorical.group_by2` default test for categorical/dichotomous grouped/correlated variables with a 2-level by variable
#' - `add_p.tbl_summary-attr:test.continuous.group_by2` default test for continuous grouped/correlated variables with a 2-level by variable#' ## Function Arguments
#' ## Function Arguments
#' Set defaults for function arguments
#' - **`add_p.tbl_cross()`**: `add_p.tbl_cross-arg:test`, `add_p.tbl_cross-arg:pvalue_fun`
#' - **`add_p.tbl_summary()`**: `add_p.tbl_summary-arg:test`, `add_p.tbl_summary-arg:pvalue_fun`
#' - **`add_q()`**: `add_q-arg:method`, `add_q-arg:pvalue_fun`
#' - **`tbl_regression()`**: `tbl_regression-arg:conf.level`, `tbl_regression-arg:estimate_fun`, `tbl_regression-arg:pvalue_fun`, `tbl_regression-arg:tidy_fun`
#' - **`tbl_summary()`**: `tbl_summary-arg:label`, `tbl_summary-arg:statistic`, `tbl_summary-arg:digits`, `tbl_summary-arg:type`, `tbl_summary-arg:value`, `tbl_summary-arg:missing`, `tbl_summary-arg:missing_text`, `tbl_summary-arg:percent`, `tbl_summary-arg:sort`
#' - **`tbl_survfit()`**: `tbl_survfit-arg:statistic`
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{custom_theme_ex1.png}{options: width=60\%}}
NULL

# # The code below creates the markdown text to copy above
# # by function lines
# df_by_function <-
#   df_theme_elements %>%
#   dplyr::filter(argument == FALSE) %>%
#   dplyr::mutate(
#     fn = ifelse(fn == "pkgwide", "Package-wide", fn),
#     names_line = paste0("- `", name,"` ", desc)
#   ) %>%
#   dplyr::select(fn, names_line) %>%
#   dplyr::group_by(fn) %>%
#   tidyr::nest()
#
# for (i in 1:nrow(df_by_function)) {
#   cat("#' ### `", df_by_function$fn[i], "`\n", sep = "")
#   for (j in 1:nrow(df_by_function$data[[i]])) {
#     cat("#' ", df_by_function$data[[i]]$names_line[j], "\n", sep = "")
#   }
# }
#
# # the argument lines
# df_theme_elements %>%
#   dplyr::filter(argument == TRUE) %>%
#   dplyr::group_nest(fn) %>%
#   dplyr::mutate(
#     vec_names = purrr::map_chr(.data$data, ~paste0("`", .x$name, "`", collapse = ", ")),
#     final = glue::glue("#' - **`{fn}()`**: {vec_names}")
#   ) %>%
#   dplyr::pull(final) %>%
#   purrr::walk(~cat(.x, "\n"))


