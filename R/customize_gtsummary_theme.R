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
#'   theme_name = "My Template",
#'   "fn:tbl_summary-str:label" = "{var_label}, {stat_label}",
#'   "fn:tbl_summary-lgl:show_stat_footnote" = FALSE,
#'   "fn:tbl_summary-arg:statistic" = list(
#'     all_continuous() ~ "{median} ({p25} - {p75})",
#'     all_categorical() ~ "{n} ({p})"
#'   ),
#'   "fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2)
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
#' The following fields are available to set:
#'
#' ## Package-wide Settings
#' - `str:print_engine` default print engine
#' - `fn:pvalue_fun` default p-value formatting function
#'
#' ## Function Settings
#' ### `tbl_summary()`
#' - `fn:tbl_summary-fn:percent_fun` default function for styling percentages
#' - `fn:tbl_summary-str:label` label to display, e.g. `"{var_label}, {stat_label}"`
#' - `fn:tbl_summary-lgl:show_stat_footnote` logical indicating whether to show the statistic label footnote
#' ### `add_p.tbl_summary()`
#' - `fn:add_p-attr:test.continuous_by2` default test for continuous variables with a 2-level by variable
#' - `fn:add_p-attr:test.continuous`	default test for continuous variables with a 3- or more level by variable
#' - `fn:add_p-attr:test.categorical`	default test for categorical/dichotomous variables
#' - `fn:add_p-attr:test.categorical.low_count`	default test for categorical/dichotomous variables with minimum expected count <5
#' - `fn:add_p-attr:test.categorical.group_by2`	default test for categorical/dichotomous grouped/correlated variables with a 2-level by variable
#' - `fn:add_p-attr:test.continuous.group_by2`	default test for continuous grouped/correlated variables with a 2-level by variable
#' ### `as_gt()`
#' - `fn:as_gt-expr:addl_cmds` expression of gt commands to append to the calls in `as_gt()`
#' ## Function Arguments
#' Set defaults for function arguments
#' ### `tbl_summary()`
#' - `fn:tbl_summary-arg:label`
#' - `fn:tbl_summary-arg:statistic`
#' - `fn:tbl_summary-arg:digits`
#' - `fn:tbl_summary-arg:type`
#' - `fn:tbl_summary-arg:value`
#' - `fn:tbl_summary-arg:missing`
#' - `fn:tbl_summary-arg:missing_text`
#' - `fn:tbl_summary-arg:percent`
#' - `fn:tbl_summary-arg:sort`
#' ### `add_p.tbl_summary()`
#' - `fn:add_p.tbl_summary-arg:test`
#' - `fn:add_p.tbl_summary-arg:pvalue_fun`
#' - `fn:add_p.tbl_summary-arg:group`
#' ### `add_q()`
#' - `fn:add_q-arg:method`
#' - `fn:add_q-arg:pvalue_fun`
#'
#' ### Test
#'
#' ```{r, echo = FALSE, results = 'asis'}
#' options(knitr.kable.na = '')
#' head(df_theme_elements) %>%
#'   dplyr::mutate(name = glue("`{name}`")) %>%
#'   knitr::kable(col.names = c("", "", ""))
#' ```
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{custom_theme_ex1.png}{options: width=60\%}}
NULL

# # The code below creates the markdown text to copy above
# # the argument lines
# df_theme_elements %>%
#   dplyr::filter(argument == TRUE) %>%
#   dplyr::mutate(
#     fn = word(name, sep = "-") %>%
#       word(2, sep = ":")
#   ) %>%
#   dplyr::group_nest(fn) %>%
#   dplyr::mutate(
#     vec_names = purrr::map_chr(.data$data, ~paste0("`", .x$name, "`", collapse = ", ")),
#     final = glue::glue("#' - **`{fn}()`**: {vec_names}")
#   ) %>%
#   dplyr::pull(final) %>%
#   purrr::walk(~cat(.x, "\n"))
#
# # by function lines
# df_by_function <-
#   df_theme_elements %>%
#   dplyr::filter(argument == FALSE) %>%
#   dplyr::mutate(
#     fn = ifelse(stringr::str_starts(name, pattern = "fn:") & stringr::str_detect(name, "-"),
#                 word(name, sep = "-") %>% word(2, sep = ":"),
#                 "Package-wide"),
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
# purrr::walk2(df_by_function$fn, df_by_function$data,
#              ~cat("#' ### `", .x, "`", "\n",
#                   "- `", .y$, sep = ""))

