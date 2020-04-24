#' Set a gtsummary theme
#'
#' Use this function to set preferences for the display of gtsummary tables.
#' The default formatting and styling throughout the gtsummary package are
#' taken from the published reporting guidelines of the top four urology
#' journals: European Urology, The Journal of Urology, Urology and
#' the British Journal of Urology International. Use this function to change
#' the default reporting style to match another journal, or your own
#' personal style.
#'
#' @section Themes:
#' - `gtsummary_theme_jama()`
#'   - sets theme to align with the JAMA reporting guidelines
#'   - large p-values are rounded to two decimal places
#'   - in `tbl_summary()` the statistic reported appears on the variable label row
#'   - in `tbl_summary()` the IQR is separated with a dash, rather than comma
#'   - in `tbl_summary()` the percent symbol is not printed next to percentages
#' - `gtsummary_theme_compact()`
#'   - tables printed with gt will be compact with smaller font size and reduced cell padding
#'
#' Use `gtsummary_reset_theme()` to restore the default settings
#'
#' Review the [customizing gtsummary themes][customize_gsummary_theme] help file
#' to create your own theme.
#' @param x A gtsummary theme function, e.g. `gtsummary_theme_jama()`, or a
#' named list defining a gtsummary theme. See details below.
#' @name set_gtsummary_theme
#' @export
#' @examples
#' # Setting JAMA theme for gtsummary
#' set_gtsummary_theme(gtsummary_theme_jama())
#' # Themes can be combined by including more than one
#' set_gtsummary_theme(gtsummary_theme_compact())
#'
#' set_gtsummary_theme_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   as_gt()
#'
#' # reset gtsummary theme
#' gtsummary_reset_theme()
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{set_gtsummary_theme_ex1.png}{options: width=70\%}}

set_gtsummary_theme <- function(x) {
  # checking the input is a named list -----------------------------------------
  if (!inherits(x, "list") || is.null(names(x)) || "" %in% names(x)) {
    stop("Argument `x=` must be  named list.", call. = FALSE)
  }

  # check that all the names are proper names that set gtsummary attributes ----
  if (!all(names(x) %in% df_theme_elements$name)) {
    not_name <- names(x) %>% setdiff(df_theme_elements$name)
    stop(paste(
      "List elements", quoted_list(not_name), "are not accepted theme elements."
    ), call. = FALSE)
  }

  # adding theme elements to environment ---------------------------------------
  rlang::env_bind(.env = env_gtsummary_theme, !!!x)
}

# initializing new env where all gtsummary theme elements are saved
env_gtsummary_theme <- rlang::new_environment()

# converts a character vector into a quotes list separated by a comma, eg 'a', 'b'
quoted_list <- function(x) {
  paste(sQuote(x), collapse = ", ")
}

# ------------------------------------------------------------------------------
# this function grabs a gtsummary theme element if it exists
# otherwise returns the default value
get_theme_element <- function(x, default = NULL, eval = TRUE) {
  # checking input
  if (!x %in% df_theme_elements$name) {
    stop("`x=` is not a proper gtsummary theme element.", call. = FALSE)
  }

  # returning theme element
  # the theme element is evaluated in the caller env so it may conditionally
  # set a default depending on other objects only known at the time it is called
  if (eval == TRUE)
    return(
      rlang::eval_tidy(
        env_gtsummary_theme[[x]],
        env = rlang::caller_env()
      ) %||% default
    )

  # if eval is FALSE, then returning the unevaluated theme element
  env_gtsummary_theme[[x]] %||% default
}



# ------------------------------------------------------------------------------
#' @name set_gtsummary_theme
#' @export
gtsummary_reset_theme <- function() {
  # deleting theme environment if it exists
  rm(list = ls(envir = env_gtsummary_theme),
     envir = env_gtsummary_theme)

  invisible()
}

# ------------------------------------------------------------------------------
#' @name set_gtsummary_theme
#' @export
gtsummary_theme_jama <- function() {
  list(
    "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
    "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
    "pkgwide-str:theme_name" = "JAMA",
    "tbl_summary-str:label" = "{var_label}, {stat_label}",
    "tbl_summary-lgl:show_stat_footnote" = FALSE,
    "tbl_summary-str:continuous_stat" = "{median} ({p25} - {p75})",
    "tbl_summary-str:categorical_stat" = "{n} ({p})"
  )
}

# ------------------------------------------------------------------------------
#' @name set_gtsummary_theme
#' @export
gtsummary_theme_compact <- function(){
  list(
    "as_gt-expr:addl_cmds" =
      expr(gt::tab_options(table.font.size = 'small', data_row.padding = gt::px(1)))
  )
}



# tibble of all possible theme options
# THIS DATA FRAME IS SAVED IN "data-raw/gtsummary_themes/gtsummary_theme_elements.csv"
# OPEN THE FILE, COPY THE CELLS, USE THE datapasta PACKAGE TO PASTE AS TRIBBLE HERE
df_theme_elements <-
  tibble::tribble(
                    ~fn,                                               ~name, ~argument,                                                                                                       ~desc,
              "pkgwide",                            "pkgwide-str:theme_name",     FALSE,                                                                                    "optional name of theme",
              "pkgwide",                          "pkgwide-str:print_engine",     FALSE,                                                                "string indicating the default print engine",
              "pkgwide",                             "pkgwide-fn:pvalue_fun",     FALSE,                                                             "function to style p-values throughout package",
              "pkgwide",                      "pkgwide-fn:prependpvalue_fun",     FALSE, "function to style p-values throughout package that include a \"p\" prefix, e.g. \"p<0.001\" or \"p=0.12\"",
                "as_gt",                              "as_gt-expr:addl_cmds",     FALSE,                                    "expression of {gt} commands appended to the end of each `as_gt()` call",
          "tbl_summary",                             "tbl_summary-arg:label",      TRUE,                                                                                                          NA,
          "tbl_summary",                         "tbl_summary-arg:statistic",      TRUE,                                                                                                          NA,
          "tbl_summary",                            "tbl_summary-arg:digits",      TRUE,                                                                                                          NA,
          "tbl_summary",                              "tbl_summary-arg:type",      TRUE,                                                                                                          NA,
          "tbl_summary",                             "tbl_summary-arg:value",      TRUE,                                                                                                          NA,
          "tbl_summary",                           "tbl_summary-arg:missing",      TRUE,                                                                                                          NA,
          "tbl_summary",                      "tbl_summary-arg:missing_text",      TRUE,                                                                                                          NA,
          "tbl_summary",                           "tbl_summary-arg:percent",      TRUE,                                                                                                          NA,
          "tbl_summary",                              "tbl_summary-arg:sort",      TRUE,                                                                                                          NA,
          "tbl_summary",                        "tbl_summary-fn:percent_fun",     FALSE,                                                                             "function to style percentages",
          "tbl_summary",                             "tbl_summary-str:label",     FALSE,                  "glue string defining the final label displayed. any column in `.$meta_data` may be used.",
          "tbl_summary",                "tbl_summary-lgl:show_stat_footnote",     FALSE,                                       "logical indicating whether to show footnote of displayed statistics",
          "tbl_summary",                   "tbl_summary-str:continuous_stat",     FALSE,                                 "glue string defining the default continuous summary statistics to display",
          "tbl_summary",                  "tbl_summary-str:categorical_stat",     FALSE,                "glue string defining the default categorical and dichotomous summary statistics to display",
    "add_p.tbl_summary",                        "add_p.tbl_summary-arg:test",      TRUE,                                                                                                          NA,
    "add_p.tbl_summary",                  "add_p.tbl_summary-arg:pvalue_fun",      TRUE,                                                                                                          NA,
    "add_p.tbl_summary",        "add_p.tbl_summary-attr:test.continuous_by2",     FALSE,                                          "default test for continuous variables with a 2-level by variable",
    "add_p.tbl_summary",            "add_p.tbl_summary-attr:test.continuous",     FALSE,                                 "default test for continuous variables with a 3- or more level by variable",
    "add_p.tbl_summary",           "add_p.tbl_summary-attr:test.categorical",     FALSE,                                                        "default test for categorical/dichotomous variables",
    "add_p.tbl_summary", "add_p.tbl_summary-attr:test.categorical.low_count",     FALSE,                         "default test for categorical/dichotomous variables with minimum expected count <5",
    "add_p.tbl_summary", "add_p.tbl_summary-attr:test.categorical.group_by2",     FALSE,          "default test for categorical/dichotomous grouped/correlated variables with a 2-level by variable",
    "add_p.tbl_summary",  "add_p.tbl_summary-attr:test.continuous.group_by2",     FALSE,                       "default test for continuous grouped/correlated variables with a 2-level by variable",
                "add_q",                                  "add_q-arg:method",      TRUE,                                                                                                          NA,
                "add_q",                              "add_q-arg:pvalue_fun",      TRUE,                                                                                                          NA,
      "add_p.tbl_cross",                          "add_p.tbl_cross-arg:test",      TRUE,                                                                                                          NA,
      "add_p.tbl_cross",                    "add_p.tbl_cross-arg:pvalue_fun",      TRUE,                                                                                                          NA,
          "tbl_survfit",                         "tbl_survfit-arg:statistic",      TRUE,                                                                                                          NA,
       "tbl_regression",                     "tbl_regression-arg:conf.level",      TRUE,                                                                                                          NA,
       "tbl_regression",                   "tbl_regression-arg:estimate_fun",      TRUE,                                                                                                          NA,
       "tbl_regression",                     "tbl_regression-arg:pvalue_fun",      TRUE,                                                                                                          NA,
       "tbl_regression",                       "tbl_regression-arg:tidy_fun",      TRUE,                                                                                                          NA
    )
