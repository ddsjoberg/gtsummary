#' Set a gtsummary theme
#'
#' Use this function to set preferences for the display of gtsummary tables.
#' The default formatting and styling throughout the gtsummary package are
#' taken from the published reporting guidelines of the top four urology
#' journals: European Urology, The Journal of Urology, Urology and
#' the British Journal of Urology International. Use this function to change
#' the default reporting style to match another journal, or even your own
#' personal style!
#'
#' @section Themes:
#' - `gtsummary_theme_jama()` set theme to align with the JAMA reporting guidelines
#' - `gtsummary_theme_compact()` table printed with gt will be more compact with smaller font size and reduced cell padding
#'
#' Use `gtsummary_reset_theme()` to restore the default settings
#'
#' Review the [customizing gtsummary themes][customize_gsummary_theme] help file,
#' for details on creating your own themes.
#' @section Details:
#' @param x A named list defining a gtsummary theme. See details below.
#' @name set_gtsummary_theme
#' @export
#' @examples
#' # Setting JAMA theme for gtsummary
#' set_gtsummary_theme(gtsummary_theme_jama())
#'
#' set_gtsummary_theme_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, trt) %>%
#'   tbl_summary(by = trt)
#'
#' # reset gtsummary theme
#' gtsummary_reset_theme()
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{set_gtsummary_theme_ex1.png}{options: width=60\%}}

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

env_gtsummary_theme <- rlang::new_environment()

#' @name set_gtsummary_theme
#' @export
gtsummary_reset_theme <- function() {
  # deleting theme environment if it exists
  rm(list = ls(envir = env_gtsummary_theme),
     envir = env_gtsummary_theme)

  invisible()
}

#' @name set_gtsummary_theme
#' @export
gtsummary_theme_jama <- function() {
  list(
    theme_name = "JAMA",
    "fn:tbl_summary-str:label" = "{var_label}, {stat_label}",
    "fn:tbl_summary-lgl:show_stat_footnote" = FALSE,
    "fn:tbl_summary-arg:statistic" = list(
      all_continuous() ~ "{median} ({p25} - {p75})",
      all_categorical() ~ "{n} ({p})"
    ),
    "fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2)
  )
}

#' @name set_gtsummary_theme
#' @export
gtsummary_theme_compact <- function(){
  list(
    "fn:as_gt-expr:addl_cmds" =
      expr(gt::tab_options(table.font.size = 'small', data_row.padding = gt::px(1)))
  )
}

# this function grabs a gtsummary theme element if it exists
# otherwise returns the default value
get_theme_element <- function(x, default = NULL) {
  # checking input
  if (!x %in% df_theme_elements$name) {
    stop("`x=` is not a proper gtsummary theme element.", call. = FALSE)
  }

  # returning theme element
  env_gtsummary_theme[[x]] %||% default
}

# converts a character vector into a quotes list separated by a comma, eg 'a', 'b'
quoted_list <- function(x) {
  paste(sQuote(x), collapse = ", ")
}

# tibble of all possible theme options
df_theme_elements <-
  tibble::tribble(
    ~name, ~argument, ~desc,
    "theme_name", FALSE, "optional name of theme",
    # package level themes
    "str:print_engine", FALSE, "string indicating the default print engine",
    "fn:pvalue_fun", FALSE, "function to style p-values throughout package",
    # as_gt
    "fn:as_gt-expr:addl_cmds", FALSE,  "expression of {gt} commands appended to the end of each `as_gt()` call", # must be an expression, but should it be a string to make it easier for users?
    # tbl_summary
    "fn:tbl_summary-arg:label", TRUE, NA_character_,
    "fn:tbl_summary-arg:statistic", TRUE, NA_character_,
    "fn:tbl_summary-arg:digits", TRUE, NA_character_,
    "fn:tbl_summary-arg:type", TRUE, NA_character_,
    "fn:tbl_summary-arg:value", TRUE, NA_character_,
    "fn:tbl_summary-arg:missing", TRUE, NA_character_,
    "fn:tbl_summary-arg:missing_text", TRUE, NA_character_,
    "fn:tbl_summary-arg:percent", TRUE, NA_character_,
    "fn:tbl_summary-arg:sort", TRUE, NA_character_,
    "fn:tbl_summary-fn:percent_fun", FALSE, "function to style percentages",
    "fn:tbl_summary-str:label", FALSE, "glue string defining the final label displayed. any column in `.$meta_data` may be used.",
    "fn:tbl_summary-lgl:show_stat_footnote", FALSE, "logical indicating whether to show footnote of displayed statistics",
    # add_p.tbl_summary
    "fn:add_p.tbl_summary-arg:test", TRUE, NA_character_,
    "fn:add_p.tbl_summary-arg:pvalue_fun", TRUE, NA_character_,
    "fn:add_p-attr:test.continuous_by2", FALSE, "default test for continuous variables with a 2-level by variable",
    "fn:add_p-attr:test.continuous", FALSE, "default test for continuous variables with a 3- or more level by variable",
    "fn:add_p-attr:test.categorical", FALSE, "default test for categorical/dichotomous variables",
    "fn:add_p-attr:test.categorical.low_count", FALSE, "default test for categorical/dichotomous variables with minimum expected count <5",
    "fn:add_p-attr:test.categorical.group_by2", FALSE, "default test for categorical/dichotomous grouped/correlated variables with a 2-level by variable",
    "fn:add_p-attr:test.continuous.group_by2", FALSE, "default test for continuous grouped/correlated variables with a 2-level by variable",
    # add_q
    "fn:add_q-arg:method", TRUE, NA_character_,
    "fn:add_q-arg:pvalue_fun", TRUE, NA_character_
  )
