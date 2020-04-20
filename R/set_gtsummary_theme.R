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
#' @section Details:
#'
#' The following fields are available to set:
#'
#' ## Package-wide Settings
#' - `pkg:print_engine` default print engine
#' - `pkg:pvalue_fun` default p-value formatting function
#'
#' ## Function Settings
#' ### `tbl_summary()`
#' #### Set Default Function Arguments
#' - `fn:tbl_summary-arg:label`
#' - `fn:tbl_summary-arg:statistic`
#' - `fn:tbl_summary-arg:digits`
#' - `fn:tbl_summary-arg:type`
#' - `fn:tbl_summary-arg:value`
#' - `fn:tbl_summary-arg:missing`
#' - `fn:tbl_summary-arg:missing_text`
#' - `fn:tbl_summary-arg:percent`
#' - `fn:tbl_summary-arg:sort`
#' #### Other Settings
#' - `fn:tbl_summary-attr:percent_fun` default function for styling percentages
#' - `fn:tbl_summary-attr:label` label to display, e.g. `"{var_label}, {stat_label}"`
#' ### `add_p.tbl_summary()`
#' #### Set Default Function Arguments
#' - `fn:add_p.tbl_summary-arg:test`
#' - `fn:add_p.tbl_summary-arg:pvalue_fun`
#' - `fn:add_p.tbl_summary-arg:group`
#' #### Other Settings
#' - `fn:add_p-attr:test.continuous_by2` default test for continuous variables with 2-level by variable
#' - `fn:add_p-attr:test.continuous`	default test for continuous variables with 3- or more level by variable
#' - `fn:add_p-attr:test.categorical`	default test for categorical/dichotomous variables
#' - `fn:add_p-attr:test.categorical.low_count`	default test for categorical/dichotomous variables with minimum expected count <5 in one cell
#' - `fn:add_p-attr:test.categorical.group_by2`	default test for categorical/dichotomous grouped/correlated variables with 2-level by variable
#' - `fn:add_p-attr:test.continuous.group_by2`	default test for continuous grouped/correlated variables with 2-level by variable
#' ### `add_q()`
#' #### Set Default Function Arguments
#' - `fn:add_q-arg:method`
#' - `fn:add_q-arg:pvalue_fun`
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
#' gtsummary_theme_reset()

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
gtsummary_theme_reset <- function() {
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
    "fn:tbl_summary-attr:label" = "{var_label}, {stat_label}",
    "fn:tbl_summary-arg:statistic" = list(
      all_continuous() ~ "{median} ({p25} - {p75})",
      all_categorical() ~ "{n} ({p})"
    ),
    "pkg:pvalue_fun" = function(x) style_pvalue(x, digits = 2)
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
    ~name,
    "theme_name",
    # package level themes
    "pkg:print_engine",
    "pkg:pvalue_fun",
    # as_gt
    "fn:as_gt-attr:addl_cmds", # must be an expression, but should it be a string to make it easier for users?
    # tbl_summary
    "fn:tbl_summary-arg:label",
    "fn:tbl_summary-arg:statistic",
    "fn:tbl_summary-arg:digits",
    "fn:tbl_summary-arg:type",
    "fn:tbl_summary-arg:value",
    "fn:tbl_summary-arg:missing",
    "fn:tbl_summary-arg:missing_text",
    "fn:tbl_summary-arg:percent",
    "fn:tbl_summary-arg:sort",
    "fn:tbl_summary-attr:percent_fun",
    "fn:tbl_summary-attr:label",
    # add_p.tbl_summary
    "fn:add_p.tbl_summary-arg:test",
    "fn:add_p.tbl_summary-arg:pvalue_fun",
    "fn:add_p-attr:test.continuous_by2",
    "fn:add_p-attr:test.continuous",
    "fn:add_p-attr:test.categorical",
    "fn:add_p-attr:test.categorical.low_count",
    "fn:add_p-attr:test.categorical.group_by2",
    "fn:add_p-attr:test.continuous.group_by2",
    # add_q
    "fn:add_q-arg:method",
    "fn:add_q-arg:pvalue_fun"
  )
