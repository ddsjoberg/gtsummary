#' Set a gtsummary theme
#'
#' \lifecycle{maturing}
#' Use this function to set preferences for the display of gtsummary tables.
#' The default formatting and styling throughout the gtsummary package are
#' taken from the published reporting guidelines of the top four urology
#' journals: European Urology, The Journal of Urology, Urology and
#' the British Journal of Urology International. Use this function to change
#' the default reporting style to match another journal, or your own
#' personal style.
#'
#' @param x A gtsummary theme function, e.g. `theme_gtsummary_journal()`, or a
#' named list defining a gtsummary theme. See details below.
#' @name set_gtsummary_theme
#' @export
#' @seealso [Themes vignette](http://www.danieldsjoberg.com/gtsummary/articles/themes.html)
#' @seealso Available [gtsummary themes][theme_gtsummary]
#' @examples
#' # Setting JAMA theme for gtsummary
#' set_gtsummary_theme(theme_gtsummary_journal("jama"))
#' # Themes can be combined by including more than one
#' set_gtsummary_theme(theme_gtsummary_compact())
#'
#' set_gtsummary_theme_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   add_stat_label() %>%
#'   as_gt()
#'
#' # reset gtsummary theme
#' reset_gtsummary_theme()
#' @section Example Output:
#' \if{html}{Example}
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

  # print name of theme if present ---------------------------------------------
  if (!is.null(x$`pkgwide-str:theme_name`)) {
    rlang::inform(glue("Setting theme `{x$`pkgwide-str:theme_name`}`"))
  }

  # adding theme elements to environment ---------------------------------------
  rlang::env_bind(.env = env_gtsummary_theme, !!!x)
}

# initializing new env where all gtsummary theme elements are saved
env_gtsummary_theme <- rlang::new_environment()

# ------------------------------------------------------------------------------
# this function grabs a gtsummary theme element if it exists
# otherwise returns the default value
get_theme_element <- function(x, default = NULL, eval = TRUE) {
  # checking input
  if (!x %in% df_theme_elements$name) {
    stop(glue("`x = '{x}'` is not a proper gtsummary theme element."), call. = FALSE)
  }

  # returning theme element
  # if eval is FALSE, then returning the unevaluated theme element
  if (eval == FALSE) {
    return(env_gtsummary_theme[[x]] %||% default)
  }

  # the theme element is evaluated in the caller env so it may conditionally
  # set a default depending on other objects only known at the time it is called
  rlang::eval_tidy(env_gtsummary_theme[[x]], env = rlang::caller_env()) %||% default
}

# ------------------------------------------------------------------------------
#' @name set_gtsummary_theme
#' @export
reset_gtsummary_theme <- function() {
  # deleting theme environment if it exists
  rm(
    list = ls(envir = env_gtsummary_theme),
    envir = env_gtsummary_theme
  )

  invisible()
}
