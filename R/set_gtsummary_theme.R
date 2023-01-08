#' Set a gtsummary theme
#'
#' @description
#' \lifecycle{maturing}
#' Functions to **set**, **reset**, **get**, and evaluate **with** gtsummary themes.
#'
#' - `set_gtsummary_theme()` set a theme
#' - `reset_gtsummary_theme()` reset themes
#' - `get_gtsummary_theme()` get a named list with all active theme elements
#' - `with_gtsummary_theme()` evaluate an expression with a theme temporarily set
#' - `check_gtsummary_theme()` checks if passed theme is valid
#'
#' @section Details:
#' The default formatting and styling throughout the gtsummary package are
#' taken from the published reporting guidelines of the top four urology
#' journals: European Urology, The Journal of Urology, Urology and
#' the British Journal of Urology International. Use this function to change
#' the default reporting style to match another journal, or your own
#' personal style.
#'
#' @param x A named list defining a gtsummary theme.
#' @param expr Expression to be evaluated with the theme specified in `x=` loaded
#' @param env The environment in which to evaluate `expr=`
#' @param msg_ignored_elements Default is NULL with no message printed. Pass a string
#' that will be printed with `cli::cli_alert_info()`. The `"{elements}"`
#' object contains vector of theme elements that will be overwritten and ignored.
#' @inheritParams add_global_p
#' @name set_gtsummary_theme
#' @export
#' @seealso [Themes vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html)
#' @seealso Available [gtsummary themes][theme_gtsummary]
#' @examples
#' \donttest{
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
#' }
#' @section Example Output:
#' \if{html}{Example}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "set_gtsummary_theme_ex1.png", width = "70")`
#' }}
NULL

# ------------------------------------------------------------------------------
#' @rdname set_gtsummary_theme
#' @export
set_gtsummary_theme <- function(x, quiet = NULL) {
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # checking the input is a named list -----------------------------------------
  if (rlang::is_empty(x)) {
    return(invisible())
  }
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
  if (!is.null(x$`pkgwide-str:theme_name`) && !isTRUE(quiet)) {
    rlang::inform(glue("Setting theme `{x$`pkgwide-str:theme_name`}`"))
  }

  # adding theme elements to environment ---------------------------------------
  rlang::env_bind(.env = env_gtsummary_theme, !!!x)
}

# ------------------------------------------------------------------------------
#' @rdname set_gtsummary_theme
#' @export
reset_gtsummary_theme <- function() {
  # deleting theme environment if it exists
  rm(
    list = ls(envir = env_gtsummary_theme),
    envir = env_gtsummary_theme
  )

  invisible()
}

# ------------------------------------------------------------------------------
#' @rdname set_gtsummary_theme
#' @export
get_gtsummary_theme <- function() {
  as.list(env_gtsummary_theme)
}

# ------------------------------------------------------------------------------
#' @rdname set_gtsummary_theme
#' @export
with_gtsummary_theme <- function(x, expr,
                                 env = rlang::caller_env(),
                                 msg_ignored_elements = NULL) {
  # save current theme ---------------------------------------------------------
  current_theme <- get_gtsummary_theme()

  # on exit, restore theme -----------------------------------------------------
  on.exit(reset_gtsummary_theme(), add = TRUE)
  on.exit(set_gtsummary_theme(current_theme, quiet = TRUE), add = TRUE)

  # message ignored theme elements ---------------------------------------------
  .msg_ignored_elements(x, current_theme, msg_ignored_elements)

  # add specified theme --------------------------------------------------------
  set_gtsummary_theme(suppressMessages(x), quiet = TRUE)

  # evaluate expression
  rlang::eval_tidy({{ expr }}, env = env)
}

.msg_ignored_elements <- function(x, current_theme, msg) {
  # if no message, dont print one!
  if (is.null(msg)) {
    return(invisible())
  }

  # save vector of theme element names that will be ignored
  elements <-
    x %>%
    imap(
      function(.x, .y) {
        # if theme element not currently set, return NULL
        if (is.null(current_theme[[.y]])) {
          return(NULL)
        }
        # if theme element is already set and is different, return the name of the element
        if (!identical(.x, current_theme[[.y]])) {
          return(.y)
        }
        return(NULL)
      }
    ) %>%
    purrr::compact() %>%
    unlist()

  # print message of ignored elements
  if (!rlang::is_empty(elements)) {
    cli::cli_alert_info(msg)
  }

  return(invisible())
}

# ------------------------------------------------------------------------------
#' @rdname set_gtsummary_theme
#' @export
check_gtsummary_theme <- function(x) {
  # themes must be named lists
  if (!inherits(x, "list") || !rlang::is_named(x)) {
    cli::cli_alert_danger("{.code x=} must be a named list.")
    return(invisible())
  }

  # check all names are true theme elements
  if (any(!names(x) %in% df_theme_elements$name)) {
    paste(
      "Theme element(s) {.val {setdiff(names(x), df_theme_elements$name)}}",
      "are not valid."
    ) %>%
      cli::cli_alert_danger()
    return(invisible())
  }

  cli::cli_alert_success("Looks good!")
  invisible()
}

# ------------------------------------------------------------------------------
# this function grabs a gtsummary theme element if it exists
# otherwise returns the default value
get_theme_element <- function(x, default = NULL, eval = TRUE) {
  # checking input REMOVED THIS CHECK IN v1.6.0 AND ADDED check_gtsummary_theme() instead
  # if (!x %in% df_theme_elements$name) {
  #   stop(glue("`x = '{x}'` is not a proper gtsummary theme element."), call. = FALSE)
  # }

  # returning theme element
  # if eval is FALSE, then returning the un-evaluated theme element
  if (eval == FALSE) {
    return(env_gtsummary_theme[[x]] %||% default)
  }

  # the theme element is evaluated in the caller env so it may conditionally
  # set a default depending on other objects only known at the time it is called
  rlang::eval_tidy(env_gtsummary_theme[[x]], env = rlang::caller_env()) %||% default
}
