#' Set gtsummary theme
#'
#' @description
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
#' @param x (named `list`)\cr
#'   A named list defining a gtsummary theme.
#' @param expr (`expression`)\cr
#'   Expression to be evaluated with the theme specified in `x=` loaded
#' @param env (`environment`)\cr
#'   The environment in which to evaluate `expr=`
#' @param msg_ignored_elements (`string`)\cr
#'   Default is `NULL` with no message printed. Pass a string
#'   that will be printed with `cli::cli_alert_info()`. The `"{elements}"`
#'   object contains vector of theme elements that will be overwritten and ignored.
#' @param quiet `r lifecycle::badge("deprecated")`
#'
#' @name set_gtsummary_theme
#' @export
#'
#' @seealso [Themes vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html)
#' @seealso Available [gtsummary themes][theme_gtsummary]
#' @examples
#' # Setting JAMA theme for gtsummary
#' set_gtsummary_theme(theme_gtsummary_journal("jama"))
#' # Themes can be combined by including more than one
#' set_gtsummary_theme(theme_gtsummary_compact())
#'
#' set_gtsummary_theme_ex1 <-
#'   trial |>
#'   tbl_summary(by = trt, include = c(age, grade, trt)) |>
#'   add_stat_label() |>
#'   as_gt()
#'
#' # reset gtsummary theme
#' reset_gtsummary_theme()
NULL

# set_gtsummary_theme ----------------------------------------------------------
#' @rdname set_gtsummary_theme
#' @export
set_gtsummary_theme <- function(x, quiet) {
  set_cli_abort_call()

  # deprecation ----------------------------------------------------------------
  if (!missing(quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::set_gtsummary_theme(quiet)",
      details = "Argument has been ignored."
    )
  }

  # checking the input is a named list -----------------------------------------
  if (is_empty(x)) {
    return(invisible())
  }
  if (!inherits(x, "list") || !is_named(x)) {
    cli::cli_abort(
      "The {.arg x} argument must be a named list.",
      call = get_cli_abort_call()
    )
  }

  # check that all the names are proper names that set gtsummary attributes ----
  if (!all(names(x) %in% df_theme_elements$name)) {
    not_name <- names(x) %>% setdiff(df_theme_elements$name)
    cli::cli_abort(
      "The following names of {.arg x} are not accepted theme elemets: {.val {not_name}}.",
      call = get_cli_abort_call()
    )
  }

  # print name of theme if present ---------------------------------------------
  if (!is_empty(x$`pkgwide-str:theme_name`)) {
    cli::cli_inform("Setting theme {.val {x$`pkgwide-str:theme_name`}}")
  }

  # adding theme elements to environment ---------------------------------------
  env_bind(.env = env_gtsummary_theme, !!!x)
}

# ------------------------------------------------------------------------------
#' @rdname set_gtsummary_theme
#' @export
reset_gtsummary_theme <- function() {
  set_cli_abort_call()
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
  set_cli_abort_call()
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
  on.exit(suppressMessages(set_gtsummary_theme(current_theme)), add = TRUE)

  # message ignored theme elements ---------------------------------------------
  .msg_ignored_elements(x, current_theme, msg_ignored_elements)

  # add specified theme --------------------------------------------------------
  suppressMessages(set_gtsummary_theme(x))

  # evaluate expression
  eval_tidy({{ expr }}, env = env)
}

.msg_ignored_elements <- function(x, current_theme, msg) {
  # if no message, dont print one!
  if (is.null(msg)) {
    return(invisible())
  }

  # save vector of theme element names that will be ignored
  elements <-
    x |>
    imap(
      function(.x, .y) {
        # if theme element not currently set, return NULL
        if (is_empty(current_theme[[.y]])) {
          return(NULL)
        }
        # if theme element is already set and is different, return the name of the element
        if (!identical(.x, current_theme[[.y]])) {
          return(.y)
        }
        return(NULL)
      }
    ) |>
    compact() |>
    unlist()

  # print message of ignored elements
  if (!is_empty(elements)) {
    cli::cli_inform(msg)
  }

  invisible()
}

# ------------------------------------------------------------------------------
#' @rdname set_gtsummary_theme
#' @export
check_gtsummary_theme <- function(x) {
  set_cli_abort_call()

  # themes must be named lists
  if (!inherits(x, "list") || !is_named(x)) {
    cli::cli_warn("The {.arg x} argument must be a named list.")
    return(invisible())
  }

  # check all names are true theme elements
  else if (!all(names(x) %in% df_theme_elements$name)) {
    not_name <- names(x) %>% setdiff(df_theme_elements$name)
    cli::cli_warn(
      "The following names of {.arg x} are not accepted theme elemets: {.val {not_name}}."
    )
  }

  else cli::cli_alert_success("Looks good!")

  invisible()
}

# ------------------------------------------------------------------------------
# this function grabs a gtsummary theme element if it exists
# otherwise returns the default value
get_theme_element <- function(x, default = NULL, eval = TRUE, env = caller_env()) {
  # returning theme element
  # if eval is FALSE, then returning the un-evaluated theme element
  if (isFALSE(eval)) {
    return(env_gtsummary_theme[[x]] %||% default)
  }

  # the theme element is evaluated in the caller env so it may conditionally
  # set a default depending on other objects only known at the time it is called
  eval_tidy(env_gtsummary_theme[[x]], env = env) %||% default
}

get_deprecated_theme_element <- function(x, default = NULL, eval = TRUE, env = caller_env(), version = "2.0.0") {
  if (!is_empty(env_gtsummary_theme[[x]])) {
    lifecycle::deprecate_warn(
      when = version,
      what = I(glue("gtsummary theme element {shQuote(x)}"))
    )
  }

  get_theme_element(x = x, default = default, eval = eval, env = env)
}

