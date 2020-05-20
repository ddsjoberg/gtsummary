#' Set a gtsummary theme
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
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
#' @section Themes:
#' Review the [themes vignette](http://www.danieldsjoberg.com/gtsummary/dev/articles/rmarkdown.html)
#' to create your own themes.
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
  if (!is.null(x$`pkgwide-str:theme_name`))
    rlang::inform(glue("Setting `{x$`pkgwide-str:theme_name`}` theme"))

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
  if (eval == FALSE)
    return(env_gtsummary_theme[[x]] %||% default)

  # the theme element is evaluated in the caller env so it may conditionally
  # set a default depending on other objects only known at the time it is called
  rlang::eval_tidy(env_gtsummary_theme[[x]], env = rlang::caller_env()) %||% default
}

# ------------------------------------------------------------------------------
#' @name set_gtsummary_theme
#' @export
reset_gtsummary_theme <- function() {
  # deleting theme environment if it exists
  rm(list = ls(envir = env_gtsummary_theme),
     envir = env_gtsummary_theme)

  invisible()
}

# tibble of all possible theme options
# THIS DATA FRAME IS SAVED IN "vignettes/data/gtsummary_theme_elements.csv"
# OPEN THE FILE, COPY THE CELLS, USE THE datapasta PACKAGE TO PASTE AS TRIBBLE HERE
df_theme_elements <-
tibble::tribble(
                       ~fn,                                               ~name, ~argument,                                                                                                                                                                                                               ~desc,                                                                                                                                                   ~example,
            "Package-wide",                            "pkgwide-str:theme_name",     FALSE,                                                                                                                                                                                            "optional name of theme",                                                                                                                                    "\"My Personal Theme\"",
            "Package-wide",                          "pkgwide-str:print_engine",     FALSE,                                                                                                                                                                        "string indicating the default print engine",                                                                                                                                                "\"kable\"",
            "Package-wide",                             "pkgwide-fn:pvalue_fun",     FALSE,                                                                                                                                                                     "function to style p-values throughout package",                                                                                                                  "function(x) style_pvalue(x, digits = 2)",
            "Package-wide",                      "pkgwide-fn:prependpvalue_fun",     FALSE,                                                                                                         "function to style p-values throughout package that include a \"p\" prefix, e.g. \"p<0.001\" or \"p=0.12\"",                                                                                                "function(x) style_pvalue(x, digits = 2, prepend_p = TRUE)",
            "Package-wide",                                 "pkgwide-lgl:quiet",     FALSE,                                                                                                                                                            "logical indicating whether to suppress messages or not",                                                                                                                                                         NA,
                   "as_gt",                               "as_gt-lst:addl_cmds",     FALSE,               "list expression of gt-package commands inserted in the `as_gt()` call. Do not include the `data=` argument. Expression is inserted after the named call, e.g. after \"tab_spanner\" in the example.",                                                                              "list(tab_spanner = rlang::expr(gt::tab_options(table.font.size = 'small')))",
  "as_flextable.gtsummary",              "as_flextable.gtsummary-lst:addl_cmds",     FALSE, "list of expressions of flextable-package commands inserted in the `as_flextable()` call. Do not include the `data=` argument. Expression is inserted after the named call, e.g. after \"autofit\" in the example.", "list(autofit = list(rlang::expr(flextable::font(fontname = \"Bodoni 72\", part = \"all\")), rlang::expr(flextable::fontsize(size = 8, part = \"all\"))))",
   "as_huxtable.gtsummary",               "as_huxtable.gtsummary-lst:addl_cmds",     FALSE,                                              "list expression of huxtable-package commands inserted in the `as_huxtable()` call. Do not include the `data=` argument. Expression is inserted after the named call.",                                                                                                                                                         NA,
          "as_kable_extra",                      "as_kable_extra-lst:addl_cmds",     FALSE,                                         "list expression of kableExtra-package commands inserted in the `as_kable_extra()` call. Do not include the `data=` argument. Expression is inserted after the named call.",                                                                                                                                                         NA,
             "tbl_summary",                             "tbl_summary-arg:label",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_summary",                         "tbl_summary-arg:statistic",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_summary",                            "tbl_summary-arg:digits",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_summary",                              "tbl_summary-arg:type",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_summary",                             "tbl_summary-arg:value",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_summary",                           "tbl_summary-arg:missing",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_summary",                      "tbl_summary-arg:missing_text",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_summary",                           "tbl_summary-arg:percent",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_summary",                              "tbl_summary-arg:sort",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_summary",                        "tbl_summary-fn:percent_fun",     FALSE,                                                                                                                                                                                     "function to style percentages",                                                                                                                             "function(x) style_percent(x)",
             "tbl_summary",                              "tbl_summary-fn:N_fun",     FALSE,                                                                                                                                                                                       "function to style intergers",                                                                                                                         "function(x) sprintf(\"%.0f\", x)",
             "tbl_summary",                   "tbl_summary-str:continuous_stat",     FALSE,                                                                                                                                         "glue string defining the default continuous summary statistics to display",                                                                                                                                        "\"{mean} ({sd})\"",
             "tbl_summary",                  "tbl_summary-str:categorical_stat",     FALSE,                                                                                                                        "glue string defining the default categorical and dichotomous summary statistics to display",                                                                                                                                     "\"{n} / {N} ({p}%)\"",
       "add_p.tbl_summary",                        "add_p.tbl_summary-arg:test",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
       "add_p.tbl_summary",                  "add_p.tbl_summary-arg:pvalue_fun",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
       "add_p.tbl_summary",        "add_p.tbl_summary-attr:test.continuous_by2",     FALSE,                                                                                                                                                  "default test for continuous variables with a 2-level by variable",                                                                                                                                               "\"t.test\"",
       "add_p.tbl_summary",            "add_p.tbl_summary-attr:test.continuous",     FALSE,                                                                                                                                         "default test for continuous variables with a 3- or more level by variable",                                                                                                                                                  "\"aov\"",
       "add_p.tbl_summary",           "add_p.tbl_summary-attr:test.categorical",     FALSE,                                                                                                                                                                "default test for categorical/dichotomous variables",                                                                                                                                           "\"chisq.test\"",
       "add_p.tbl_summary", "add_p.tbl_summary-attr:test.categorical.low_count",     FALSE,                                                                                                                                 "default test for categorical/dichotomous variables with minimum expected count <5",                                                                                                                                          "\"fisher.test\"",
       "add_p.tbl_summary", "add_p.tbl_summary-attr:test.categorical.group_by2",     FALSE,                                                                                                                  "default test for categorical/dichotomous grouped/correlated variables with a 2-level by variable",                                                                                                                                                 "\"lme4\"",
       "add_p.tbl_summary",  "add_p.tbl_summary-attr:test.continuous.group_by2",     FALSE,                                                                                                                               "default test for continuous grouped/correlated variables with a 2-level by variable",                                                                                                                                                 "\"lme4\"",
          "add_stat_label",                       "add_stat_label-arg:location",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
                   "add_q",                                  "add_q-arg:method",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
                   "add_q",                              "add_q-arg:pvalue_fun",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
         "add_p.tbl_cross",                          "add_p.tbl_cross-arg:test",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
         "add_p.tbl_cross",                    "add_p.tbl_cross-arg:pvalue_fun",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
             "tbl_survfit",                         "tbl_survfit-arg:statistic",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
          "tbl_regression",                     "tbl_regression-arg:conf.level",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
          "tbl_regression",                   "tbl_regression-arg:estimate_fun",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
          "tbl_regression",                     "tbl_regression-arg:pvalue_fun",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
          "tbl_regression",                       "tbl_regression-arg:tidy_fun",      TRUE,                                                                                                                                                                                                                  NA,                                                                                                                                                         NA,
          "tbl_regression",                    "tbl_regression-str:coef_header",     FALSE,                                                                                                                                            "String setting the default term for the beta coefficient column header",                                                                                                    "ifelse(exponentiate == TRUE, \"exp(coef)\", \"coef\")"
  )
