#' Add CI Column
#'
#' Add a new column with the confidence intervals for proportions, means, etc.
#'
#' @param x (`tbl_summary`)\cr
#'   a summary table of class `'tblsummary'`
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Indicates how the confidence interval will be displayed.
#'   Default is `list(all_continuous() ~ "{conf.low}, {conf.high}", all_categorical() ~ "{conf.low}%, {conf.high}%")`
#' @param method ([`formula-list-selector`][syntax])\cr
#'   Confidence interval method. Default is
#'   `list(all_continuous() ~ "t.test", all_categorical() ~ "wilson")`.
#'   See details below.
#' @param conf.level(scalar `real`)\cr
#'   Confidence level. Default is `0.95`
#' @param style_fun (`function`)\cr
#'   Function to style upper and lower bound of confidence interval. Default is
#'   `list(all_continuous() ~ styfn_sigfig(), all_categorical() ~ styfn_sigfig(scale =  100))`.
#' @param pattern (`string`)\cr
#'   Indicates the pattern to use to merge the CI with
#'   the statistics cell. The default is NULL, where no columns are merged.
#'   The two columns that will be merged are the statistics column,
#'   represented by `"{stat}"` and the CI column represented by `"{ci}"`,
#'   e.g. `pattern = "{stat} ({ci})"` will merge the two columns with the CI
#'   in parentheses. Default is `NULL`, and no merging is performed.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams tbl_summary
#'
#' @section method argument:
#'
#' Must be one of
#' `c("wilson", "wilson.no.correct", "exact", "asymptotic")` for categorical
#' variables, and `c("t.test", "wilcox.test")` for continuous variables.
#'
#' Methods `c("wilson", "wilson.no.correct")` are calculated with
#' `prop.test(correct = c(TRUE, FALSE))`.
#' The default method, `"wilson"`, includes the Yates continuity correction.
#' Methods `c("exact", "asymptotic")` are calculated with `Hmisc::binconf(method=)`.
#'
#' Confidence intervals for means are calculated using `t.test()` and
#' `wilcox.test()` for pseudo-medians.
#'
#' @return gtsummary table
#' @name add_ci
#'
#' @examples
#' # Example 1 ----------------------------------
#' add_ci_ex1 <-
#'   trial %>%
#'   select(marker, response, trt) %>%
#'   tbl_summary(
#'     missing = "no",
#'     statistic = all_continuous() ~ "{mean} ({sd})"
#'   ) %>%
#'   add_ci()
#'
#' # Example 2 ----------------------------------
#' add_ci_ex2 <-
#'   trial %>%
#'   select(response, grade) %>%
#'   tbl_summary(
#'     statistic = all_categorical() ~ "{p}%",
#'     missing = "no"
#'   ) %>%
#'   add_ci(pattern = "{stat} ({ci})") %>%
#'   modify_footnote(everything() ~ NA)
NULL

#' @rdname add_ci
#' @export
add_ci <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_ci")
}

#' @rdname add_ci
#' @export
add_ci.tbl_summary <- function(x,
                               method = list(all_continuous() ~ "t.test", all_categorical() ~ "wilson"),
                               include = everything(),
                               statistic =
                                 list(all_continuous() ~ "{conf.low}, {conf.high}",
                                      all_categorical() ~ "{conf.low}%, {conf.high}%"),
                               conf.level = 0.95,
                               style_fun =
                                 list(all_continuous() ~ styfn_sigfig(),
                                      all_categorical() ~ styfn_sigfig(scale =  100)),
                               pattern = NULL,
                               ...) {
  set_cli_abort_call()
  check_dots_used()

  # check inputs ---------------------------------------------------------------
  check_scalar_range(conf.level, range = c(0, 1))
  check_string(pattern, allow_empty = TRUE)
  if (!is_empty(pattern)) {
    if (!rlang::is_empty(.extract_glue_elements(pattern) |> setdiff(c("stat", "ci")))) {
      cli::cli_abort(
        "The {.arg pattern} argument allows only for elements {.val {c('stat', 'ci')}} to be included in curly brackets.",
        call = get_cli_abort_call()
      )
    }
    if (!setequal(.extract_glue_elements(pattern), c("stat", "ci"))) {
      cli::cli_abort(
        "The {.arg pattern} argument must include references to both {.val {c('{stat}', '{ci}')}}",
        call = get_cli_abort_call()
      )
    }
  }

  # process inputs -------------------------------------------------------------
  cards::process_selectors(
    x = select_prep(x$table_body),
    include = {{ include }}
  )

  cards::process_formula_selectors(
    x = select_prep(x$table_body |> filter(.data$variable %in% .env$include)),
    method = method,
    statistic = statistic,
    style_fun = style_fun
  )

  # check if mean CI selected for summary without a mean -----------------------
  variables_with_mean_ci <- keep(method, ~.x == "t.test") |> names()
  walk(
    variables_with_mean_ci,
    function(variable) {
      if (!"mean" %in% .extract_glue_elements(x[["inputs"]][["statistic"]][[variable]])) {
        cli::cli_inform(
          "A confidence interval for the mean in variable {.val {variable}} was
           requested; however, the primary table does not contain a mean."
        )
      }
    }
  )
}
