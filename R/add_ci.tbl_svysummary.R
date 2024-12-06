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
#'   `list(all_continuous() ~ "svymean", all_categorical() ~ "svyprop.logit")`.
#'   See details below.
#' @param df (`numeric`)\cr
#'   denominator degrees of freedom, passed to `survey::svyciprop(df)` or `confint(df)`.
#'   Default is `survey::degf(x$inputs$data)`.
#' @inheritParams add_ci.tbl_summary
#' @inheritParams rlang::args_dots_empty
#'
#' @section method argument:
#'
#' Must be one of
#' - `"svyprop.logit"`, `"svyprop.likelihood"`, `"svyprop.asin"`,
#'     `"svyprop.beta"`, `"svyprop.mean"`, `"svyprop.xlogit"`
#'     calculated via `survey::svyciprop()` for **categorical** variables
#' - `"svymean"` calculated via `survey::svymean()` for **continuous** variables
#' - `"svymedian.mean"`, `"svymedian.beta"`, `"svymedian.xlogit"`,
#'     `"svymedian.asin"`, `"svymedian.score"` calculated via `survey::svyquantile(quantiles = 0.5)` for **continuous** variables
#'
#' @return gtsummary table
#' @export
#' @name add_ci.tbl_svysummary
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("cardx", "survey")) && gtsummary:::is_pkg_installed("broom",  ref = "cardx")
#' data(api, package = "survey")
#' survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
#'   tbl_svysummary(
#'     by = "both",
#'     include = c(api00, stype),
#'     statistic = all_continuous() ~ "{mean} ({sd})"
#'   ) |>
#'   add_stat_label() |>
#'   add_ci(pattern = "{stat} (95% CI {ci})") |>
#'   modify_header(all_stat_cols() ~ "**{level}**") |>
#'   modify_spanning_header(all_stat_cols() ~ "**Survived**")
add_ci.tbl_svysummary <- function(x,
                                  method = list(all_continuous() ~ "svymean", all_categorical() ~ "svyprop.logit"),
                                  include = everything(),
                                  statistic =
                                    list(all_continuous() ~ "{conf.low}, {conf.high}",
                                         all_categorical() ~ "{conf.low}%, {conf.high}%"),
                                  conf.level = 0.95,
                                  style_fun =
                                    list(all_continuous() ~ label_style_sigfig(),
                                         all_categorical() ~ label_style_sigfig(scale =  100)),
                                  pattern = NULL,
                                  df = survey::degf(x$inputs$data),
                                  ...) {
  set_cli_abort_call()
  check_dots_used()
  updated_call_list <- c(x$call_list, list(add_ci = match.call()))

  # check inputs ---------------------------------------------------------------
  check_scalar_range(conf.level, range = c(0, 1))
  check_string(pattern, allow_empty = TRUE)
  if (!"column" %in% x$inputs$percent) {
    cli::cli_inform("The {.fun add_ci} function is meant to work with {.code tbl_svysummary(percent={cli::cli_format('column')})},
                     but {.code tbl_svysummary(percent={cli::cli_format(x$inputs$percent)})} was used.")
  }

  # process inputs -------------------------------------------------------------
  cards::process_selectors(
    data = scope_table_body(x$table_body),
    include = {{ include }}
  )

  cards::process_formula_selectors(
    data = scope_table_body(x$table_body |> dplyr::filter(.data$variable %in% .env$include)),
    method = method,
    statistic = statistic,
    style_fun = style_fun
  )
  cards::fill_formula_selectors(
    data = scope_table_body(x$table_body |> dplyr::filter(.data$variable %in% .env$include)),
    method = eval(formals(asNamespace("gtsummary")[["add_ci.tbl_svysummary"]])[["method"]]),
    statistic = eval(formals(asNamespace("gtsummary")[["add_ci.tbl_svysummary"]])[["statistic"]]),
    style_fun = eval(formals(asNamespace("gtsummary")[["add_ci.tbl_svysummary"]])[["style_fun"]])
  )

  # check statistic and style_fun arg ------------------------------------------
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) {
      is_string(x) && # must be a string
        !is_empty(.extract_glue_elements(x)) && # must contain at least one glue element
        is_empty(setdiff(.extract_glue_elements(x), c("conf.low", "conf.high"))) # glue elements must be conf.low and conf.high only
    },
    error_msg = c(
      "There was an error in the values passed for the {.arg statistic} argument.",
      i = "The value must be a string of length {.val {1}}.",
      i = "The value must contain glue-style elements and only {.val {{conf.low}}} and {.val {{conf.high}}} may be included."
    )
  )
  cards::check_list_elements(
    x = style_fun,
    predicate = \(x) is.function(x),
    error_msg = "The values passed in the {.arg style_fun} argument must be functions."
  )

  # check the method values match the summary types ----------------------------
  methods_continuous <-
    c("svymean", "svymedian", "svymedian.mean", "svymedian.beta", "svymedian.xlogit",
      "svymedian.asin", "svymedian.score")
  methods_categorical <-
    c("svyprop", "svyprop.logit", "svyprop.likelihood", "svyprop.asin",
      "svyprop.beta", "svyprop.mean", "svyprop.xlogit")

  walk(
    include,
    \(variable) {
      if (x$input$type[[variable]] %in% c("continuous", "continuous2") &&
          !method[[variable]] %in% methods_continuous) {
        cli::cli_abort(
          "The value of the {.arg method} argument for continuous variable
           {.val {variable}} must be one of {.val {methods_continuous}}",
          call = get_cli_abort_call()
        )
      }
      else if (x$input$type[[variable]] %in% c("categorical", "dichotomous") &&
               !method[[variable]] %in% methods_categorical) {
        cli::cli_abort(
          'The value of the {.arg method} argument for categorical variable
           {.val {variable}} must be one of {.val {methods_categorical}}',
          call = get_cli_abort_call()
        )
      }
    }
  )

  # check if mean CI selected for summary without a mean -----------------------
  variables_with_mean_ci <- keep(method, ~.x == "svymean") |> names()
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

  # calculate ARD CIs ----------------------------------------------------------
  x$cards$add_ci <-
    .calculate_add_ci_cards_svysummary(data = x$inputs$data, include = include, by = x$inputs$by,
                                       method = method, style_fun = style_fun, conf.level = conf.level,
                                       overall= "add_overall" %in% names(x$call_list), df = df,
                                       value = x$inputs$value)

  # finalize styling of the table ----------------------------------------------
  brdg_add_ci(x, pattern = pattern, statistic = statistic, include = include,
              conf.level = conf.level, updated_call_list = updated_call_list)
}

.calculate_add_ci_cards_svysummary <- function(data, include, by, method, style_fun, conf.level, overall = FALSE, df, value) {
  lst_cards <-
    lapply(
      include,
      FUN = \(v) .calculate_one_ci_ard_svysummary(data = data, variable = v, by = by, method = method, conf.level = conf.level, df = df, value = value)
    ) |>
    set_names(include)

  # if we need to add an overall calculation if `add_overall()` has been previously run
  if (isTRUE(overall)) {
    lst_cards <-
      lapply(
        include,
        FUN = \(v) .calculate_one_ci_ard_svysummary(data = data, variable = v, by = NULL, method = method, conf.level = conf.level, df = df, value = value)
      ) |>
      set_names(include) |>
      append(lst_cards)
  }


  lst_cards |>
    # update the formatting function and apply the function
    imap(
      ~dplyr::mutate(
        .x,
        fmt_fn = ifelse(.data$stat_name %in% c("estimate", "conf.low", "conf.high"),
                        list(style_fun[[.y]]),
                        .data$fmt_fn)
      ) |>
        cards::apply_fmt_fn()
    ) |>
    dplyr::bind_rows() |>
    cards::tidy_ard_column_order()
}

.calculate_one_ci_ard_svysummary <- function(data, variable, by, method, conf.level, df, value) {
  switch(
    method[[variable]],
    # continuous variables
    "svymean" = cardx::ard_continuous_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "svymean", df = df),
    "svymedian.mean" = cardx::ard_continuous_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "svymedian.mean", df = df),
    "svymedian.beta" = cardx::ard_continuous_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "svymedian.beta", df = df),
    "svymedian.xlogit" = cardx::ard_continuous_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "svymedian.xlogit", df = df),
    "svymedian.asin" = cardx::ard_continuous_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "svymedian.asin", df = df),
    "svymedian.score" = cardx::ard_continuous_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "svymedian.score", df = df),

    # categorical variables
    "svyprop.logit" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "logit", df = df, value = value),
    "svyprop.likelihood" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "likelihood", df = df, value = value),
    "svyprop.asin" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "asin", df = df, value = value),
    "svyprop.beta" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "beta", df = df, value = value),
    "svyprop.mean" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "mean", df = df, value = value),
    "svyprop.xlogit" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "xlogit", df = df, value = value),

    # documentation for `"svymedian"` and `"svyprop"` were removed in gtsummary v2.0
    "svymedian" = cardx::ard_continuous_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "svymedian.mean", df = df),
    "svyprop" = cardx::ard_categorical_ci(data, variables = all_of(variable), by = any_of(by), conf.level = conf.level, method = "logit", df = df, value = value)
  )
}
