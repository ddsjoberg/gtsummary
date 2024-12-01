#' Add the global p-values
#'
#' This function uses `car::Anova()` (by default) to calculate global p-values
#' for model covariates.
#' Output from `tbl_regression` and `tbl_uvregression` objects supported.
#'
#' @param x (`tbl_regression`, `tbl_uvregression`)\cr
#'   Object with class `'tbl_regression'` or `'tbl_uvregression'`
#' @param keep (scalar `logical`)\cr
#'   Logical argument indicating whether to also retain the individual
#'   p-values in the table output for each level of the categorical variable.
#'   Default is `FALSE`.
#' @param anova_fun (`function`)\cr
#'   Function used to calculate global p-values.
#'   Default is generic [`global_pvalue_fun()`], which wraps `car::Anova()` for
#'   most models. The `type` argument is passed to this function. See help file for details.
#'
#'   To pass a custom function, it must accept as its first argument is a model.
#'   Note that anything passed in `...` will be passed to this function.
#'   The function must return an object of class `'cards'` (see `cardx::ard_car_anova()` as an example),
#'   or a tibble with columns `'term'` and `'p.value'` (e.g. `\(x, type, ...) car::Anova(x, type, ...) |> broom::tidy()`).
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to calculate global p-value for. Default is `everything()`
#' @param quiet `r lifecycle::badge("deprecated")`
#' @param type Type argument passed to `anova_fun`. Default is `"III"`
#' @param ... Additional arguments to be passed to `car::Anova`,
#' `aod::wald.test()` or `anova_fun` (if specified)
#'
#' @author Daniel D. Sjoberg
#' @name add_global_p
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("cardx", "broom", "car", "parameters"))
#' # Example 1 ----------------------------------
#' lm(marker ~ age + grade, trial) |>
#'   tbl_regression() |>
#'   add_global_p()
#'
#' # Example 2 ----------------------------------
#' trial[c("response", "age", "trt", "grade")] |>
#'   tbl_uvregression(
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) |>
#'   add_global_p()
NULL

#' @rdname add_global_p
#' @export
add_global_p <- function(x, ...) {
  check_class(x, "gtsummary")
  UseMethod("add_global_p")
}

#' @rdname add_global_p
#' @export
add_global_p.tbl_regression <- function(x,
                                        include = everything(),
                                        keep = FALSE,
                                        anova_fun = global_pvalue_fun,
                                        type = "III",
                                        quiet,
                                        ...) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(add_global_p = match.call()))

  # deprecation ----------------------------------------------------------------
  if (!missing(quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::add_global_p(quiet)"
    )
  }
  if (inherits(x, "tbl_uvregression") && !is_empty(x$inputs$x)) {
    cli::cli_abort(
      "The {.fun add_global_p} does not support {.code tbl_uvregression(x=)} objects.",
      call = get_cli_abort_call()
    )
  }

  # process inputs -------------------------------------------------------------
  check_scalar_logical(keep)
  check_class(anova_fun, cls = "function")
  if (missing(type)) {
    type <- get_theme_element("add_global_p-str:type", default = type)
  }
  cards::process_selectors(
    data = scope_table_body(x$table_body),
    include = {{ include }}
  )
  if (is_empty(include)) return(x) # styler: off

  # calculating global p-values ------------------------------------------------
  global_p_results <- .calculate_anova_fun(x, include, anova_fun, type, ...)
  x$cards$add_global_p <- global_p_results

  # process results ------------------------------------------------------------
  global_p_results <-
    .process_global_p_results(
      global_p_results,
      include = include,
      class = intersect(class(x), c("tbl_regression", "tbl_uvregression"))
    )

  # merging in global p-value --------------------------------------------------
  x <- .merge_global_p_results(x, global_p_results = global_p_results, keep = keep)

  # return results -------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @rdname add_global_p
#' @export
add_global_p.tbl_uvregression <- add_global_p.tbl_regression

.calculate_anova_fun <- function(x, include, anova_fun, type, ...) {
  if (inherits(x, "tbl_regression")) {
    global_p_results <-
      cards::eval_capture_conditions(
        case_switch(
          identical(anova_fun, global_pvalue_fun) ~ do.call(anova_fun, list(x$inputs$x, type = type, ...)),
          .default = do.call(anova_fun, list(x$inputs$x, ...))
        )
      )
    if (!is_empty(global_p_results[["error"]])) {
      cli::cli_abort(
        c("There was an error running {.code anova_fun}. See message below.",
          "x" = global_p_results[["error"]]),
        call = get_cli_abort_call()
      )
    }

    return(global_p_results[["result"]])
  }

  # calculate results for `tbl_uvregression` tables
  imap(
    x$tbls[include],
    function(tbl, variable) {
      global_p_results <-
        cards::eval_capture_conditions(
          case_switch(
            identical(anova_fun, global_pvalue_fun) ~ do.call(anova_fun, list(tbl$inputs$x, type = type, ...)),
            .default = do.call(anova_fun, list(tbl$inputs$x, ...))
          )
        )

      if (!is_empty(global_p_results[["error"]])) {
        cli::cli_abort(
          c("There was an error running {.code anova_fun} for variable {.val {variable}}. See message below.",
            "x" = global_p_results[["error"]]),
          call = get_cli_abort_call()
        )
      }

      global_p_results[["result"]]
    }
  )
}


.merge_global_p_results <- function(x, global_p_results, keep) {
  # adding p-value column, if it is not already there
  if (!"p.value" %in% names(x$table_body)) {
    x <- x |>
      modify_table_body(~dplyr::mutate(.x, p.value = NA_real_)) |>
      modify_table_styling(
        columns = "p.value",
        label = "**p-value**",
        hide = FALSE,
        fmt_fun = x$inputs$pvalue_fun
      )
  }

  # adding global p-values
  x <- x |>
    modify_table_body(
      ~ dplyr::left_join(
        .x,
        global_p_results,
        by = c("row_type", "variable")
      ) |>
        dplyr::mutate(
          p.value = dplyr::coalesce(.data$p.value_global, .data$p.value)
        ) |>
        dplyr::select(-c("p.value_global"))
    )

  # if keep == FALSE, then deleting variable-level p-values
  if (isFALSE(keep)) {
    x$table_body <-
      x$table_body %>%
      dplyr::mutate(
        p.value = ifelse(.data$row_type == "level", NA_real_, .data$p.value)
      )
  }
  x
}

.process_global_p_results <- function(x, include, class) {
  if (class == "tbl_regression") {
    if (inherits(x, "card")) {
      x <- x |>
        dplyr::filter(.data$stat_name %in% "p.value") |>
        tidyr::pivot_wider(
          id_cols = "variable",
          names_from = "stat_name",
          values_from = "stat",
          values_fn = unlist
        )
    }
    else if (is.data.frame(x) && all(c("term", "p.value") %in% names(x))) {
      x <- x |>
        dplyr::select(variable = "term", "p.value")
    } else {
      cli::cli_abort(
        c("The returned result from {.code anova_fun} is not the expected structure.",
          i = "See {.help add_global_p} for details.")
      )
    }
    # final processing
    return(
      x  |>
        dplyr::filter(.data$variable %in% .env$include) |>
        dplyr::mutate(row_type = "label") |>
        dplyr::rename(p.value_global = "p.value")
    )
  }


  # now do the same for tbl_uvregression models
  imap(
    x,
    function(x, variable) {
      if (inherits(x, "card")) {
        x <- x |>
          dplyr::filter(.data$stat_name %in% "p.value") |>
          tidyr::pivot_wider(
            id_cols = "variable",
            names_from = "stat_name",
            values_from = "stat",
            values_fn = unlist
          )
      }
      else if (is.data.frame(x) && all(c("term", "p.value") %in% names(x))) {
        x <- x |>
          dplyr::select(variable = "term", "p.value")
      } else {
        cli::cli_abort(
          c("The returned result from {.code anova_fun} is not the expected structure.",
            i = "See {.help add_global_p} for details.")
        )
      }

      # final processing
      x  |>
        dplyr::filter(.data$variable %in% .env$variable) |>
        dplyr::mutate(row_type = "label") |>
        dplyr::rename(p.value_global = "p.value")
    }
  ) |>
    dplyr::bind_rows()
}

