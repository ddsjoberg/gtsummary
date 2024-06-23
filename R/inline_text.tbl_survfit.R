#' Report statistics from survfit tables inline
#'
#' \lifecycle{maturing}\cr
#' Extracts and returns statistics from a `tbl_survfit` object for
#' inline reporting in an R markdown document. Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#'
#' @inheritParams inline_text.tbl_summary
#' @param x (`tbl_survfit`)\cr
#'   Object created from  [`tbl_survfit()`]
#' @param time,prob (`numeric` scalar)\cr
#'   time or probability for which to return result
#' @param column ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column to print from `x$table_body`.
#'   Columns may be selected with `time` or `prob` arguments as well.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variable name of statistic to present.
#' @param pattern (`string`)\cr
#'   String indicating the statistics to return.
#' @param level (`string`)\cr
#'   Level of the variable to display for categorical variables.
#'   Can also specify the 'Unknown' row.  Default is `NULL`
#' @param estimate_fun (`function`)\cr
#'   Function to round and format estimate and confidence limits.
#'   Default is the same function used in `tbl_survfit()`
#'
#' @author Daniel D. Sjoberg
#' @export
#' @return A string reporting results from a gtsummary table
#'
#' @examplesIf gtsummary:::is_pkg_installed("survival", reference_pkg = "gtsummary")
#' library(survival)
#'
#' # fit survfit
#' fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
#' fit2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
#'
#' # sumarize survfit objects
#' tbl1 <-
#'   tbl_survfit(
#'     fit1,
#'     times = c(12, 24),
#'     label = ~"Treatment",
#'     label_header = "**{time} Month**"
#'   ) %>%
#'   add_p()
#'
#' tbl2 <-
#'   tbl_survfit(
#'     fit2,
#'     probs = 0.5,
#'     label_header = "**Median Survival**"
#'   )
#'
#' # report results inline
#' inline_text(tbl1, time = 24, level = "Drug B")
#' inline_text(tbl1, time = 24, level = "Drug B",
#'             pattern = "{estimate} [95% CI {conf.low}, {conf.high}]")
#' inline_text(tbl1, column = p.value)
#' inline_text(tbl2, prob = 0.5)
inline_text.tbl_survfit <- function(x,
                                    variable = NULL,
                                    level = NULL,
                                    pattern = NULL,
                                    time = NULL, prob = NULL, column = NULL,
                                    estimate_fun = x$inputs$estimate_fun,
                                    pvalue_fun = label_style_pvalue(prepend_p = TRUE), ...) {
  set_cli_abort_call()
  check_dots_empty()

  # quoting inputs -------------------------------------------------------------
  variable <- rlang::enquo(variable)
  column <- rlang::enquo(column)

  # setting defaults ---------------------------------------------------------
  pvalue_fun <-
    case_switch(
      missing(pvalue_fun) ~ get_theme_element("pkgwide-fn:prependpvalue_fun", default = pvalue_fun),
      .default = pvalue_fun
    )
  pvalue_fun <- as_function(pvalue_fun)
  estimate_fun <- as_function(estimate_fun)

  # applying formatting functions --------------------------------------------
  x <- x |>
    modify_fmt_fun(
      any_of(c("p.value", "q.value")) ~ pvalue_fun,
      any_of(c("estimate", "conf.high", "conf.low")) ~ estimate_fun
    )

  if (!is_empty(pattern) && any(c("p.value", "q.value", "estimate", "conf.high", "conf.low") %in% .extract_glue_elements(pattern))) {
    lst_fmt_fn_updates <-
      c(rep_named(c("p.value", "q.value"), list(pvalue_fun)),
        rep_named(c("estimate", "conf.high", "conf.low"), list(estimate_fun)))
    x$cards[[1]] <- .update_fmt_fn(x$cards[[1]], lst_fmt_fn_updates)
    x$cards <- .update_fmt_fn(x$cards, lst_fmt_fn_updates)
  }

  # checking inputs ----------------------------------------------------------
  if (c(!is_empty(time), !is_empty(prob), !is_quo_empty(column)) |> sum() != 1) {
    cli::cli_abort(
      "Specify one and only one of {.arg time}, {.arg prob}, and {.arg column} arguments.",
      call = get_cli_abort_call()
    )
  }

  # selecting column ---------------------------------------------------------
  if (!is_empty(time)) {
    if (!time %in% x$inputs$times) {
      cli::cli_abort(
        "The {.arg time} argument must be one of {.val {x$inputs$times}}.",
        call = get_cli_abort_call()
      )
    }
    column <- paste0("stat_", which(x$inputs$times %in% time))
  }
  if (!is_empty(prob)) {
    if (!prob %in% x$inputs$probs) {
      cli::cli_abort(
        "The {.arg prob} argument must be one of {.val {x$inputs$probs}}.",
        call = get_cli_abort_call()
      )
    }
    column <- paste0("stat_", which(x$inputs$probs %in% prob))
  }
  cards::process_selectors(
    data = x$table_body,
    column = {{ column }}
  )

  # select variable
  if (is_quo_empty(variable)) variable <- x$table_body$variable[1]

  # convert internal cards object to structure like tbl_sumary -----------------
  x$cards[[1]] <-
    x$cards[[1]] |>
    imap(
      \(card, variable) {
        # if no stratifying variables
        if (card |> dplyr::select(cards::all_ard_groups()) |> names() |> is_empty()) {
          card <- card |>
            dplyr::mutate(variable = .env$variable) |>
            dplyr::select(-"variable_level")
        }
        else {
          card <- card |>
            dplyr::mutate(
              variable = .data$group1,
              variable_level = .data$group1_level
            ) |>
            dplyr::select(-cards::all_ard_groups())
        }
      }
    ) |>
    dplyr::bind_rows()

  # call generic inline_text() function ----------------------------------------
  inline_text.gtsummary(
    x = x,
    variable = !!variable,
    level = {{ level }},
    column = all_of(column),
    pattern = pattern
  )
}


