#' Add model statistics
#'
#' Add model statistics returned from `broom::glance()`. Statistics can either
#' be appended to the table (`add_glance_table()`), or added as a
#' table source note (`add_glance_source_note()`).
#'
#' @param x (`tbl_regression`)\cr
#'   a `'tbl_regression'` object
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   names of statistics to include in output. Must be column
#'   names of the tibble returned by `broom::glance()` or from the `glance_fun`
#'   argument. The include argument
#'   can also be used to specify the order the statistics appear in the table.
#' @param label ([`formula-list-selector`][syntax])\cr
#'   specifies statistic labels, e.g. `list(r.squared = "R2", p.value = "P")`
#' @param fmt_fun ([`formula-list-selector`][syntax])\cr
#'   Specifies the the functions used to format/round the glance statistics.
#'   The default is to round
#'   the number of observations and degrees of freedom to the nearest integer,
#'   p-values are styled with `style_pvalue()` and the remaining statistics
#'   are styled with `style_sigfig(x, digits = 3)`
#' @param glance_fun function that returns model statistics. Default is
#' `broom::glance()` for most model obejcts, and
#' `broom::glance(mice::pool())` for MICE 'mira' models.
#' Custom functions must return a single row tibble.
#' @param sep1 Separator between statistic name and statistic.
#' Default is `" = "`, e.g. `"R2 = 0.456"`
#' @param sep2 Separator between statistics. Default is `"; "`
#' @param text_interpret String indicates whether source note text
#' will be interpreted with
#' [gt::md()] or [gt::html()]. Must be `"md"` (default) or `"html"`.
#'
#' @return gtsummary table
#' @name add_glance
#'
#' @section Tips:
#' When combining `add_glance_table()` with `tbl_merge()`, the
#' ordering of the model terms and the glance statistics may become jumbled.
#' To re-order the rows with glance statistics on bottom, use the script below:
#'
#' ```r
#' tbl_merge(list(tbl1, tbl2)) %>%
#'   modify_table_body(~.x %>% arrange(row_type == "glance_statistic"))
#' ````
#'
#' @examples
#' mod <- lm(age ~ marker + grade, trial) |> tbl_regression()
#'
#' # Example 1 ----------------------------------
#' mod |>
#'   add_glance_table(
#'     label = list(sigma = "\U03C3"),
#'     include = c(r.squared, AIC, sigma)
#'   )
#'
#' # Example 2 ----------------------------------
#' mod |>
#'   add_glance_source_note(
#'     label = list(sigma = "\U03C3"),
#'     include = c(r.squared, AIC, sigma)
#'   )
NULL

#' @export
#' @rdname add_glance
add_glance_table <- function(x,
                             include = everything(),
                             label = NULL,
                             fmt_fun =
                               list(
                                 everything() ~ label_style_sigfig(digits = 3),
                                 any_of("p.value") ~ label_style_pvalue(digits = 1),
                                 c(where(is.integer), starts_with("df")) ~ label_style_number()
                               ),
                             glance_fun = broom::glance) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(add_glance_table = match.call()))
  check_pkg_installed("broom", reference_pkg = "gtsummary")
  check_not_missing(x)
  check_class(x, "tbl_regression")

  # use a modified glance for mice models --------------------------------------
  if (missing(glance_fun) && inherits(x$inputs$x, "mice")) {
    check_pkg_installed("mice", reference_pkg = "gtsummary")
    glance_fun <- \(x) broom::glance(mice::pool(x))
  }

  # calculate and prepare the glance function results --------------------------
  lst_prep_glance <-
    .prep_glance_statistics(
      x = x,
      include = {{ include }},
      label = label,
      fmt_fun = fmt_fun,
      glance_fun = glance_fun
    )

  # add instructions to print horizontal line ----------------------------------
  x$table_styling$horizontal_line_above <-
    expr(.data$row_type == "glance_statistic" & .data$variable %in% !!lst_prep_glance$df_glance$variable[1])

  # updating regression gtsummary object ---------------------------------------
  # appending stats to table_body
  x$table_body <-
    x$table_body %>%
    dplyr::bind_rows(lst_prep_glance$df_glance)

  # apply formatting functions
  for (fun in unique(lst_prep_glance$fmt_fun)) {
    x <- x |>
      modify_fmt_fun(
        estimate = fun,
        rows = .data$row_type == "glance_statistic" & .data$variable %in% !!names(keep(lst_prep_glance$fmt_fun, ~identical(.x, fun)))
      )
  }

  # returning gtsummary table --------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname add_glance
add_glance_source_note <- function(x,
                                   include = everything(),
                                   label = NULL,
                                   fmt_fun =
                                     list(
                                       everything() ~ label_style_sigfig(digits = 3),
                                       any_of("p.value") ~ label_style_pvalue(digits = 1),
                                       c(where(is.integer), starts_with("df")) ~ label_style_number()
                                     ),
                                   glance_fun = broom::glance,
                                   text_interpret = c("md", "html"),
                                   sep1 = " = ", sep2 = "; ") {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(add_glance_source_note = match.call()))
  check_pkg_installed("broom", reference_pkg = "gtsummary")
  check_not_missing(x)
  check_class(x, "tbl_regression")
  text_interpret <- arg_match(text_interpret)
  check_string(sep1)
  check_string(sep2)

  # use a modified glance for mice models --------------------------------------
  if (missing(glance_fun) && inherits(x$inputs$x, "mice")) {
    check_pkg_installed("mice", reference_pkg = "gtsummary")
    glance_fun <- \(x) broom::glance(mice::pool(x))
  }

  # calculate and prepare the glance function results --------------------------
  lst_prep_glance <-
    .prep_glance_statistics(
      x = x,
      include = {{ include }},
      label = label,
      fmt_fun = fmt_fun,
      glance_fun = glance_fun
    )

  # apply formatting function to glance statistics -----------------------------
  lst_prep_glance$df_glance$estimate_fmt <- NA_character_

  for (fun in unique(lst_prep_glance$fmt_fun)) {
    idx <- lst_prep_glance$df_glance$variable %in% names(keep(lst_prep_glance$fmt_fun, ~identical(.x, fun)))
    lst_prep_glance$df_glance$estimate_fmt[idx] <-
      fun(lst_prep_glance$df_glance$estimate[idx])
  }

  # compile stats into source note ---------------------------------------------
  x$table_styling$source_note <-
    paste(lst_prep_glance$df_glance$label, lst_prep_glance$df_glance$estimate_fmt, sep = sep1, collapse = sep2)
  attr(x$table_styling$source_note, "text_interpret") <- match.arg(text_interpret)

  # returning gtsummary table --------------------------------------------------
  x$call_list <- updated_call_list
  x
}


.prep_glance_statistics <- function(x, include, label, fmt_fun, glance_fun) {
  # checking inputs ------------------------------------------------------------
  glance_fun <- as_function(glance_fun)

  # prepping glance table ------------------------------------------------------
  df_glance_orig <- glance_fun(x$inputs$x)
  cards::process_selectors(df_glance_orig, include = {{ include }})
  df_glance_orig <- df_glance_orig[include]

  # adding user-specified labels -----------------------------------------------
  cards::process_formula_selectors(
    df_glance_orig,
    label = label,
    fmt_fun = fmt_fun
  )
  cards::check_list_elements(
    x = label,
    predicate = \(x) is_string(x),
    error_msg = "Elements of {.arg label} argument must be strings."
  )
  cards::check_list_elements(
    x = fmt_fun,
    predicate = \(x) is.function(x),
    error_msg = "Elements of {.arg fmt_fun} argument must be functions."
  )
  cards::fill_formula_selectors(
    data = df_glance_orig,
    fmt_fun = formals(gtsummary::add_glance_table)[["fmt_fn"]] |> eval()
  )

  df_label <-
    case_switch(
      !is_empty(label) ~ enframe(unlist(label), "variable", "label"),
      .default = dplyr::tibble(variable = character(), label = character())
    )

  # prepping data frame to be appended to `x$table_body` -----------------------
  df_glance <-
    df_glance_orig |>
    dplyr::mutate_all(list) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "estimate",
      values_transform = list(estimate = unlist)
    ) |>
    # adding default labels
    dplyr::left_join(df_default_glance_labels(), by = c("variable" = "statistic_name")) %>%
    dplyr::mutate(
      label =
        dplyr::coalesce(.data$label, .data$variable) |>
        translate_vector()
    ) |>
    # updating table with user-specified labels
    dplyr::rows_update(df_label, by = "variable", unmatched = "ignore") |>
    mutate(
      row_type = "glance_statistic",
      var_label = .data$label
    )

  # return objects needed to finalize glance stats
  list(df_glance = df_glance, fmt_fun = fmt_fun)
}

# default statistic labels
df_default_glance_labels <- function() {
  dplyr::tribble(
    ~statistic_name, ~label,
    "r.squared", "R\U00B2",
    "adj.r.squared", "Adjusted R\U00B2",
    "p.value", "p-value",
    "logLik", "Log-likelihood",
    "statistic", "Statistic",
    "df.residual", "Residual df",
    "null.deviance", "Null deviance",
    "df.null", "Null df",
    "nevent", "N events",
    "concordance", "c-index",
    "std.error.concordance", "c-index SE",
    "nobs", "No. Obs.",
    "deviance", "Deviance",
    "sigma", "Sigma"
  )
}



