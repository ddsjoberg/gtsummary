#' Add Model Statistics
#'
#' Add model statistics returned from `broom::glance()`. Statistics can either
#' be appended to the table (`add_glance_table()`), or added as a
#' table source note (`add_glance_source_note()`).
#'
#' @param x 'tbl_regression' object
#' @param include list of statistics to include in output. Must be column
#' names of the tibble returned by `broom::glance()`. The include argument
#' can also be used to specify the order the statistics appear in the table.
#' @param label List of formulas specifying statistic labels,
#' e.g. `list(r.squared ~ "R2", p.value ~ "P")`
#' @param fmt_fun List of formulas where the LHS is a statistic and the RHS
#' is a function to format/round the statistics. The default is to round
#' the number of observations and degrees of freedom to the nearest integer,
#' p-values are styled with `style_pvalue()` and the remaining statistics
#' are styled with `style_sigfig(x, digits = 3)`
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
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @name add_glance
#'
#' @section Default Labels:
#' The following statistics have set default labels when printed.
#' When there is no default, the column name from `broom::glance()` is printed.
#'
#' ```{r, echo = FALSE}
#' df_default_glance_labels %>%
#'   select(`Statistic Name` = statistic_name, `Default Label` = label) %>%
#'   knitr::kable()
#' ```
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
#' \donttest{
#' mod <- lm(age ~ marker + grade, trial) %>% tbl_regression()
#'
#' # Example 1 ----------------------------------
#' add_glance_ex1 <-
#'   mod %>%
#'   add_glance_table(
#'     label = list(sigma ~ "\U03C3"),
#'     include = c(r.squared, AIC, sigma)
#'   )
#'
#' # Example 2 ----------------------------------
#' add_glance_ex2 <-
#'   mod %>%
#'   add_glance_source_note(
#'     label = list(sigma ~ "\U03C3"),
#'     include = c(r.squared, AIC, sigma)
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_glance_ex1.png", width = "35")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_glance_ex2.png", width = "35")`
#' }}
NULL

#' @export
#' @rdname add_glance
add_glance_table <- function(x, include = everything(), label = NULL,
                             fmt_fun = NULL, glance_fun = NULL) {
  updated_call_list <- c(x$call_list, list(add_glance_table = match.call()))
  .assert_class(x, c("tbl_regression"))

  # default glance() function --------------------------------------------------
  glance_fun <- glance_fun %||% .default_glance_fun(x)

  # prepare glance statistics and formatting functions -------------------------
  lst_prep_glance <-
    .prep_glance_statistics(
      x = x, include = {{ include }},
      label = label, fmt_fun = fmt_fun,
      glance_fun = glance_fun
    )

  # add instructions to print horizontal line ----------------------------------
  x$table_styling$horizontal_line_above <-
    expr(.data$row_type == "glance_statistic" & .data$variable %in% !!lst_prep_glance$df_glance$variable[1])

  # updating regression gtsummary object ---------------------------------------
  # appending stats to table_body
  x$table_body <-
    x$table_body %>%
    bind_rows(lst_prep_glance$df_glance)

  # creating modify_fmt_fun calls, and running them
  df_modify_fmt_fun <-
    lst_prep_glance$df_fmt_fun %>%
    nest(glance_statistic = "glance_statistic") %>%
    rowwise() %>%
    mutate(
      glance_statistic = unlist(.data$glance_statistic) %>% unname() %>% list(),
    )

  # evaluating modify_fmt_fun calls on gtsumary object
  x <-
    map2(
      df_modify_fmt_fun$fmt_fun, df_modify_fmt_fun$glance_statistic,
      ~ expr(
        modify_fmt_fun(
          update = list(estimate ~ !!.x),
          rows = .data$row_type == "glance_statistic" & .data$variable %in% !!.y
        )
      )
    ) %>%
    reduce(function(x, y) expr(!!x %>% !!y), .init = expr(x)) %>%
    eval()

  # returning gtsummary table --------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname add_glance
add_glance_source_note <- function(x, include = everything(), label = NULL,
                                   fmt_fun = NULL, glance_fun = NULL,
                                   text_interpret = c("md", "html"),
                                   sep1 = " = ", sep2 = "; ") {
  updated_call_list <- c(x$call_list, list(add_glance_source_note = match.call()))
  .assert_class(x, c("tbl_regression"))

  # default glance() function --------------------------------------------------
  glance_fun <- glance_fun %||% .default_glance_fun(x)

  # prepare glance statistics and formatting functions -------------------------
  lst_prep_glance <-
    .prep_glance_statistics(
      x = x, include = {{ include }},
      label = label, fmt_fun = fmt_fun,
      glance_fun = glance_fun
    )

  # compile stats into source note ---------------------------------------------
  x$table_styling$source_note <-
    lst_prep_glance$df_glance %>%
    left_join(lst_prep_glance$df_fmt_fun, by = c("variable" = "glance_statistic")) %>%
    rowwise() %>%
    mutate(
      fmt_stat = do.call(fmt_fun, list(.data$estimate)) %>%
        {
          paste0(.data$label, sep1, .)
        }
    ) %>%
    pull("fmt_stat") %>%
    paste(collapse = sep2)
  attr(x$table_styling$source_note, "text_interpret") <- match.arg(text_interpret)

  # returning gtsummary table --------------------------------------------------
  x$call_list <- updated_call_list
  x
}

.prep_glance_statistics <- function(x, include, label, fmt_fun, glance_fun) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_regression")) {
    stop("`x=` must be class 'tbl_regression'")
  }
  glance_fun <- gts_mapper(glance_fun, "glance_fun=")

  # prepping glance table ------------------------------------------------------
  df_glance_orig <- glance_fun(x$model_obj)
  include <- .select_to_varnames({{ include }}, data = df_glance_orig)
  df_glance_orig <- df_glance_orig %>% select(all_of(include))

  # adding user-specified labels -----------------------------------------------
  label <-
    .formula_list_to_named_list(
      x = label,
      data = df_glance_orig,
      arg_name = "label",
      type_check = chuck(type_check, "is_string", "fn"),
      type_check_msg = chuck(type_check, "is_string", "msg")
    )

  df_label <-
    switch(!is.null(label),
      enframe(unlist(label), "variable", "label")
    ) %||%
    tibble(variable = character(), label = character())


  # prepping data frame to be appended to `x$table_body` -----------------------
  language <- get_theme_element("pkgwide-str:language", default = "en")
  df_glance <-
    df_glance_orig %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "estimate"
    ) %>%
    # adding default labels
    left_join(df_default_glance_labels, by = c("variable" = "statistic_name")) %>%
    mutate(label = map2_chr(
      .data$label, .data$variable,
      ~ dplyr::coalesce(.x, .y) %>%
        translate_text(language = language)
    )) %>%
    # updating table with user-specified labels
    dplyr::rows_update(df_label, by = "variable") %>%
    mutate(
      row_type = "glance_statistic",
      var_label = .data$label
    )

  # parsing fmt_fun instructions -----------------------------------------------
  fmt_fun <-
    .formula_list_to_named_list(
      x = fmt_fun,
      data = df_glance_orig,
      arg_name = "fmt_fun",
      type_check = chuck(type_check, "is_function", "fn"),
      type_check_msg = chuck(type_check, "is_function", "msg")
    )

  df_fmt_fun <-
    tibble(glance_statistic = df_glance$variable) %>%
    mutate(
      fmt_fun = map(
        .data$glance_statistic,
        function(.x) {
          if (.x %in% c("nobs", "df", "df.residual")) {
            return(fmt_fun[[.x]] %||% style_number)
          }
          if (.x %in% c("p.value")) {
            return(fmt_fun[[.x]] %||% x$inputs$pvalue_fun)
          }
          return(fmt_fun[[.x]] %||% function(x) style_sigfig(x, digits = 3))
        }
      )
    )

  # return objects needed to finalize glance stats
  list(df_glance = df_glance, df_fmt_fun = df_fmt_fun)
}

# default statistic labels
df_default_glance_labels <-
  tribble(
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

.default_glance_fun <- function(x) {
  if (inherits(x$model_obj, "mira")) {
    assert_package("mice", fn = "add_glance_*()")
    return(function(x) broom::glance(mice::pool(x)))
  }

  return(broom::glance)
}
