#' Add a custom statistic
#'
#' The function allows a user to add a new column (or columns) of statistics to an
#' existing `tbl_summary`, `tbl_svysummary`, or `tbl_continuous` object.
#'
#' @param x (`tbl_summary`/`tbl_svysummary`/`tbl_continuous`)\cr
#'   A gtsummary table of class `'tbl_summary'`, `'tbl_svysummary'`, or `'tbl_continuous'`.
#' @param fns ([`formula-list-selector`][syntax])\cr
#'   Indicates the functions that create the statistic. See details below.
#' @param location ([`formula-list-selector`][syntax])\cr
#'   Indicates the location the new statistics are placed.
#'   The values must be one of `c("label", "level", "missing")`.
#'   When `"label"`, a single statistic
#'   is placed on the variable label row. When `"level"` the statistics are placed
#'   on the variable level rows. The length of the vector of statistics returned from the
#'   `fns` function must match the dimension of levels. Default is to place the
#'   new statistics on the label row.
#'
#' @section Details:
#'
#' The returns from custom functions passed in `fns=` are required to follow a
#' specified format. Each of these function will execute on a single variable.
#'
#' 1. Each function must return a tibble or a vector. If a vector is returned,
#'   it will be converted to a tibble with one column and number of rows equal
#'   to the length of the vector.
#'
#' 1. When `location='label'`, the returned statistic from the custom function
#'   must be a tibble with one row. When `location='level'` the tibble must have
#'   the same number of rows as there are levels in the variable (excluding the
#'   row for unknown values).
#'
#' 1. Each function may take the following arguments: `foo(data, variable, by, tbl, ...)`
#'     - `data=` is the input data frame passed to `tbl_summary()`
#'     - `variable=` is a string indicating the variable to perform the calculation on. This is the variable in the label column of the table.
#'     - `by=` is a string indicating the by variable from `tbl_summary=`, if present
#'     - `tbl=` the original `tbl_summary()`/`tbl_svysummary()` object is also available to utilize
#'
#' The user-defined function does not need to utilize each of these inputs. It's
#' encouraged the user-defined function accept `...` as each of the arguments
#' *will* be passed to the function, even if not all inputs are utilized by
#' the user's function, e.g. `foo(data, variable, by, ...)`
#'
#' - Use `modify_header()` to update the column headers
#' - Use `modify_fmt_fun()` to update the functions that format the statistics
#' - Use `modify_footnote_header()` to add a explanatory footnote
#'
#' If you return a tibble with column names `p.value` or `q.value`, default
#' p-value formatting will be applied, and you may take advantage of subsequent
#' p-value formatting functions, such as `bold_p()` or `add_q()`.
#'
#' @export
#' @return A 'gtsummary' of the same class as the input
#'
#' @examples
#' # Example 1 ----------------------------------
#' # fn returns t-test pvalue
#' my_ttest <- function(data, variable, by, ...) {
#'   t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
#' }
#'
#' trial |>
#'   tbl_summary(
#'     by = trt,
#'     include = c(trt, age, marker),
#'     missing = "no"
#'   ) |>
#'   add_stat(fns = everything() ~ my_ttest) |>
#'   modify_header(add_stat_1 = "**p-value**", all_stat_cols() ~ "**{level}**")
#'
#' # Example 2 ----------------------------------
#' # fn returns t-test test statistic and pvalue
#' my_ttest2 <- function(data, variable, by, ...) {
#'   t.test(data[[variable]] ~ as.factor(data[[by]])) |>
#'     broom::tidy() %>%
#'     dplyr::mutate(
#'       stat = glue::glue("t={style_sigfig(statistic)}, {style_pvalue(p.value, prepend_p = TRUE)}")
#'     ) %>%
#'     dplyr::pull(stat)
#' }
#'
#' trial |>
#'   tbl_summary(
#'     by = trt,
#'     include = c(trt, age, marker),
#'     missing = "no"
#'   ) |>
#'   add_stat(fns = everything() ~ my_ttest2) |>
#'   modify_header(add_stat_1 = "**Treatment Comparison**")
#'
#' # Example 3 ----------------------------------
#' # return test statistic and p-value is separate columns
#' my_ttest3 <- function(data, variable, by, ...) {
#'   t.test(data[[variable]] ~ as.factor(data[[by]])) %>%
#'     broom::tidy() %>%
#'     select(statistic, p.value)
#' }
#'
#' trial |>
#'   tbl_summary(
#'     by = trt,
#'     include = c(trt, age, marker),
#'     missing = "no"
#'   ) |>
#'   add_stat(fns = everything() ~ my_ttest3) |>
#'   modify_header(statistic = "**t-statistic**", p.value = "**p-value**") |>
#'   modify_fmt_fun(statistic = label_style_sigfig(), p.value = label_style_pvalue(digits = 2))
add_stat <- function(x, fns, location = everything() ~ "label") {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(add_stat = match.call()))

  # checking inputs ------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(fns)
  check_class(x, c("tbl_summary", "tbl_svysummary", "tbl_continuous"))

  # adding type if `tbl_continuous`...this is used later on
  if (inherits(x, "tbl_continuous")) {
    x$inputs$type <- rep_named(x$inputs$include, list("categorical"))
  }

  # convert to named lists -----------------------------------------------------
  cards::process_formula_selectors(
    scope_table_body(x$table_body),
    location = location,
    fns = fns
  )
  cards::fill_formula_selectors(
    scope_table_body(x$table_body),
    location = eval(formals(gtsummary::add_stat)[["location"]])
  )
  cards::check_list_elements(
    x = location,
    predicate = \(x) is_string(x) && x %in% c("label", "level", "missing"),
    error_msg = "The element values for the {.arg location} argument
                 must be one of {.val {c('label', 'level', 'missing')}}."
  )
  cards::check_list_elements(
    x = fns,
    predicate = \(x) is.function(x),
    error_msg = "The element values for the {.arg fns} argument must be a {.cls function}."
  )

  # setting new column name ----------------------------------------------------
  stat_col_name <-
    dplyr::select(x$table_body, dplyr::matches("^add_stat_\\d*[1-9]\\d*$")) |>
    names() |>
    length() %>%
    {paste0("add_stat_", . + 1)} # styler: off

  # calculating statistics -----------------------------------------------------
  df_new_stat <-
    dplyr::tibble(variable = names(fns)) |>
    dplyr::mutate(
      summary_type = map_chr(.data$variable, ~ x$inputs$type[[.x]]),
      row_type = map_chr(.data$variable, ~ location[[.x]]),
      label = map2(
        .data$variable, .data$row_type,
        ~ dplyr::filter(x$table_body, .data$variable == .x, .data$row_type == .y)$label
      )
    ) |>
    mutate(
      df_add_stats =
        imap(
          fns,
          ~ eval_fn_safe(tbl = x, variable = .y, fn = .x)
        )
    ) |>
    select(-"summary_type")

  # converting returned statistics to a tibble if not already ------------------
  df_new_stat$df_add_stats <-
    df_new_stat$df_add_stats |>
    map(~ switch(is.data.frame(.x), .x) %||% dplyr::tibble(!!stat_col_name := .x)) # styler: off

  # check dims of calculated statistics ----------------------------------------
  pmap(
    list(df_new_stat$variable, df_new_stat$label, df_new_stat$df_add_stats),
    function(variable, label, df_add_stats) {
      if (nrow(df_add_stats) != length(label)) {
        cli::cli_abort(
          c("Dimension of {.val {variable}} and the added statistic do not match.",
             i = "Expecting statistic/data frame to be length/no. rows {.val {length(label)}}."),
          call = get_cli_abort_call()
        )
      }
    }
  )

  # check new column names do not exist in `x$table_body`
  new_col_names <- dplyr::bind_rows(df_new_stat$df_add_stats) |> names()
  if (any(new_col_names %in% names(x$table_body))) {
    cli::cli_abort(
      "Cannot add new column that already exist in {.cls gtsummary} table:
       {.val {new_col_names |> intersect(names(x$table_body))}}.",
      call = get_cli_abort_call()
    )
  }

  # merging new columns with `x$table_body` ------------------------------------
  x <- x |>
    modify_table_body(
      dplyr::left_join,
      df_new_stat |> tidyr::unnest(cols = c("label", "df_add_stats")),
      by = c("variable", "row_type", "label")
    ) %>%
    # showing all new columns
    modify_table_styling(
      columns = all_of(new_col_names),
      hide = FALSE,
    ) %>%
    # assigning a default fmt_fun
    modify_table_styling(
      columns = c(where(is.numeric) & all_of(new_col_names)),
      fmt_fun = label_style_sigfig(digits = 3)
    ) |>
    # if a numeric column is called 'p.value' or 'q.value', giving p-value default formatting
    modify_table_styling(
      columns = c(where(is.numeric) & any_of(intersect(c("p.value", "q.value"), new_col_names))),
      fmt_fun = get_theme_element("pkgwide-fn:pvalue_fun", default = label_style_pvalue())
    )

  # return tbl_summary object --------------------------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list
  x
}


eval_fn_safe <- function(variable, tbl, fn) {
  result <-
    cards::eval_capture_conditions(
      exec(
        fn,
        data = tbl$inputs$data,
        variable = variable,
        by = tbl$inputs$by,
        tbl = tbl
      )
    )

  if (!is_empty(result[["warning"]])) {
    cli::cli_inform(
      c("There was a warning for variable {.val {variable}}", "!" = "{result[['warning']]}")
    )
  }
  if (!is_empty(result[["error"]])) {
    cli::cli_inform(
      c("There was a error for variable {.val {variable}}", "x" = "{result[['error']]}")
    )
  }

  # return result
  result[["result"]] %||% NA_real_
}
