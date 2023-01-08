#' Add a custom statistic column
#'
#' \lifecycle{maturing}
#' The function allows a user to add a new column (or columns) of statistics to an
#' existing `tbl_summary`, `tbl_svysummary`, or `tbl_continuous` object.
#'
#' @param x `tbl_summary`, `tbl_svysummary`, or `tbl_continuous` object
#' @param fns list of formulas indicating the functions that create the statistic.
#' See details below.
#' @param location list of formulas indicating the location the new statistics
#' are placed. The RHS of the formula must be one of `c("label", "level", "missing")`.
#' When `"label"`, a single statistic
#' is placed on the variable label row. When `"level"` the statistics are placed
#' on the variable level rows. The length of the vector of statistics returned from the
#' `fns` function must match the dimension of levels. Default is to place the
#' new statistics on the label row.
#' @param ... DEPRECATED
#'
#' @section Details:
#'
#' The returns from custom functions passed in `fns=` are required to follow a
#' specified format. Each of these function will execute on a single variable.
#' 1. Each function must return a tibble or a vector. If a vector is returned,
#' it will be converted to a tibble with one column and number of rows equal
#' to the length of the vector.
#' 1. When `location = "label"`, the returned statistic from the custom function
#' must be a tibble with one row. When `location = "level"` the tibble must have
#' the same number of rows as there are levels in the variable (excluding the
#' row for unknown values).
#' 1. Each function may take the following arguments: `foo(data, variable, by, tbl, ...)`
#'     - `data=` is the input data frame passed to `tbl_summary()`
#'     - `variable=` is a string indicating the variable to perform the calculation on. This is the variable in the label column of the table.
#'     - `by=` is a string indicating the by variable from `tbl_summary=`, if present
#'     - `tbl=` the original `tbl_summary()`/`tbl_svysummary()` object is also available to utilize
#'
#' The user-defined does not need to utilize each of these inputs. It's
#' encouraged the user-defined function accept `...` as each of the arguments
#' *will* be passed to the function, even if not all inputs are utilized by
#' the user's function, e.g. `foo(data, variable, by, ...)`
#'
#' - Use `modify_header()` to update the column headers
#' - Use `modify_fmt_fun()` to update the functions that format the statistics
#' - Use `modify_footnote()` to add a explanatory footnote
#'
#' If you return a tibble with column names `p.value` or `q.value`, default
#' p-value formatting will be applied, and you may take advantage of subsequent
#' p-value formatting functions, such as `bold_p()` or `add_q()`.
#'
#' To access the continuous variable in a `tbl_continuous()` table, use
#' `tbl$inputs$variable`.
#'
#' @export
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @examples
#' \donttest{
#' library(dplyr, warn.conflicts = FALSE)
#' library(stringr)
#' # Example 1 ----------------------------------
#' # fn returns t-test pvalue
#' my_ttest <- function(data, variable, by, ...) {
#'   t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
#' }
#'
#' add_stat_ex1 <-
#'   trial %>%
#'   select(trt, age, marker) %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_stat(fns = everything() ~ my_ttest) %>%
#'   modify_header(
#'     list(
#'       add_stat_1 ~ "**p-value**",
#'       all_stat_cols() ~ "**{level}**"
#'     )
#'   )
#'
#' # Example 2 ----------------------------------
#' # fn returns t-test test statistic and pvalue
#' my_ttest2 <- function(data, variable, by, ...) {
#'   t.test(data[[variable]] ~ as.factor(data[[by]])) %>%
#'     broom::tidy() %>%
#'     mutate(
#'       stat = str_glue("t={style_sigfig(statistic)}, {style_pvalue(p.value, prepend_p = TRUE)}")
#'     ) %>%
#'     pull(stat)
#' }
#'
#' add_stat_ex2 <-
#'   trial %>%
#'   select(trt, age, marker) %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_stat(fns = everything() ~ my_ttest2) %>%
#'   modify_header(add_stat_1 ~ "**Treatment Comparison**")
#'
#' # Example 3 ----------------------------------
#' # return test statistic and p-value is separate columns
#' my_ttest3 <- function(data, variable, by, ...) {
#'   t.test(data[[variable]] ~ as.factor(data[[by]])) %>%
#'     broom::tidy() %>%
#'     select(statistic, p.value)
#' }
#'
#' add_stat_ex3 <-
#'   trial %>%
#'   select(trt, age, marker) %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_stat(fns = everything() ~ my_ttest3) %>%
#'   modify_header(
#'     list(
#'       statistic ~ "**t-statistic**",
#'       p.value ~ "**p-value**"
#'     )
#'   ) %>%
#'   modify_fmt_fun(
#'     list(
#'       statistic ~ style_sigfig,
#'       p.value ~ style_pvalue
#'     )
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_stat_ex1.png", width = "60")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_stat_ex2.png", width = "60")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_stat_ex3.png", width = "60")`
#' }}

add_stat <- function(x, fns, location = NULL, ...) {
  updated_call_list <- c(x$call_list, list(add_stat = match.call()))
  # checking inputs ------------------------------------------------------------
  .assert_class(x, c("tbl_summary", "tbl_svysummary", "tbl_continuous"))

  # deprecated arguments -------------------------------------------------------
  dots <- rlang::dots_list(...)
  dep_args <-
    list(
      fmt_fun = list("gtsummary::add_stat(fmt_fun=)", "modify_fmt_fun()"),
      header = list("gtsummary::add_stat(header=)", "modify_header()"),
      footnote = list("gtsummary::add_stat(footnote=)", "modify_footnote()"),
      new_col_name = list("gtsummary::add_stat(new_col_name=)", NULL)
    )
  purrr::iwalk(
    dep_args,
    function(.x, .y) {
      if (!is.null(dots[[.y]])) {
        lifecycle::deprecate_stop(when = "1.4.0", what = .x[[1]], with = .x[[2]])
      }
    }
  )

  # convert to named lists -----------------------------------------------------
  if (rlang::is_string(location)) {
    lifecycle::deprecate_stop(
      "1.4.0",
      "gtsummary::add_stat(location = 'must be a formula list, e.g. `everything() ~ \"label\"`,')"
    )
    location <- inject(everything() ~ !!location)
  }
  location <-
    .formula_list_to_named_list(
      x = location,
      data = switch(class(x)[1],
        "tbl_summary" = select(x$inputs$data, any_of(x$meta_data$variable)),
        "tbl_svysummary" = select(x$inputs$data$variables, any_of(x$meta_data$variable))
      ),
      var_info = x$table_body,
      arg_name = "location",
      type_check = chuck(type_check, "is_string", "fn"),
      type_check_msg = chuck(type_check, "is_string", "msg")
    )
  imap(
    location,
    ~ switch(!is_string(.x) || !.x %in% c("label", "level", "missing"),
      abort("RHS of `location=` formulas must be one of 'label', 'level', or 'missing'")
    )
  )

  fns <-
    .formula_list_to_named_list(
      x = fns,
      data = switch(class(x)[1],
        "tbl_summary" = select(x$inputs$data, any_of(x$meta_data$variable)),
        "tbl_svysummary" = select(x$inputs$data$variables, any_of(x$meta_data$variable))
      ),
      var_info = x$table_body,
      arg_name = "fns",
      type_check = chuck(type_check, "is_function", "fn"),
      type_check_msg = chuck(type_check, "is_function", "msg")
    )

  # setting new column name ----------------------------------------------------
  stat_col_name <-
    select(x$table_body, dplyr::matches("^add_stat_\\d*[1-9]\\d*$")) %>%
    names() %>%
    length() %>%
    {
      paste0("add_stat_", . + 1)
    }

  # calculating statistics -----------------------------------------------------
  df_new_stat <-
    tibble(variable = names(fns)) %>%
    left_join(x$meta_data %>% select("variable", "summary_type"),
      by = "variable"
    ) %>%
    mutate(
      row_type = map_chr(.data$variable, ~ location[[.x]] %||% "label"),
      label = map2(
        .data$variable, .data$row_type,
        ~ filter(x$table_body, .data$variable == .x, .data$row_type == .y)$label
      )
    ) %>%
    mutate(
      df_add_stats = purrr::imap(fns, ~ eval_fn_safe(tbl = x, variable = .y, fn = .x))
    ) %>%
    select(-"summary_type")

  # converting returned statistics to a tibble if not already ------------------
  df_new_stat$df_add_stats <-
    df_new_stat$df_add_stats %>%
    map(~ switch(is.data.frame(.x),
      .x
    ) %||% tibble(!!stat_col_name := .x))

  # check dims of calculated statistics ----------------------------------------
  purrr::pwalk(
    list(df_new_stat$variable, df_new_stat$label, df_new_stat$df_add_stats),
    function(variable, label, df_add_stats) {
      if (nrow(df_add_stats) != length(label)) {
        glue(
          "Dimension of '{variable}' and the added statistic do not match. ",
          "Expecting statistic/data frame to be length/no. rows {length(label)}."
        ) %>%
          abort()
      }
    }
  )

  # check new column names do not exist in `x$table_body`
  new_col_names <- bind_rows(df_new_stat$df_add_stats) %>% names()
  if (any(new_col_names %in% names(x$table_body))) {
    paste(
      "Cannot add new column that already exist in gtsummary table:",
      "{.field {quoted_list(new_col_names %in% intersect(names(x$table_body)))}}"
    ) %>%
      abort()
  }

  # merging new columns with `x$table_body` ------------------------------------
  x <-
    x %>%
    modify_table_body(
      left_join,
      df_new_stat %>% tidyr::unnest(cols = c("label", "df_add_stats")),
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
      fmt_fun = function(x) style_sigfig(x, digits = 3)
    ) %>%
    # if a numeric column is called 'p.value' or 'q.value', giving p-value default formatting
    modify_table_styling(
      columns = c(where(is.numeric) & any_of(c("p.value", "q.value"))),
      fmt_fun = get_theme_element("pkgwide-fn:pvalue_fun", default = style_pvalue)
    )

  # return tbl_summary object --------------------------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list
  x
}


eval_fn_safe <- function(variable, tbl, fn) {
  tryCatch(
    withCallingHandlers(
      {
        # initializing to NA
        stat <- NA_real_
        stat <- rlang::call2(
          fn,
          data = tbl$inputs$data,
          variable = variable,
          by = tbl$inputs$by,
          tbl = tbl
        ) %>%
          eval()
      },
      # printing warning and errors as message
      warning = function(w) {
        message(glue(
          "There was an warning for variable '{variable}':\n ", as.character(w)
        ))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      message(glue(
        "There was an error for variable '{variable}':\n", as.character(e)
      ))
      return(NA_real_)
    }
  )

  stat
}
