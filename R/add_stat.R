#' Add a custom statistic column
#'
#' \lifecycle{experimental}
#' The function allows a user to add a new column (or columns) of statistics
#'
#' @param x `tbl_summary` or `tbl_svysummary` object
#' @param fns list of formulas indicating the functions that create the statistic.
#' See details below.
#' @param fmt_fun for numeric statistics, `fmt_fun=` is the styling/formatting
#' function. When multiple columns are added, supply a list of styling functions.
#' @param header Column header of new column(s). Default is `"**Statistic**"`
#' when a single column is returned; otherwise, the column name.
#' @param footnote Footnote associated with new column. Supply vector of footnotes
#' if more than one column added.
#' @param new_col_name DEPRECATED. name of new column to be created in `.$table_body`.
#' Default is `"add_stat_1"`, unless that column exists then it is `"add_stat_2"`, etc.
#' @param location Must be one of `c("label", "level")` and indicates which
#' row(s) the new statistics are placed on. When `"label"` a single statistic
#' is placed on the variable label row. When `"level"` the statistics are placed
#' on the variable level rows. The length of the vector of statistics returned from the
#' `fns` function must match the dimension of levels. Continuous and dichotomous
#' statistics are placed on the variable label row.
#'
#' @section Details:
#'
#' The returns from custom functions passed in `fns=` are required to follow a
#' specified format. Each of these function will execute on a single variable from
#' `tbl_summary()`/`tbl_svysummary()`.
#' 1. Each function must return a tibble or a vector. If a vector is returned,
#' it will be converted to a tibble with one column and number of rows equal
#' to the length of the vector.
#' 1. When `location = "label"`, the returned statistic from the custom function
#' must be a tibble with one row. When `location = "level"` the tibble must have
#' the same number of rows as there are levels in the variable (excluding the
#' row for unknown values).
#' 1. Each function may take the following arguments: `foo(data, variable, by, tbl, ...)`
#'     - `data=` is the input data frame passed to `tbl_summary()`
#'     - `variable=` is a string indicating the variable to perform the calculation on
#'     - `by=` is a string indicating the by variable from `tbl_summary=`, if present
#'     - `tbl=` the original `tbl_summary()`/`tbl_svysummary()` object is also available to utilize
#'
#' The user-defined does not need to utilize each of these inputs. It's
#' encouraged the user-defined function accept `...` as each of the arguments
#' *will* be passed to the function, even if not all inputs are utilized by
#' the user's function, e.g. `foo(data, variable, by, ...)`
#'
#' @export
#' @examples
#' library(dplyr); library(stringr)
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
#'   add_stat(
#'     fns = everything() ~ my_ttest, # all variables compared with with t-test
#'     fmt_fun = style_pvalue,        # format result with style_pvalue()
#'     header = "**My p-value**",     # new column header
#'     footnote = "Welch's two-sample t-test"
#'   ) %>%
#'   modify_header(all_stat_cols() ~ "**{level}**")
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
#'   add_stat(
#'     fns = everything() ~ my_ttest2,    # all variables will be compared by t-test
#'     fmt_fun = NULL, # fn returns and chr, so no formatting function needed
#'     header = "**Treatment Comparison**"       # column header
#'   )
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
#'   add_stat(
#'     fns = everything() ~ my_ttest3,    # all variables will be compared by t-test
#'     fmt_fun = list(style_sigfig, style_pvalue),
#'     header = c("**t-statistic**", "**p-value**")       # column header
#'   )
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_stat_ex1.png}{options: width=60\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_stat_ex2.png}{options: width=60\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{add_stat_ex3.png}{options: width=60\%}}

add_stat <- function(x, fns, fmt_fun = NULL, header = NULL,
                     footnote = NULL,
                     location = c("label", "level"),
                     new_col_name = NULL) {
  # checking inputs ------------------------------------------------------------
  location <- match.arg(location)

  if (!inherits(x, c("tbl_summary", "tbl_svysummary"))) {
    abort("Argument `x=` must be of class 'tbl_summary'")
  }

  if (!is.null(fmt_fun) && !is_function(fmt_fun) && !purrr::every(fmt_fun, is_function)) {
    abort("Argument `fmt_fun=` must be a function or list of functions.")
  }

  if (!is.null(new_col_name)) {
    lifecycle::deprecate_warn("1.4.0", "gtsummary::add_stat(new_col_name=)")
    if (!is.character(new_col_name)) {
      abort("Argument `new_col_name=` must be a character string or vector.")
    }
  }

  if (!is.null(header) && !is.character(header)) {
    abort("Argument `header=` must be a character string or vector.")
  }

  if (!is.null(footnote) && !is.character(footnote)) {
    abort("Argument `footnote=` must be a character string or vector.")
  }

  # convert fns to named list --------------------------------------------------
  fns <-
    .formula_list_to_named_list(
      x = fns,
      data = switch(class(x)[1],
                    "tbl_summary" = select(x$inputs$data, any_of(x$meta_data$variable)),
                    "tbl_svysummary" = select(x$inputs$data$variables, any_of(x$meta_data$variable))) ,
      var_info = x$table_body,
      arg_name = "fns"
    )

  # setting new column name ----------------------------------------------------
  stat_col_name <-
    switch(!is.null(new_col_name), new_col_name) %||%
    (names(x$table_body) %>% .[startsWith(., "add_stat_")] %>% length() %>% {paste0("add_stat_", . + 1)})

  # calculating statistics -----------------------------------------------------
  df_new_stat <-
    tibble(variable = names(fns)) %>%
    left_join(x$meta_data %>% select(.data$variable, .data$summary_type),
              by = "variable") %>%
    mutate(
      row_type = ifelse(.data$summary_type %in% c("categorical", "continuous2"),
                        .env$location,
                        "label"),
      label = map2(
        variable, row_type,
        ~filter(x$table_body, .data$variable == .x, .data$row_type == .y)$label
      )
    ) %>%
    mutate(
      df_add_stats = purrr::imap(fns, ~eval_fn_safe(tbl = x, variable = .y, fn = .x))
    ) %>%
    select(-.data$summary_type)

  # converting returned statistics to a tibble if not already ------------------
  df_new_stat$df_add_stats <-
    df_new_stat$df_add_stats %>%
    map(~switch(is.data.frame(.x), .x) %||% tibble(!!stat_col_name := .x))

  # check dims of calculated statistics ----------------------------------------
  purrr::pwalk(
    list(df_new_stat$variable, df_new_stat$label, df_new_stat$df_add_stats),
    function(variable, label, df_add_stats) {
      if (nrow(df_add_stats) != length(label))
        glue("Dimension of '{variable}' and the added statistic do not match. ",
             "Expecting statistic/data frame to be length/no. rows {length(label)}.") %>%
        abort()
    }
  )

  # table attributes -----------------------------------------------------------
  df_new_tibble_to_add <- bind_rows(df_new_stat$df_add_stats)
  new_col_names <- names(df_new_tibble_to_add)
  if (!rlang::is_empty(intersect(new_col_names, names(x$table_body)))) {
    glue("New added columns {quoted_list(intersect(new_col_names, names(x$table_body)))} ",
         "already exist in `x$table_body`.") %>%
      stringr::str_wrap() %>%
      abort()
  }

  table_header_updates <-
    tibble(
      column = new_col_names,
      label =
        switch(!is.null(header), rep_len(header, length(new_col_names))) %||%
        switch(is.null(header) && length(new_col_names) == 1, "**Statistic**") %||%
        new_col_names,
      footnote = rep_len(footnote %||% NA_character_, length(new_col_names)),
      column_is_character = map_lgl(column, ~is.character(df_new_tibble_to_add[[.x]]))
    )
  table_header_updates$fmt_fun <-
    # if user passes a single formatting function, apply it to all new columns
    switch(!is.null(fmt_fun) && is.function(fmt_fun), rep_len(list(fmt_fun), length(new_col_names))) %||%
    # if users passed a list of formatting function, apply them all to the new columns
    switch(!is.null(fmt_fun) && is.list(fmt_fun), rep_len(fmt_fun, length(new_col_names))) %||%
    # if the user did not pass a fmt_fun, use style_sigfig as default (NULL for character columns)
    switch(is.null(fmt_fun),
           map(table_header_updates$column_is_character,
               ~switch(!.x, style_sigfig)))
    # creating an expression of the updates
  expr_table_header_updates <-
    pmap(
      list(table_header_updates$column,
           table_header_updates$label,
           table_header_updates$footnote,
           table_header_updates$fmt_fun),
      function(column, label, footnote, fmt_fun) {
        expr(modify_table_header(column = !!column, label = !!label,
                                 hide = FALSE, footnote = !!footnote,
                                 fmt_fun = !!fmt_fun))
      }
    )

  # updating tbl_summary object ------------------------------------------------
  x <- x %>%
    modify_table_body(
      dplyr::left_join,
      unnest(df_new_stat, cols = all_of(c("label", "df_add_stats"))),
      by = c("variable", "row_type", "label")
    )

  # applying table_header updates
  x <- expr_table_header_updates %>% reduce(function(x, y) expr(!!x %>% !!y), .init = x) %>% eval()

  # return tbl_summary object --------------------------------------------------
  x
}



eval_fn_safe <- function(variable, tbl, fn) {

  tryCatch(
    withCallingHandlers({
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
    })

  stat
}
