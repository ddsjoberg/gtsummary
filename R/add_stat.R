#' Add a custom statistic column
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' The function allows a user to add a new column with a custom, user-defined statistic.
#'
#' @param x tbl_summary object
#' @param fns list of formulas indicating the functions that create the statistic
#' @param fmt_fun for numeric statistics, `fmt_fun=` is the styling/formatting
#' function. Default is `style_sigfig()`.
#' @param header Column header of new column. Default is `"**Statistic**"`
#' @param footnote Footnote associated with new column. Default is no
#' footnote (i.e. NULL)
#' @param new_col_name name of new column to be created in `.$table_body`.
#' Default is `"add_stat_1"`, unless that column exists then it is `"add_stat_2"`, etc.
#'
#' @section Details:
#'
#' The custom functions passed in `fns=` are required to follow a specified
#' format. Each of these function will execute on a single variable from `tbl_summary()`.
#' 1. Each function must return a single scalar or character value of length one.
#' 1. Each function may take the following arguments: `foo(data, variable, by, tbl)`
#'   - `data=` is the input data frame passed to `tbl_summary()`
#'   - `variable=` is a string indicating the variable to perform the calculation on
#'   - `by=` is a string indicating the by variable from `tbl_summary=`, if present
#'   - `tbl=` the original `tbl_summary()` object is also available to utilize
#'
#' The user-defined does not need to utilize each of these inputs. It's
#' encouraged the user accept `...` as an input to safe-guard as each of these
#' items *will* be passed to the function, even if not all inputs are utilized by
#' the user's function, e.g. `foo(data, variable, by, ...)`
#'
#'
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' # this example replicates `add_p()`
#'
#' # fn returns t-test pvalue
#' my_ttest <- function(data, variable, by, ...) {
#'   t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
#' }
#'
#' add_stat_ex1 <-
#'   trial %>%
#'   select(trt, age, marker) %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_p(test = everything() ~ t.test) %>%
#'   # replicating result of `add_p()` with `add_stat()`
#'   add_stat(
#'     fns = everything() ~ my_ttest, # all variables compared with with t-test
#'     fmt_fun = style_pvalue,        # format result with style_pvalue()
#'     header = "**My p-value**"      # new column header
#'   )
#'
#' # Example 2 ----------------------------------
#' # fn returns t-test test statistic and pvalue
#' my_ttest2 <- function(data, variable, by, ...) {
#'   tt <- t.test(data[[variable]] ~ as.factor(data[[by]]))
#'
#'   # returning test statistic and pvalue
#'   stringr::str_glue(
#'     "t={style_sigfig(tt$statistic)}, {style_pvalue(tt$p.value, prepend_p = TRUE)}"
#'   )
#' }
#'
#' add_stat_ex2 <-
#'   trial %>%
#'   select(trt, age, marker) %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_stat(
#'     fns = everything() ~ my_ttest2,    # all variables will be compared by t-test
#'     fmt_fun = NULL, # fn returns and chr, so no formatting function needed
#'     header = "**Treatment Comparison**",       # column header
#'     footnote = "T-test statistic and p-value"  # footnote
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_stat_ex1.png}{options: width=60\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_stat_ex2.png}{options: width=60\%}}

add_stat <- function(x, fns, fmt_fun = style_sigfig, header = "**Statistic**",
#' @examples
#' # Example 1 ----------------------------------
                     footnote = NULL, new_col_name = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_summary")) {
    abort("Argument `x=` must be of class 'tbl_summary'")
  }

  if (!is.null(fmt_fun) && !is_function(fmt_fun)) {
    abort("Argument `fmt_fun=` must be NULL or a function.")
  }

  if (!is.null(new_col_name) && !is_string(new_col_name)) {
    abort("Argument `new_col_name=` must be NULL or a string")
  }

  if (!is_string(header)) {
    abort("Argument `header=` must be a string.")
  }

  if (!is.null(footnote) && !is_string(footnote)) {
    abort("Argument `footnote=` must be NULL or a string.")
  }

  # convert fns to named list --------------------------------------------------
  fns <- tidyselect_to_list(
    select(x$inputs$data, any_of(x$meta_data$variable)),
    fns, .meta_data = x$meta_data, arg_name = "fns"
  )

  # setting new column name ----------------------------------------------------
  stat_col_name <-
    switch(!is.null(new_col_name), new_col_name) %||%
    (names(x$table_body) %>% .[startsWith(., "add_stat_")] %>% length() %>% {paste0("add_stat_", . + 1)})

  # calculating statistics -----------------------------------------------------
  df_new_stat <- tibble(
    row_type = "label",
    variable = names(fns),
    !!stat_col_name := purrr::imap(fns, ~eval_fn_safe(tbl = x, variable = .y, fn = .x)) %>% unlist()
  )

  # updating tbl_summary object ------------------------------------------------
  # table_body
  x$table_body <-
    left_join(
      x$table_body,
      df_new_stat,
      by = c("variable", "row_type")
    )

  # fmt_fun in table_body
  lst_fmt_fun <- list(fmt_fun) %>% set_names(stat_col_name)
  x$table_header <-
    table_header_fill_missing(x$table_header, x$table_body)
  x$table_header <- do.call(table_header_fmt_fun,
                            c(list(table_header = x$table_header), lst_fmt_fun))

  # label in table_body
  lst_header <- list(header) %>% set_names(stat_col_name)
  x <- do.call(modify_header_internal,c(list(x = x), lst_header))

  # adding footnote
  if (!is.null(footnote)) {
    x$table_header <-
      mutate(
        x$table_header,
        footnote = ifelse(.data$column == stat_col_name,
                          .env$footnote, .data$footnote))
  }

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
