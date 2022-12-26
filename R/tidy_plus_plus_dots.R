#' Passing Arguments to `broom.helpers::tidy_plus_plus()`
#'
#' @description
#' Functions `tbl_regression()` and `tbl_uvregression()` will pass any arguments
#' passed in the `...` to `broom.helpers::tidy_plus_plus(...)`.
#'
#' There are restrictions, however, when passing arguments to
#' `tbl_uvregression()`. Keep in mind that serially constructs model objects
#' and passes these model objects to `tbl_regression()`. Lastly, this series
#' of  'tbl_regression' are stacked using `tbl_stack()`.
#'
#' Because the arguments passed in `...` will be passed to `tbl_regression()`
#' for each model, you must ensure that the arguments passed will not
#' result in an error in _any_ of the calls to `tbl_regression()`.
#' For this reason, when passing arguments that take, for example, column names,
#' you must pass the names in `any_of()`.
#' For example, if you wanted remove reference rows, you would use:
#'
#' ```r
#' trial %>%
#'   tbl_uvregression(
#'     method = lm,
#'     y = age,
#'     include = c(trt, grade),
#'     no_reference_row =  any_of(c("trt", "grade"))
#'   )
#' ```
#'
#' For more flexible inputs that accept all tidyselect notation, you must utilize
#' `tbl_regression()` individually for each univariable model, and stack them
#' with `tbl_stack()`.
#' @name tidy_plus_plus_dots
#' @keywords internal
NULL
