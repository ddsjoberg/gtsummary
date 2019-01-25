#' The `tibble` package's `as_tibble` function
#'
#' See \code{\link[tibble]{as_tibble}} for details.
#' @name as_tibble
#' @importFrom tibble as_tibble
#' @export
NULL

#' The `tibble` package's `as_data_frame` function is an
#' alias for \code{tibble::as_tibble}
#'
#' See \code{\link[tibble]{as_tibble}} for details.
#' @name as_data_frame
#' @importFrom tibble as_data_frame
#' @export
NULL

#' Convert `fmt_table1` objects to data frame
#'
#' @param x object of class `fmt_table1` object from \code{\link{fmt_table1}}
#'   function
#' @param ... further arguments passed to individual methods.
#' @details as_data_frame.fmt_table1 is alias.
#' @seealso \code{\link{fmt_table1}}, \code{\link{as_tibble.fmt_regression}},
#'   \code{\link{as_tibble.fmt_uni_regression}}, \code{\link[tibble]{as_tibble}}
#' @name as_tibble.fmt_table1
#' @aliases as_data_frame.fmt_table1
#' @export
as_tibble.fmt_table1 <- function(x, ...) {
  table_extra <- x %>%
    purrr::pluck("table1") %>%
    dplyr::select(-dplyr::one_of("row_type", ".variable")) %>%
    dplyr::mutate_all(function(x) ifelse(is.na(x), " ", x)) %>%
    row_to_name()
  return(table_extra)
}

#' @export
#' @rdname as_tibble.fmt_table1
as_data_frame.fmt_table1 <- as_tibble.fmt_table1

#' Convert `fmt_regression` objects to data frame
#'
#' @param x object of class \code{fmt_regression} object from
#'   \code{fmt_regression()} function
#' @param ... further arguments passed to individual methods.
#' @details as_data_frame.fmt_regression is alias.
#' @seealso \code{\link{fmt_regression}}, \code{\link{as_tibble.fmt_table1}},
#'   \code{\link{as_tibble.fmt_uni_regression}}, \code{\link[tibble]{as_tibble}}
#' @name as_tibble.fmt_regression
#' @aliases as_data_frame.fmt_regression
#' @export
as_tibble.fmt_regression <- function(x, ...) {
  table_extra <- x %>%
    purrr::pluck("model_tbl") %>%
    dplyr::select(dplyr::one_of(c("label", "est", "ci", "pvalue"))) %>%
    dplyr::mutate_all(function(x) ifelse(is.na(x), " ", x)) %>%
    row_to_name()
  return(table_extra)
}

#' @export
#' @rdname as_tibble.fmt_regression
as_data_frame.fmt_regression <- as_tibble.fmt_regression

#' Convert `fmt_uni_regression` objects to data frame
#'
#' @param x object of class \code{fmt_uni_regression} object from
#'   \code{fmt_uni_regression()} function
#' @param ... further arguments passed to individual methods.
#' @details as_data_frame.fmt_uni_regression is alias.
#' @seealso \code{\link{fmt_uni_regression}}, \code{\link{as_tibble.fmt_table1}},
#'   \code{\link{as_tibble.fmt_regression}}, \code{\link[tibble]{as_tibble}}
#' @aliases as_data_frame.fmt_uni_regression
#' @name as_tibble.fmt_uni_regression
#' @export
as_tibble.fmt_uni_regression <- function(x, ...) {
  table_extra <- x %>%
    purrr::pluck("model_tbl") %>%
    dplyr::select(-c("row_type", "var_type", "variable", "ll", "ul", "pvalue_exact", "p_pvalue")) %>%
    dplyr::mutate_all(function(x) ifelse(is.na(x), " ", x)) %>%
    row_to_name()
  return(table_extra)
}

#' @export
#' @rdname as_tibble.fmt_uni_regression
as_data_frame.fmt_uni_regression <- as_tibble.fmt_uni_regression
