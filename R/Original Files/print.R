#' Print `fmt_table1` objects
#'
#' @param x object of class `fmt_table1` object from \code{\link{fmt_table1}} function
#' @param ... further arguments passed to or from other methods.
#' @name print.fmt_table1
#' @export
print.fmt_table1 <- function(x, ...) {
  cat("\n")
  x %>%
    purrr::pluck("table1") %>%
    dplyr::select(-dplyr::one_of("row_type", ".variable")) %>%
    dplyr::mutate_all(function(x) ifelse(is.na(x), " ", x)) %T>% {
      names(.) <- NULL
    } %>%
    MASS::write.matrix()
}

#' Print `fmt_regression` objects
#'
#' @param x object of class `fmt_regression` object from
#' \code{\link{fmt_regression}} function
#' @param ... further arguments passed to or from other methods.
#' @name print.fmt_regression
#' @export
print.fmt_regression <- function(x, ...) {
  cat("\n")
  x %>%
    purrr::pluck("model_tbl") %>%
    dplyr::select(dplyr::one_of("label", "est", "ci", "pvalue")) %>%
    dplyr::mutate_all(function(x) ifelse(is.na(x), "", x)) %T>% {
      names(.) <- NULL
    } %>%
    MASS::write.matrix()
}


#' Print `fmt_uni_regression` objects
#'
#' @param x object of class \code{fmt_uni_regression} object from \code{fmt_uni_regression()} function
#' @param ... further arguments passed to or from other methods.
#' @name print.fmt_uni_regression
#' @export
print.fmt_uni_regression <- function(x, ...) {
  cat("\n")
  x %>%
    purrr::pluck("model_tbl") %>%
    dplyr::select(-c("row_type", "var_type", "variable", "ll", "ul", "pvalue_exact", "p_pvalue")) %>%
    dplyr::mutate_all(function(x) ifelse(is.na(x), "", x)) %T>% {
      names(.) <- NULL
    } %>%
    MASS::write.matrix()
}
