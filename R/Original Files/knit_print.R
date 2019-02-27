#' A custom printing function for Rmarkdown
#'
#' The S3 generic function knit_print is the default printing function in knitr.
#' The chunk option render uses this function by default. The main purpose of this
#' S3 generic function is to customize printing of R objects in code chunks. We
#' can fall back to the normal printing behavior by setting the chunk
#' option `render = normal_print`. See \code{\link[knitr]{knit_print}} for details.
#'
#' @name knit_print
#' @export
#' @importFrom knitr knit_print
NULL


#' Print `fmt_table1` objects in Rmarkdown
#'
#' @param x object of class `fmt_table1`` object from \code{\link{fmt_table1}} function
#' @param options Copied from the printr package....should we delete?
#' @param ... further arguments passed to \code{\link[knitr]{kable}}.
#' @name knit_print.fmt_table1
#' @export
knit_print.fmt_table1 <- function(x, options, ...) {
  # extracting columns to print from list
  t <- x %>%
    purrr::pluck("table1") %>%
    dplyr::select(-dplyr::one_of("row_type", ".variable")) %>%
    dplyr::mutate_all(function(x) ifelse(is.na(x), " ", x))
  names(t) <- t[1, ] %>% t() %>% as.vector()
  t <- t[-1, ]

  k <- knitr::kable(
    t, options$printr.table.format,
    caption = options$printr.table.caption, ...
  )

  res <- paste(c("", "", k), collapse = "\n")
  knitr::asis_output(res)
}

#' Print `fmt_regression` objects in Rmarkdown
#'
#' @param x object of class `fmt_regression` object from \code{\link{fmt_regression}} function
#' @param options Copied from the printr package....should we delete?
#' @param ... further arguments passed to \code{\link[knitr]{kable}}.
#' @name knit_print.fmt_regression
#' @export
knit_print.fmt_regression <- function(x, options, ...) {
  # extracting columns to print from list
  t <- x %>%
    purrr::pluck("model_tbl") %>%
    dplyr::select(dplyr::one_of(c("label", "est", "ci", "pvalue"))) %>%
    dplyr::mutate_all(function(x) ifelse(is.na(x), "", x))
  names(t) <- t[1, ] %>% t() %>% as.vector()
  t <- t[-1, ]

  k <- knitr::kable(
    t, options$printr.table.format,
    caption = options$printr.table.caption, ...
  )

  res <- paste(c("", "", k), collapse = "\n")
  knitr::asis_output(res)
}

#' Print `fmt_uni_regression` objects in Rmarkdown
#'
#' @param x object of class `fmt_uni_regression` object from
#' \code{\link{fmt_uni_regression}} function
#' @param options Copied from the printr package....should we delete?
#' @param ... further arguments passed to `knitr::kable()`.
#' @name knit_print.fmt_uni_regression
#' @export
knit_print.fmt_uni_regression <- function(x, options, ...) {
  # extracting columns to print from list
  t <- x %>%
    purrr::pluck("model_tbl") %>%
    dplyr::select(-c("row_type", "var_type", "variable", "ll", "ul", "pvalue_exact", "p_pvalue")) %>%
    dplyr::mutate_all(function(x) ifelse(is.na(x), " ", x))
  names(t) <- t[1, ] %>% t() %>% as.vector()
  t <- t[-1, ]

  k <- knitr::kable(
    t, options$printr.table.format,
    caption = options$printr.table.caption, ...
  )

  res <- paste(c("", "", k), collapse = "\n")
  knitr::asis_output(res)
}
