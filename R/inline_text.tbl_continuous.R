#' Report statistics from summary tables inline
#'
#' Extracts and returns statistics from a `tbl_continuous()` object for
#' inline reporting in an R markdown document. Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}

#' @inherit inline_text.tbl_summary
#' @param x (`tbl_continuous`)\cr
#'   Object created from  `tbl_continuous()`
#'
#' @name inline_text.tbl_continuous
#' @export
inline_text.tbl_continuous <- function(x,
                                       variable,
                                       column = NULL,
                                       level = NULL,
                                       pattern = NULL,
                                       pvalue_fun = label_style_pvalue(prepend_p = TRUE),
                                       ...) {
  set_cli_abort_call()
  # make the cards object look like `tbl_summary$cards`, then pass to `inline_text.tbl_summary()`
  x$cards[[1]] <- .cards_continuous_to_summary(x$cards[[1]], x$inputs$by)

  inline_text.tbl_summary(
    x = x,
    variable = {{ variable }},
    column = {{ column }},
    level = {{ level }},
    pattern = pattern,
    pvalue_fun = pvalue_fun
  )
}
