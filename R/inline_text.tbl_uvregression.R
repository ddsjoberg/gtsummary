#' Report statistics from regression summary tables inline
#'
#' Extracts and returns statistics from a table created by the `tbl_uvregression`
#' function for inline reporting in an R markdown document.
#' Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#'
#' @inheritParams inline_text.gtsummary
#' @inherit inline_text.tbl_regression
#' @param x (`tbl_uvregression`)\cr
#'   Object created by [`tbl_uvregression()`]
#'
#' @export
#' @return A string reporting results from a gtsummary table
#' @examplesIf gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"))
#' inline_text_ex1 <-
#'   trial[c("response", "age", "grade")] %>%
#'   tbl_uvregression(
#'     method = glm,
#'     method.args = list(family = binomial),
#'     y = response,
#'     exponentiate = TRUE
#'   )
#'
#' inline_text(inline_text_ex1, variable = age)
#' inline_text(inline_text_ex1, variable = grade, level = "III")
inline_text.tbl_uvregression <- inline_text.tbl_regression
