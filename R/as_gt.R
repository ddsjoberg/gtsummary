#' Convert to gt_tbl object
#'
#' Function converts object to a gt_tbl object.  This function is used in the
#' background when the results are printed or knit.  A user can use this function
#' if they wish to add customized formatting available via the
#' [gt package](https://gt.rstudio.com/index.html).  Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette}
#' or
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{tbl_regression vignette}
#' for detailed examples in the 'Advanced Customization' section.
#'
#' @param x object created by a function from the gtsummary package
#' (e.g. \code{\link{tbl_summary}} or \code{\link{tbl_regression}})
#' @param include character vector naming gt commands to include in printing.
#' Default is `NULL`, which utilizes all commands in `x$gt_calls`.
#' @param exclude character vector naming gt commands to exclude in printing.
#' Default is `NULL`.
#' @param omit DEPRECATED. argument is synonymous with `exclude`
#' vector of named gt commands to omit. Default is `NULL`
#' @export
#' @return A `gt_tbl` object
#' @seealso \link{tbl_summary} \link{tbl_regression} \link{tbl_uvregression}
#' @author Daniel D. Sjoberg
#' @examples
#' \donttest{
#' as_gt_ex <-
#'   trial %>%
#'   tbl_summary(by = trt) %>%
#'   as_gt()
#' }
#' @section Example Output:
#'
#' \if{html}{\figure{as_gt_ex.png}{options: width=50\%}}

as_gt <- function(x, include = NULL, exclude = NULL, omit = NULL) {
  # making list of commands to include -----------------------------------------
  if (!is.null(omit)) {
    warn_deprecated("The 'omit' argument is deprecated. Please use 'include' and 'exclude' arguments.")
    if (is.null(exclude)) exclude <- omit
  }
  if (is.null(include)) include <- names(x$gt_calls)
  # this ensures list is in the same order as names(x$gt_calls)
  include <- names(x$gt_calls) %>% intersect(include)

  # user cannot omit the first 'gt' command
  call_names <- include %>% setdiff(exclude)
  call_names <- "gt" %>% union(call_names)

  # taking each gt function call, concatenating them with %>% separating them
  x$gt_calls[call_names] %>%
    # removing NULL elements
    compact() %>%
    glue_collapse(sep = " %>% ") %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval()
}
