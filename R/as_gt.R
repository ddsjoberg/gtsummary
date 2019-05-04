#' Convert to gt_tbl object
#'
#' Function converts object to a gt_tbl object.  This function is used in the
#' background when the results are printed or knit.  A user can use this function
#' if they wish to add customized formatting available via the
#' [gt package](https://gt.rstudio.com/index.html).  Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette}
#' or
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{tbl_regression vignette}
#' for detailed examples in the 'Advacned Customization' section.
#'
#' @param x object created by a function from the gtsummary package
#' (e.g. \code{\link{tbl_summary}} or \code{\link{tbl_regression}})
#' @param omit vector of gt commands to omit. Default is `NULL`
#' @export
#' @seealso \link{tbl_summary} \link{tbl_regression} \link{tbl_uvregression}
#' @author Daniel D. Sjoberg
#' @examples
#' as_gt_ex <-
#'   trial %>%
#'   tbl_summary(by = "trt") %>%
#'   as_gt()
#' @section Example Output:
#'
#' \if{html}{\figure{as_gt_ex.png}{options: width=50\%}}

as_gt <- function(x, omit = NULL) {
  # user cannot omit the first 'gt' command
  omit <- omit %>% setdiff("gt")

  # taking each gt function call, concatenating them with %>% separating them
  x$gt_calls[names(x$gt_calls) %>% setdiff(omit)] %>%
    # removing NULL elements
    compact() %>%
    glue::glue_collapse(sep = " %>% ") %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval()
}
