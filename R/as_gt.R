#' Convert gtsummary object to a gt_tbl object
#'
#' Function converts gtsummary objects to a gt_tbl objects.
#' Function is used in the background when the results are printed or knit.
#' A user can use this function if they wish to add customized formatting
#' available via the [gt package](https://gt.rstudio.com/index.html).
#' Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#advanced}{tbl_summary vignette}
#' or
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#advanced}{tbl_regression vignette}
#' for detailed examples in the 'Advanced Customization' section.
#'
#' @param x Object created by a function from the gtsummary package
#' (e.g. [tbl_summary] or [tbl_regression])
#' @param include Character vector naming gt commands to include in printing.
#' Default is `NULL`, which utilizes all commands in `x$gt_calls`.
#' @param exclude Character vector naming gt commands to exclude in printing.
#' Default is `NULL`.
#' @param omit DEPRECATED. Argument is synonymous with `exclude`
#' vector of named gt commands to omit. Default is `NULL`
#' @export
#' @return A `gt_tbl` object
#' @seealso [tbl_summary] [tbl_regression] [tbl_uvregression] [tbl_survival]
#' @author Daniel D. Sjoberg
#' @examples
#' \donttest{
#' as_gt_ex <-
#'   trial[c("trt", "age", "response", "grade")] %>%
#'   tbl_summary(by = trt) %>%
#'   as_gt()
#' }
#' @section Example Output:
#'
#' \if{html}{\figure{as_gt_ex.png}{options: width=50\%}}

as_gt <- function(x, include = NULL, exclude = NULL, omit = NULL) {
  # making list of commands to include -----------------------------------------
  if (!is.null(omit)) {
    lifecycle::deprecate_warn("1.2.0",
                              "gtsummary::as_gt(omit = )",
                              "as_gt(exclude = )")
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
    # adding default gt formatting options
    union(getOption("gtsummary.as_gt.addl_cmds", default = NULL)) %>%
    # removing NULL elements
    compact() %>%
    glue_collapse(sep = " %>% ") %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval()
}
