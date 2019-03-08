#' Convert to gt_tbl object
#'
#' Function converts object to a gt_tbl object.  This function is used in the
#' background when the results are printed or knit.  A user can use this function
#' if they wish to add customized formatting available via the
#' [gt package](https://gt.rstudio.com/index.html)
#'
#' @param x object created by a function from the gtsummary package
#' (e.g. \code{\link{tbl_summary}} or \code{\link{tbl_regression}})
#' @param omit vector of gt commands to omit. Default is `NULL`
#' @export
#' @author Daniel Sjoberg
#' @examples
#' trial %>% tbl_summary(by = "trt") %>% as_gt()
as_gt <- function(x, omit = NULL) {
  # user cannot omit the first 'gt' command
  omit <- omit %>% setdiff("gt")

  # taking each gt function call, concatenating them with %>% separating them
  x$gt_calls[names(x$gt_calls) %>% setdiff(omit)] %>%
    paste0(collapse = " %>% ") %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval()
}
