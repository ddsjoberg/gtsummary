#' Convert to knitr_kable object
#'
#' Function converts gtsummary objects to a knitr_kable objects.  This function
#' is used in the background when the results are printed or knit.  A user can
#' use this function if they wish to add customized formatting available
#' via [knitr::kable].
#'
#' @param x Object created by a function from the gtsummary package
#' (e.g. [tbl_summary] or [tbl_regression])
#' @param include Character vector naming kable commands to include in printing.
#' Default is `NULL`, which utilizes all commands in `x$kable_calls`.
#' @param exclude Character vector naming kable commands to exclude in printing.
#' Default is `NULL`.
#' @param ... Additional arguments passed to [knitr::kable]
#' @export
#' @return A `knitr_kable` object
#' @seealso [tbl_summary] [tbl_regression] [tbl_uvregression] [tbl_survival]
#' @author Daniel D. Sjoberg
#' @examples
#' trial %>%
#'   tbl_summary(by = trt) %>%
#'   as_kable()
as_kable <- function(x, include = NULL, exclude = NULL, ...) {
  # making list of commands to include -----------------------------------------
  if (is.null(include)) include <- names(x$kable_calls)
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(x$kable_calls) %>% intersect(include)

  # user cannot exclude the first 'kable' command
  call_names <- include %>% setdiff(exclude)
  call_names <- "kable" %>% union(call_names)

  # saving vector of column labels
  col_labels <-
    x$table_header %>%
    filter(.data$hide == FALSE) %>%
    pull(.data$label)

  # taking each kable function call, concatenating them with %>% separating them
  x$kable_calls[call_names] %>%
    # removing NULL elements
    compact() %>%
    glue_collapse(sep = " %>% ") %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval() %>%
    # performing final modifcations prior to returning kable object
    mutate_all(~ ifelse(is.na(.), "", .)) %>%
    knitr::kable(col.names = col_labels, ...)
}
