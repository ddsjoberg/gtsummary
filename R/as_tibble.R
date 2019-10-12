#' Convert gtsummary object to tibble
#'
#' Function converts gtsummary objects tibbles. The formatting stored in
#' `x$kable_calls` is applied.
#'
#' @name as_tibble_methods
#' @param x Object created by a function from the gtsummary package
#' (e.g. [tbl_summary] or [tbl_regression])
#' @param include Character vector naming kable commands to include in printing.
#' Default is `NULL`, which utilizes all commands in `x$kable_calls`.
#' @param exclude Character vector naming kable commands to exclude in printing.
#' Default is `NULL`.
#' @param col_labels Logical argument adding column labels to output tibble.
#' Default is `TRUE`.
#' @param ... Not used
#' @return a [tibble][tibble::tibble-package]
#' @seealso [tbl_summary] [tbl_regression] [tbl_uvregression] [tbl_survival]
#' @author Daniel D. Sjoberg
#' @examples
#' tbl <-
#'   trial %>%
#'   tbl_summary(by = trt)
#'
#' as_tibble(tbl)
#'
#' # without column labels
#' as_tibble(tbl, col_names = FALSE)
NULL

#' @rdname as_tibble_methods
#' @export
as_tibble.tbl_summary <- function(x, include = NULL, exclude = NULL,
                                  col_labels = TRUE, ...) {
  # Printing message that spannign headers and footnotes will be lost
  message(glue(
    "Results printed using 'knitr::kable()' do not support footers \n",
    "or spanning headers. \n",
    "Tables styled by the gt package support footers and spanning headers."
  ))


  # making list of commands to include -----------------------------------------
  if (is.null(include)) include <- names(x$kable_calls)
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(x$kable_calls) %>% intersect(include)

  # user cannot exclude the first 'kable' command
  call_names <- include %>% setdiff(exclude)
  call_names <- "kable" %>% union(call_names)

  # saving vector of column labels
  column_labels <-
    x$table_header %>%
    filter(.data$hide == FALSE) %>%
    pull(.data$label)

  # taking each kable function call, concatenating them with %>% separating them
  tbl <-
    x$kable_calls[call_names] %>%
    # removing NULL elements
    compact() %>%
    glue_collapse(sep = " %>% ") %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval() %>%
    # performing final modifcations prior to returning kable object
    mutate_all(~ ifelse(is.na(.), "", .))

  # setting column labels
  if (col_labels == TRUE) tbl <- set_names(tbl, column_labels)

  tbl
}

#' @rdname as_tibble_methods
#' @export
as_tibble.tbl_regression <- as_tibble.tbl_summary

#' @rdname as_tibble_methods
#' @export
as_tibble.tbl_uvregression <- as_tibble.tbl_summary

#' @rdname as_tibble_methods
#' @export
as_tibble.tbl_merge <- as_tibble.tbl_summary

#' @rdname as_tibble_methods
#' @export
as_tibble.tbl_stack <- as_tibble.tbl_summary

#' @rdname as_tibble_methods
#' @export
as_tibble.tbl_survival <- as_tibble.tbl_summary
