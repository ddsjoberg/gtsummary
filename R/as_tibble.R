#' Convert gtsummary object to tibble
#'
#' Function converts gtsummary objects tibbles. The formatting stored in
#' `x$kable_calls` is applied.
#'
#' @name as_tibbleS3
#' @inheritParams as_kable
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

#' @rdname as_tibbleS3
#' @export
as_tibble.tbl_summary <- function(x, include = everything(), col_labels = TRUE,
                                  exclude = NULL,  ...) {
  # Printing message that spanning headers and footnotes will be lost
  message(glue(
    "Results printed using 'knitr::kable()' do not support footers \n",
    "or spanning headers. \n",
    "Tables styled by the gt package support footers and spanning headers."
  ))

  # DEPRECATION notes ----------------------------------------------------------
  if (!rlang::quo_is_null(rlang::enquo(exclude))) {
    lifecycle::deprecate_warn(
      "1.2.5",
      "gtsummary::as_tibble(exclude = )",
      "as_tibble(include = )",
      details = paste0(
        "The `include` argument accepts quoted and unquoted expressions similar\n",
        "to `dplyr::select()`. To exclude commands, use the minus sign.\n",
        "For example, `include = -cols_hide`"
      )
    )
  }

  # converting to charcter vector ----------------------------------------------
  include <- var_input_to_string(data = vctr_2_tibble(names(x$kable_calls)),
                                 select_input = !!rlang::enquo(include))
  exclude <- var_input_to_string(data = vctr_2_tibble(names(x$kable_calls)),
                                 select_input = !!rlang::enquo(exclude))

  # making list of commands to include -----------------------------------------
  # this ensures list is in the same order as names(x$kable_calls)
  include <- names(x$kable_calls) %>% intersect(include)

  # user cannot exclude the first 'kable' command
  include <- include %>% setdiff(exclude)
  include <- "kable" %>% union(include)

  # saving vector of column labels
  column_labels <-
    x$table_header %>%
    filter(.data$hide == FALSE) %>%
    pull(.data$label)

  # taking each kable function call, concatenating them with %>% separating them
  tbl <-
    x$kable_calls[include] %>%
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

#' @rdname as_tibbleS3
#' @export
as_tibble.tbl_regression <- as_tibble.tbl_summary

#' @rdname as_tibbleS3
#' @export
as_tibble.tbl_uvregression <- as_tibble.tbl_summary

#' @rdname as_tibbleS3
#' @export
as_tibble.tbl_merge <- as_tibble.tbl_summary

#' @rdname as_tibbleS3
#' @export
as_tibble.tbl_stack <- as_tibble.tbl_summary

#' @rdname as_tibbleS3
#' @export
as_tibble.tbl_survival <- as_tibble.tbl_summary
