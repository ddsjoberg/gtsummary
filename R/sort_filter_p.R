#' Sort and filter variables in table by p-values
#'
#' @param x An object created using gtsummary functions
#' @param q Logical argument. When `TRUE` will the q-value column is used
#' @param t p-values/q-values less than or equal to this threshold
#' will be retained. Default is 0.05
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @name sort_filter_p
#' @rdname sort_filter_p
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' sort_filter_p_ex1 <-
#'   trial %>%
#'   select(age, grade, response, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   filter_p(t = 0.8) %>%
#'   sort_p()
#'
#' # Example 2 ----------------------------------
#' sort_p_ex2 <-
#'   glm(response ~ trt + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   sort_p()
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "sort_filter_p_ex1.png", width = "50")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "sort_p_ex2.png", width = "50")`
#' }}
NULL


#' @export
#' @rdname sort_filter_p
sort_p <- function(x, q = FALSE) {
  updated_call_list <- c(x$call_list, list(sort_p = match.call()))
  # checking inputs ------------------------------------------------------------
  # checking class of x
  .assert_class(x, "gtsummary")

  # checking input table has a p.value column
  if (q == FALSE && !"p.value" %in% names(x$table_body)) {
    stop("There is no p-value column. `x$table_body` must have a column called 'p.value'",
      call. = FALSE
    )
  }

  # checking input table has a q.value column
  if (q == TRUE && !"q.value" %in% names(x$table_body)) {
    stop("There is no q-value column. `x$table_body` must have a column called 'q.value'",
      call. = FALSE
    )
  }

  # update table_body ---------------------------------------------------------
  # storing column name to bold
  sort_var <- ifelse(q == FALSE, "p.value", "q.value")

  # sorting table by p.value or q.value
  x$table_body <-
    x$table_body %>%
    group_by(.data$variable) %>%
    mutate(sort_col = min(.data[[sort_var]], na.rm = TRUE)) %>%
    arrange(.data$sort_col) %>%
    select(-"sort_col") %>%
    ungroup()

  # returning results ----------------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname sort_filter_p
filter_p <- function(x, q = FALSE, t = 0.05) {
  updated_call_list <- c(x$call_list, list(filter_p = match.call()))
  # checking inputs ------------------------------------------------------------
  .assert_class(x, "gtsummary")

  # checking input table has a p.value column
  if (q == FALSE && !"p.value" %in% names(x$table_body)) {
    stop("There is no p-value column. `x$table_body` must have a column called 'p.value'",
      call. = FALSE
    )
  }

  # checking input table has a q.value column
  if (q == TRUE && !"q.value" %in% names(x$table_body)) {
    stop("There is no q-value column. `x$table_body` must have a column called 'q.value'",
      call. = FALSE
    )
  }

  # update table_body ---------------------------------------------------------
  # storing column name to bold
  sort_var <- ifelse(q == FALSE, "p.value", "q.value")

  # sorting table by p.value or q.value
  x$table_body <-
    x$table_body %>%
    group_by(.data$variable) %>%
    mutate(sort_col = min(.data[[sort_var]], na.rm = TRUE)) %>%
    filter(.data$sort_col <= .env$t) %>%
    select(-"sort_col") %>%
    ungroup()

  # returning results ----------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
