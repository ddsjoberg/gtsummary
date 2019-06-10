
#' Sort variables in table by ascending p-values
#'
#' Sort tables created by gtsummary by p-values
#'
#' @param x an object created using gtsummary functions
#' @param ... further arguments passed to other methods.
#' @author Karissa Whiting
#' @seealso \code{\link{sort_p.tbl_summary}},
#' \code{\link{sort_p.tbl_regression}},
#' \code{\link{sort_p.tbl_uvregression}}
#' @export
#' @keywords internal
sort_p <- function(x, ...) UseMethod("sort_p")

#' Sort variables in table by ascending p-values
#'
#' Sort variables in tables created by \code{\link{tbl_summary}} by ascending p-values
#'
#' @param x an object created using `tbl_summary` function
#' @param q logical argument. When TRUE will sort by the q-value column rather than the p-values
#' @param ... not used
#' @family tbl_summary tools
#' @author Karissa Whiting
#' @examples
#' tbl_sum_sort_p_ex <-
#'   trial %>%
#'   dplyr::select(age, grade, response, trt) %>%
#'   tbl_summary(by = "trt") %>%
#'   add_p() %>%
#'   sort_p()
#' @section Example Output:
#' \if{html}{\figure{tbl_sum_sort_p_ex.png}{options: width=50\%}}
#' @export
sort_p.tbl_summary <- function(x, q = FALSE, ...) {

  # checking that add_p has been previously run
  if (is.null(x$call_list$add_p)) {
    stop("Before you can sort by p-values, run add_p() to calculate the p-values")
  }
  # checking that add_q has been previously run if sort by q-values
  if (q == TRUE & is.null(x$call_list$add_q)) {
    stop("Before you sort by q-values, run add_q() to calculate the q-values")
  }

  sort_var <-  "p.value"
  if (q == TRUE) sort_var <- "q.value"

  table_body <-  x$table_body %>%
    group_by(.data$variable) %>%
    mutate(sort_col = min(.data[[sort_var]], na.rm = TRUE))

  table_body <- table_body %>%
    arrange(.data$sort_col) %>%
    select(-.data$sort_col) %>%
    ungroup()

  # replacing old table_body with new
  x$table_body <- table_body

  # adding indicator to output that add_overall was run on this data
  x$call_list <- c(x$call_list, list(sort_p = match.call()))

  # returning tbl_summary object
  return(x)

}

#' Sort variables in table by ascending p-values
#'
#' Sort variables in tables created by \code{\link{tbl_regression}} by ascending p-values
#'
#' @param x an object created using `tbl_regression` function
#' @param ... not used
#' @family tbl_regression tools
#' @author Karissa Whiting
#' @examples
#' tbl_lm_sort_p_ex <-
#'   glm(response ~ trt + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   sort_p()
#' @section Example Output:
#' \if{html}{\figure{tbl_lm_sort_p_ex.png}{options: width=50\%}}
#' @export
#'

sort_p.tbl_regression <- function(x, ...) {


  table_body <- x$table_body %>%
    group_by(.data$variable) %>%
    mutate(sort_col = min(.data$p.value, na.rm = TRUE)) %>%
    arrange(.data$sort_col) %>%
    select(-.data$sort_col) %>%
    ungroup()

  # replacing old table_body with new
  x$table_body <- table_body

  # adding indicator to output that add_overall was run on this data
  x$call_list <- c(x$call_list, list(sort_p = match.call()))

  # returning tbl_summary object
  return(x)

}

#' Sort variables in table by ascending p-values
#'
#' Sort variables in tables created by \code{\link{tbl_uvregression}} by ascending p-values
#'
#' @param x an object created using `tbl_uvregression` function
#' @param q logical argument. When TRUE will sort by the q-value column rather than the p-values
#' @param ... not used
#' @family tbl_uvregression tools
#' @author Karissa Whiting
#' @examples
#' tbl_uvglm_sort_p_ex <-
#'   trial %>%
#'   dplyr::select(age, marker, response, grade) %>%
#'   tbl_uvregression(
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) %>%
#'   sort_p()
#' @section Example Output:
#' \if{html}{\figure{tbl_uvglm_sort_p_ex.png}{options: width=50\%}}
#' @export
#'

sort_p.tbl_uvregression <- function(x, q = FALSE, ...) {

  # checking that add_q has been previously run if sort by q-values
  if (q == TRUE & is.null(x$call_list$add_q)) {
    stop("Before you sort by q-values, run add_q() to calculate the q-values")
  }

  sort_var <-  "p.value"
  if (q == TRUE) sort_var <- "q.value"

  table_body <-  x$table_body %>%
    group_by(.data$variable) %>%
    mutate(sort_col = min(.data[[sort_var]], na.rm = TRUE))

  table_body <- table_body %>%
    arrange(.data$sort_col) %>%
    select(-.data$sort_col) %>%
    ungroup()

  # replacing old table_body with new
  x$table_body <- table_body

  # adding indicator to output that add_overall was run on this data
  x$call_list <- c(x$call_list, list(sort_p = match.call()))

  # returning tbl_uvregression object
  return(x)

}
