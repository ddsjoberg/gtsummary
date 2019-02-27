#' Bold significant p-values in Rmarkdown
#'
#' Bold p-values in `fmt_table1`, `fmt_regression`, and `fmt_uni_regression`
#' objects by adding '__' to each side of the string
#'
#' @param x `fmt_table1`, `fmt_regression`, or `fmt_uni_regression` object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{bold_p.fmt_table1}}, \code{\link{bold_p.fmt_regression}}
#' @export
bold_p <- function(x, ...) UseMethod("bold_p")

#' Bold or unbold p-values for `fmt_table1` objects in Rmarkdown
#'
#' @param x `fmt_table1` object
#' @param t Determines the threshold below which p-values get bolded. Default is 0.05.
#' @param q logical argument. When TRUE will bold or unbold the q-value column rather than the p-values
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' trial %>% fmt_table1(by = "trt") %>% add_comparison() %>% bold_p()
bold_p.fmt_table1 <- function(x, t = 0.05, q = FALSE, ...) {

  # if bolding q values, checking add_q() was previously run
  if (q == TRUE & !("add_q" %in% names(x$call_list))) {
    stop(
      "There are no q-values to bold. You need to use add_q() after add_comparison() and before using bold_p(q = TRUE)"
    )
  }
  # checking that p-values do exist
  if (!("add_comparison" %in% names(x$call_list))) {
    stop(
      "There are no p-values to bold. You need to use add_comparison() after fmt_table1() and before using bold_p()"
    )
  }

  # getting name of column that will be bolded/unbolded
  var_to_bold <- ifelse(q == TRUE, "qvalue", "pvalue")

  # list of variable names where p-value/q-value will be bolded, and indicies of table1 rows that will change
  var_sig <- x$meta_data$.variable[x$meta_data[[paste0(var_to_bold, "_exact")]] < t]
  var_sig_indicies <- (x$table1$.variable %in% var_sig) & x$table1$row_type == "label"

  # This replaces p-values/q-values for var_sig variables with bolded p-values
  x$table1[[var_to_bold]] <-
    ifelse(
      var_sig_indicies,
      paste0("__", x$table1[[var_to_bold]], "__"), # replacing sig rows with __ on each side
      x$table1[[var_to_bold]]
    )

  # Returns the table 1 object
  x$call_list <- c(x$call_list, list(bold_p = match.call()))
  return(x)
}

#' Bold or unbold p-values for `fmt_regression`  objects in Rmarkdown
#'
#' @param x `fmt_regression` object
#' @param t Determines the threshold below which p-values get bolded. Default is 0.05.
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' lm(mpg ~ hp + am, mtcars) %>%
#'   fmt_regression() %>%
#'   bold_p()
bold_p.fmt_regression <- function(x, t = 0.05, ...) {

  # This replaces p-values for var_sig variables with bolded p-values
  x$model_tbl <-
    x$model_tbl %>%
    dplyr::mutate_(
      pvalue = ~ ifelse(
        pvalue_exact < t,
        paste0("__", pvalue, "__"),
        pvalue
      )
    )

  return(x)
}

#' Bold or unbold p-values for `fmt_uni_regression` objects in Rmarkdown
#'
#' @param x `fmt_uni_regression` object
#' @param t Determines the threshold below which p-values get bolded. Default is 0.05.
#' @param q logical argument. When TRUE will bold or unbold the q-value column rather than the p-values
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#'
#' trial %>%
#'   fmt_uni_regression(
#'     method = "lm",
#'     y = "age"
#'   ) %>%
#'   bold_p(t = 0.20)
bold_p.fmt_uni_regression <- function(x, t = 0.05, q = FALSE, ...) {

  # if bolding q values, checking add_q() was previously run
  if (q == TRUE & !("add_q" %in% names(x$call_list))) {
    stop("You need to use add_q() after fmt_uni_regression() before using bold_p(q = TRUE)")
  }

  ### simplest scenario - without global p-values
  if (q == FALSE & !("global_pvalue" %in% colnames(x$meta_data))) {
    # This replaces p-values for var_sig variables with bolded p-values
    x$model_tbl <-
      x$model_tbl %>%
      dplyr::mutate_(pvalue = ~ ifelse(pvalue_exact < t,
        paste0("__", pvalue, "__"),
        pvalue
      ))
  }



  ### with global p-values
  if (q == FALSE &
    ("global_pvalue" %in% colnames(x$meta_data))) {
    var_sig <-
      x$meta_data %>%
      dplyr::filter_(~ global_pvalue_exact < t) %>%
      dplyr::pull("variable")

    x$model_tbl <-
      x$model_tbl %>%
      dplyr::mutate_(
        pvalue = ~ ifelse(
          variable %in% var_sig & row_type == "label",
          paste0("__", pvalue, "__"),
          pvalue
        )
      )
  }

  # This replaces p-values for var_sig variables with bolded p-values
  if (q == TRUE) {
    # This replaces p-values for var_sig variables with bolded p-values
    var_sig <-
      x$meta_data %>%
      dplyr::filter_(~ qvalue_exact < t) %>%
      dplyr::pull("variable")

    x$model_tbl <-
      x$model_tbl %>%
      dplyr::mutate_(
        qvalue = ~ ifelse(
          variable %in% var_sig & row_type == "label",
          paste0("__", qvalue, "__"),
          qvalue
        )
      )
  }

  # Returns the fmt_regression object
  return(x)
}
