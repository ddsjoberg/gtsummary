#' Bold variable labels in Rmarkdown
#'
#' Bold variable labels in `biostatR` objects by adding '__'
#' to each side of the string
#'
#' @param x `biostatR` object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{bold_labels.fmt_table1}},
#' \code{\link{bold_labels.fmt_regression}},
#' \code{\link{bold_labels.fmt_uni_regression}}
#' @export
bold_labels <- function(x, ...) UseMethod("bold_labels")

#' Bold or unbold variable labels for `fmt_table1` objects in Rmarkdown
#'
#' @param x `fmt_table1` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' trial %>% fmt_table1() %>% bold_labels()
bold_labels.fmt_table1 <- function(x, ...) {

  # bolding section
  x$table1$label <-
    ifelse(x$table1$row_type == "label",
      paste0("__", x$table1$label, "__"),
      x$table1$label
    )

  return(x)
}


#' Bold or unbold variable labels for `fmt_regression` objects in Rmarkdown
#'
#' @param x `fmt_regression` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' lm(hp ~ factor(cyl), mtcars) %>% fmt_regression() %>% bold_labels()
bold_labels.fmt_regression <- function(x, ...) {

  # bolding section
  x$model_tbl$label <-
    ifelse(x$model_tbl$row_type == "label",
      paste0("__", x$model_tbl$label, "__"),
      x$model_tbl$label
    )

  return(x)
}


#' Bold or unbold variable labels for `fmt_uni_regression` objects in Rmarkdown
#'
#' @param x `fmt_uni_regression` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' trial %>%
#'   fmt_uni_regression(
#'     method = "glm",
#'     y = "response",
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) %>%
#'   bold_labels()
bold_labels.fmt_uni_regression <- bold_labels.fmt_regression
