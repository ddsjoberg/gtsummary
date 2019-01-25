#' Italicize variable labels in Rmarkdown
#'
#' Italicize variable labels in `biostatR` objects by adding '_'
#' to each side of the string
#'
#' @param x `biostatR` object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{italicize_labels.fmt_table1}},
#' \code{\link{italicize_labels.fmt_regression}},
#' \code{\link{italicize_labels.fmt_uni_regression}}
#' @export
italicize_labels <- function(x, ...) UseMethod("italicize_labels")

#' Italicize or unitalicize labels for `fmt_table1` objects in Rmarkdown
#'
#' @param x `fmt_table1` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' trial %>% fmt_table1() %>% italicize_labels()
italicize_labels.fmt_table1 <- function(x, ...) {

  # italicize section
  x$table1$label <-
    ifelse(x$table1$row_type == "label",
      paste0("_", x$table1$label, "_"),
      x$table1$label
    )

  return(x)
}


#' Italicize or unitalicize labels for `fmt_regression` objects in Rmarkdown
#'
#' @param x `fmt_regression` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' lm(hp ~ factor(cyl), mtcars) %>% fmt_regression() %>% italicize_labels()
italicize_labels.fmt_regression <- function(x, ...) {

  # italicize section
  x$model_tbl$label <-
    ifelse(x$model_tbl$row_type == "label",
      paste0("_", x$model_tbl$label, "_"),
      x$model_tbl$label
    )

  return(x)
}

#' Italicize or unitalicize labels for `fmt_uni_regression` objects in Rmarkdown
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
#'   italicize_labels()
italicize_labels.fmt_uni_regression <- italicize_labels.fmt_regression
