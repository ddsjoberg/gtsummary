#' Italicize variable levels in Rmarkdown
#'
#' Italicize variable levels in `biostatR` objects by adding '_'
#' to each side of the string
#'
#' @param x `biostatR` object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{italicize_levels.fmt_table1}},
#' \code{\link{italicize_levels.fmt_regression}},
#' \code{\link{italicize_levels.fmt_uni_regression}}
#' @export
italicize_levels <- function(x, ...) UseMethod("italicize_levels")

#' Italicize or unitalicize levels for `fmt_table1` objects in Rmarkdown
#'
#' @param x `fmt_table1` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' trial %>% fmt_table1() %>% italicize_levels()
italicize_levels.fmt_table1 <- function(x, ...) {

  # italicize section
  x$table1$label <-
    ifelse(x$table1$row_type %in% c("level", "missing"),
      paste0("_", x$table1$label, "_"),
      x$table1$label
    )

  return(x)
}


#' Italicize or unitalicize levels for `fmt_regression` objects in Rmarkdown
#'
#' @param x `fmt_regression` object
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' lm(hp ~ factor(cyl), mtcars) %>% fmt_regression() %>% italicize_levels()
italicize_levels.fmt_regression <- function(x, ...) {

  # Italicize section
  x$model_tbl$label <-
    ifelse(x$model_tbl$row_type == "level",
      paste0("_", x$model_tbl$label, "_"),
      x$model_tbl$label
    )

  return(x)
}

#' Italicize or unitalicize levels for `fmt_uni_regression` objects in Rmarkdown
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
#'   italicize_levels()
italicize_levels.fmt_uni_regression <- italicize_levels.fmt_regression
