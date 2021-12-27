## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## -----------------------------------------------------------------------------
#' Add together two numbers
#'
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}

## ---- eval = FALSE------------------------------------------------------------
#  #' Do values in a numeric vector fall in specified range?
#  #'
#  #' This is a shortcut for `x >= left & x <= right`, implemented
#  #' efficiently in C++ for local values, and translated to the
#  #' appropriate SQL for remote tables.
#  #'
#  #' @param x A numeric vector of values
#  #' @param left,right Boundary values
#  #' @export
#  #' @examples
#  #' between(1:12, 7, 9)
#  #'
#  #' x <- rnorm(1e2)
#  #' x[between(x, -1, 1)]
#  between <- function(x, left, right) {
#  }

