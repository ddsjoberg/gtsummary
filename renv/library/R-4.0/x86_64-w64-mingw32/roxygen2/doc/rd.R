## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## -----------------------------------------------------------------------------
#' Sum of vector elements.
#'
#' `sum` returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the [Summary()] group generic. For this to work properly,
#' the arguments `...` should be unnamed, and dispatch is on the
#' first argument.
sum <- function(..., na.rm = TRUE) {}

## -----------------------------------------------------------------------------
#' Sum of vector elements.
#' 
#' @description
#' `sum` returns the sum of all the values present in its arguments.
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the [Summary()] group generic. For this to work properly,
#' the arguments `...` should be unnamed, and dispatch is on the
#' first argument.

## -----------------------------------------------------------------------------
#' Sum of vector elements.
#'
#' `sum()` returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the [Summary] group generic. For this to work properly,
#' the arguments `...` should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param ... Numeric, complex, or logical vectors.
#' @param na.rm A logical scalar. Should missing values (including `NaN`)
#'   be removed?
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow 
#'   (<http://en.wikipedia.org/wiki/Integer_overflow>) occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   <http://en.wikipedia.org/wiki/Empty_sum> for more details.
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }
sum <- function(..., na.rm = TRUE) {}

## -----------------------------------------------------------------------------
#' An S4 class to represent a bank account.
#'
#' @slot balance A length-one numeric vector
Account <- setClass("Account",
  slots = list(balance = "numeric")
)

## -----------------------------------------------------------------------------
#' R6 Class Representing a Person
#'
#' @description
#' A person has a name and a hair color.
#'
#' @details
#' A person can also greet you.

Person <- R6::R6Class("Person",
public = list(

    #' @field name First or full name of the person.
    name = NULL,

    #' @field hair Hair color of the person.
    hair = NULL,

    #' @description
    #' Create a new person object.
    #' @param name Name.
    #' @param hair Hair color.
    #' @return A new `Person` object.
    initialize = function(name = NA, hair = NA) {
      self$name <- name
      self$hair <- hair
      self$greet()
    },

    #' @description
    #' Change hair color.
    #' @param val New hair color.
    #' @examples
    #' P <- Person("Ann", "black")
    #' P$hair
    #' P$set_hair("red")
    #' P$hair
    set_hair = function(val) {
      self$hair <- val
    },

    #' @description
    #' Say hi.
    greet = function() {
      cat(paste0("Hello, my name is ", self$name, ".\n"))
    }
  )
)

## -----------------------------------------------------------------------------
#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables
#' \describe{
#'   \item{price}{price in US dollars (\$326--\$18,823)}
#'   \item{carat}{weight of the diamond (0.2--5.01)}
#'   \item{cut}{quality of the cut (Fair, Good, Very Good, Premium, Ideal)}
#'   \item{color}{diamond colour, from D (best) to J (worst)}
#'   \item{clarity}{a measurement of how clear the diamond is (I1 (worst), SI2,
#'     SI1, VS2, VS1, VVS2, VVS1, IF (best))}
#'   \item{x}{length in mm (0--10.74)}
#'   \item{y}{width in mm (0--58.9)}
#'   \item{z}{depth in mm (0--31.8)}
#'   \item{depth}{total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)}
#'   \item{table}{width of top of diamond relative to widest point (43--95)}
#' }
#' @source <http://www.diamondse.info/>
"diamonds"

## -----------------------------------------------------------------------------
#' @details
#' The only function you're likely to need from roxygen2 is [roxygenize()]. 
#' Otherwise refer to the vignettes to see how to format the documentation.
#' @keywords internal
"_PACKAGE"

## -----------------------------------------------------------------------------
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.

## -----------------------------------------------------------------------------
#' @details # Warning
#' Do not operate heavy machinery within 8 hours of using this function.

## -----------------------------------------------------------------------------
#' @details # Warning
#' You must not call this function unless ...
#'
#' ## Exceptions
#' Apart from the following special cases...

## -----------------------------------------------------------------------------
#' @family aggregations
#' @seealso [prod()] for products, [cumsum()] for cumulative sums, and
#'   [colSums()]/[rowSums()] marginal sums over high-dimensional arrays.

## ---- eval = FALSE------------------------------------------------------------
#  list(
#    rd_family_title = list(aggregations = "Aggregation functions")
#  )

## -----------------------------------------------------------------------------
#' Foo bar generic
#'
#' @param x Object to foo.
foobar <- function(x) UseMethod("x")

#' @describeIn foobar Difference between the mean and the median
foobar.numeric <- function(x) abs(mean(x) - median(x))

#' @describeIn foobar First and last values pasted together in a string.
foobar.character <- function(x) paste0(x[1], "-", x[length(x)])

## -----------------------------------------------------------------------------
#' Basic arithmetic
#'
#' @param x,y numeric vectors.
add <- function(x, y) x + y

#' @rdname add
times <- function(x, y) x * y

## -----------------------------------------------------------------------------
#' Basic arithmetic
#'
#' @param x,y numeric vectors.
#' @name arith
NULL

#' @rdname arith
add <- function(x, y) x + y

#' @rdname arith
times <- function(x, y) x * y

## -----------------------------------------------------------------------------
#' @rdname arith
#' @order 2
add <- function(x, y) x + y

#' @rdname arith
#' @order 1
times <- function(x, y) x * y

## -----------------------------------------------------------------------------
my_params <- function() {
  c(
    "@param x An integer vector",
    "@param y A character vector"
  )
}

#' A title
#' 
#' @eval my_params()
#' @export
foo <- function(x, y) {
}

## -----------------------------------------------------------------------------
#' A title
#' 
#' @param x An integer vector
#' @param y A character vector
#' @export
foo <- function(x, y) {
}

## -----------------------------------------------------------------------------
my_note <- function(x) {
  paste0("\\note{", paste0(x, "\n", collapse =""), "}")
}

#' @evalRd my_note(c(
#'   "This is the first line",
#'   "This is the second line"
#' ))
NULL

## -----------------------------------------------------------------------------
#' @backref src/file.cpp
#' @backref src/file.h

