% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style_ratio.R
\name{style_ratio}
\alias{style_ratio}
\title{Style ratios}
\usage{
style_ratio(
  x,
  digits = 2,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  prefix = "",
  suffix = "",
  na = NA_character_,
  ...
)
}
\arguments{
\item{x}{(\code{numeric})
Numeric vector}

\item{digits}{(\code{integer})\cr
Integer specifying the number of significant
digits to display for numbers below 1. Numbers larger than 1 will be be \code{digits + 1}.
Default is \code{digits = 2}.}

\item{big.mark}{(\code{string})\cr
Character used between every 3 digits to separate
hundreds/thousands/millions/etc.
Default is \code{","}, except when \code{decimal.mark = ","} when the default is a space.}

\item{decimal.mark}{(\code{string})\cr
The character to be used to indicate the numeric decimal point.
Default is \code{"."}  or \code{getOption("OutDec")}}

\item{prefix}{(\code{string})\cr
Additional text to display before the number.}

\item{suffix}{(\code{string})\cr
Additional text to display after the number.}

\item{na}{(\code{NA}/\code{string})\cr
Character to replace \code{NA} values with. Default is \code{NA_character}}

\item{...}{Arguments passed on to \code{base::format()}}
}
\value{
A character vector of styled ratios
}
\description{
When reporting ratios, such as relative risk or an odds ratio, we'll often
want the rounding to be similar on each side of the number 1.  For example,
if we report an odds ratio of 0.95 with a confidence interval of 0.70 to 1.24,
we would want to round to two decimal places for all values. In other words,
2 significant figures for numbers less than 1 and 3 significant figures 1 and
larger. \code{style_ratio()} performs significant figure-like rounding in this manner.
}
\examples{
c(0.123, 0.9, 1.1234, 12.345, 101.234, -0.123, -0.9, -1.1234, -12.345, -101.234) |>
  style_ratio()
}
\author{
Daniel D. Sjoberg
}
