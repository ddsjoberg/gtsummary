% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style_sigfig.R
\name{style_sigfig}
\alias{style_sigfig}
\title{Style significant figure-like rounding}
\usage{
style_sigfig(
  x,
  digits = 2,
  scale = 1,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  prefix = "",
  suffix = "",
  na = NA_character_,
  ...
)
}
\arguments{
\item{x}{Numeric vector}

\item{digits}{Integer specifying the minimum number of significant
digits to display}

\item{scale}{(scalar \code{numeric})\cr
A scaling factor: \code{x} will be multiplied by scale before formatting.}

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
A character vector of styled numbers
}
\description{
Converts a numeric argument into a string that has been rounded to a
significant figure-like number. Scientific notation output
is avoided, however, and additional significant figures may be displayed for
large numbers.  For example, if the number of significant digits
requested is 2, 123 will be displayed (rather than 120 or 1.2x10^2).
}
\section{Details}{

\itemize{
\item Scientific notation output is avoided.
\item If 2 significant figures are requested, the number is rounded to no more than 2 decimal places.
For example, a number will be rounded to 2 decimals places when \code{abs(x) < 1},
1 decimal place when \code{abs(x) >= 1 & abs(x) < 10},
and to the nearest integer when \code{abs(x) >= 10}.
\item Additional significant figures
may be displayed for large numbers. For example, if the number of
significant digits requested is 2,
123 will be displayed (rather than 120 or 1.2x10^2).
}
}

\examples{
c(0.123, 0.9, 1.1234, 12.345, -0.123, -0.9, -1.1234, -132.345, NA, -0.001) \%>\%
  style_sigfig()
}
\seealso{
Other style tools: 
\code{\link{label_style}}
}
\author{
Daniel D. Sjoberg
}
\concept{style tools}
