% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_kable.R
\name{as_kable}
\alias{as_kable}
\title{Convert gtsummary object to a kable object}
\usage{
as_kable(x, ..., include = everything(), return_calls = FALSE)
}
\arguments{
\item{x}{(\code{gtsummary})\cr
Object created by a function from the gtsummary package
(e.g. \link{tbl_summary} or \link{tbl_regression})}

\item{...}{Additional arguments passed to \code{\link[knitr:kable]{knitr::kable()}}}

\item{include}{Commands to include in output. Input may be a vector of
quoted or unquoted names. tidyselect and gtsummary select helper
functions are also accepted.
Default is \code{everything()}.}

\item{return_calls}{Logical. Default is \code{FALSE}. If \code{TRUE}, the calls are returned
as a list of expressions.}
}
\value{
A \code{knitr_kable} object
}
\description{
Output from \code{\link[knitr:kable]{knitr::kable()}} is less full featured compared to
summary tables produced with \href{https://gt.rstudio.com/index.html}{gt}.
For example, kable summary tables do not include indentation, footnotes,
or spanning header rows.

Line breaks (\verb{\\n}) are removed from column headers and table cells.
}
\details{
Tip: To better distinguish variable labels and level labels when
indenting is not supported, try \code{\link[=bold_labels]{bold_labels()}} or \code{\link[=italicize_levels]{italicize_levels()}}.
}
\examples{
trial |>
  tbl_summary(by = trt) |>
  bold_labels() |>
  as_kable()
}
\author{
Daniel D. Sjoberg
}
