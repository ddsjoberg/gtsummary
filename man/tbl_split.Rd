% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_split.R
\name{tbl_split}
\alias{tbl_split}
\alias{tbl_split.gtsummary}
\alias{print.tbl_split}
\title{Split gtsummary table}
\usage{
tbl_split(x, ...)

\method{tbl_split}{gtsummary}(x, variables, ...)

\method{print}{tbl_split}(x, ...)
}
\arguments{
\item{x}{(\code{gtsummary})\cr
gtsummary table}

\item{...}{These dots are for future extensions and must be empty.}

\item{variables}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
variables at which to split the gtsummary table rows (tables
will be separated after each of these variables)}
}
\value{
\code{tbl_split} object
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}\cr
The \code{tbl_split} function splits a single gtsummary table into multiple tables.
Updates to the print method are expected.
}
\examples{
tbl <-
  tbl_summary(trial) |>
  tbl_split(variables = c(marker, grade))
}
