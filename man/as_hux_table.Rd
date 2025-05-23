% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_hux_table.R
\name{as_hux_table}
\alias{as_hux_table}
\alias{as_hux_xlsx}
\title{Convert gtsummary object to a huxtable object}
\usage{
as_hux_table(x, include = everything(), return_calls = FALSE)

as_hux_xlsx(x, file, include = everything(), bold_header_rows = TRUE)
}
\arguments{
\item{x}{(\code{gtsummary})\cr
An object of class \code{"gtsummary"}}

\item{include}{Commands to include in output. Input may be a vector of
quoted or unquoted names. tidyselect and gtsummary select helper
functions are also accepted.
Default is \code{everything()}.}

\item{return_calls}{Logical. Default is \code{FALSE}. If \code{TRUE}, the calls are returned
as a list of expressions.}

\item{file}{File path for the output.}

\item{bold_header_rows}{(scalar \code{logical})\cr
logical indicating whether to bold header rows. Default is \code{TRUE}}
}
\value{
A \{huxtable\} object
}
\description{
Function converts a gtsummary object to a huxtable object.
A user can use this function if they wish to add customized formatting
available via the huxtable functions. The huxtable package supports output
to PDF via LaTeX, as well as HTML and Word.
}
\section{Excel Output}{


Use the \code{as_hux_xlsx()} function to save a copy of the table in an excel file.
The file is saved using \code{huxtable::quick_xlsx()}.
}

\examples{
\dontshow{if (gtsummary:::is_pkg_installed("huxtable")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p() |>
  as_hux_table()
\dontshow{\}) # examplesIf}
}
\author{
David Hugh-Jones, Daniel D. Sjoberg
}
