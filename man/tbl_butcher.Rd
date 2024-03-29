% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_butcher.R
\name{tbl_butcher}
\alias{tbl_butcher}
\title{Reduce size of gtsummary objects}
\usage{
tbl_butcher(x, include = NULL)
}
\arguments{
\item{x}{a gtsummary object}

\item{include}{names of additional elements to retain in the gtsummary
object. \code{c("table_body", "table_styling")} will always be retained.
Default is \code{NULL}}
}
\value{
a gtsummary object
}
\description{
Some gtsummary objects can become large and the size becomes cumbersome
when working with the object.
The function removes all elements from a gtsummary object, except those
required to print the table. This may result in gtsummary functions
that add information or modify the table, such as \code{add_global_p()},
will no longer execute
after the excess elements have been removed (aka butchered). Of note,
the majority of \code{inline_text()} calls will continue to execute
properly.
}
\examples{
\donttest{
tbl_large <-
  trial \%>\%
  tbl_uvregression(
    y = age,
    method = lm
  )

tbl_butchered <-
  tbl_large \%>\%
  tbl_butcher()

# size comparison
object.size(tbl_large) \%>\% format(units = "Mb")
object.size(tbl_butchered) \%>\% format(units = "Mb")
}
}
