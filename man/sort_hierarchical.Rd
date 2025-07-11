% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sort_hierarchical.R
\name{sort_hierarchical}
\alias{sort_hierarchical}
\alias{sort_hierarchical.tbl_hierarchical}
\alias{sort_hierarchical.tbl_hierarchical_count}
\alias{sort_hierarchical.tbl_ard_hierarchical}
\title{Sort Hierarchical Tables}
\usage{
sort_hierarchical(x, ...)

\method{sort_hierarchical}{tbl_hierarchical}(x, sort = c("descending", "alphanumeric"), ...)

\method{sort_hierarchical}{tbl_hierarchical_count}(x, sort = c("descending", "alphanumeric"), ...)

\method{sort_hierarchical}{tbl_ard_hierarchical}(x, sort = c("descending", "alphanumeric"), ...)
}
\arguments{
\item{x}{(\code{tbl_hierarchical}, \code{tbl_hierarchical_count}, \code{tbl_ard_hierarchical})\cr
a hierarchical gtsummary table of class \code{'tbl_hierarchical'}, \code{'tbl_hierarchical_count'},
or \code{'tbl_ard_hierarchical'}.}

\item{...}{These dots are for future extensions and must be empty.}

\item{sort}{(\code{string})\cr
type of sorting to perform. Value must be one of:
\itemize{
\item \code{"alphanumeric"} - at each hierarchy level of the table, rows are ordered alphanumerically (i.e. A to Z)
by label text.
\item \code{"descending"} - at each hierarchy level of the table, count sums are calculated for each row and rows are
sorted in descending order by sum. If \code{sort = "descending"}, the \code{n} statistic is used to calculate row sums if
included in \code{statistic} for all variables, otherwise \code{p} is used. If neither \code{n} nor \code{p} are present in \code{x} for
all variables, an error will occur.
}

Defaults to \code{"descending"}.}
}
\value{
A \code{gtsummary} of the same class as \code{x}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}\cr

This function is used to sort hierarchical tables. Options for sorting criteria are:
\enumerate{
\item Descending - within each section of the hierarchy table, event rate sums are calculated for each row and rows are
sorted in descending order by sum (default).
\item Alphanumeric - rows are ordered alphanumerically (i.e. A to Z) by label text. By default, \code{\link[=tbl_hierarchical]{tbl_hierarchical()}}
sorts tables in alphanumeric order.
}
}
\examples{
\dontshow{if ((identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
theme_gtsummary_compact()
ADAE_subset <- cards::ADAE |>
  dplyr::filter(AEBODSYS \%in\% c("SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                                "EAR AND LABYRINTH DISORDERS")) |>
  dplyr::filter(.by = AEBODSYS, dplyr::row_number() < 20)

tbl <-
  tbl_hierarchical(
    data = ADAE_subset,
    variables = c(AEBODSYS, AEDECOD),
    by = TRTA,
    denominator = cards::ADSL |> mutate(TRTA = ARM),
    id = USUBJID,
    overall_row = TRUE
  ) |>
  add_overall()


# Example 1 - Descending Frequency Sort ------------------
sort_hierarchical(tbl)

# Example 2 - Alphanumeric Sort --------------------------
sort_hierarchical(tbl, sort = "alphanumeric")

reset_gtsummary_theme()
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=filter_hierarchical]{filter_hierarchical()}}
}
