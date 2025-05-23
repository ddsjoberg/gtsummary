% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_vif.R
\name{add_vif}
\alias{add_vif}
\title{Add Variance Inflation Factor}
\usage{
add_vif(x, statistic = NULL, estimate_fun = label_style_sigfig(digits = 2))
}
\arguments{
\item{x}{\code{'tbl_regression'} object}

\item{statistic}{\code{"VIF"} (variance inflation factors, for models with no categorical terms) or one of/combination of \code{"GVIF"} (generalized variance inflation factors), \code{"aGVIF"} 'adjusted GVIF, i.e. \verb{GVIF^[1/(2*df)]} and/or \code{"df"} (degrees of freedom).
See \code{car::vif()} for details.}

\item{estimate_fun}{Default is \code{label_style_sigfig(digits = 2)}.}
}
\description{
Add the variance inflation factor (VIF) or
generalized VIF (GVIF) to the regression table.
Function uses \code{car::vif()} to calculate the VIF.
}
\examples{
\dontshow{if ((identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("cardx", "car", "broom.helpers"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Example 1 ----------------------------------
lm(age ~ grade + marker, trial) |>
  tbl_regression() |>
  add_vif()

# Example 2 ----------------------------------
lm(age ~ grade + marker, trial) |>
  tbl_regression() |>
  add_vif(c("aGVIF", "df"))
\dontshow{\}) # examplesIf}
}
\seealso{
Review \link[=syntax]{list, formula, and selector syntax} used throughout gtsummary
}
