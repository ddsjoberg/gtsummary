% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_nevent.tbl_survfit.R
\name{add_nevent.tbl_survfit}
\alias{add_nevent.tbl_survfit}
\title{Add event N}
\usage{
\method{add_nevent}{tbl_survfit}(x, ...)
}
\arguments{
\item{x}{object of class 'tbl_survfit'}

\item{...}{Not used}
}
\description{
For each \code{survfit()} object summarized with \code{tbl_survfit()} this function
will add the total number of events observed in a new column.
}
\examples{
\dontshow{if (gtsummary:::is_pkg_installed(c("survival", "broom"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(survival)
fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
fit2 <- survfit(Surv(ttdeath, death) ~ trt, trial)

# Example 1 ----------------------------------
list(fit1, fit2) |>
  tbl_survfit(times = c(12, 24)) |>
  add_n() |>
  add_nevent()
\dontshow{\}) # examplesIf}
}
\seealso{
Other tbl_survfit tools: 
\code{\link{add_p.tbl_survfit}()}
}
\concept{tbl_survfit tools}
