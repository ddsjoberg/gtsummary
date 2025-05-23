% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_significance_stars.R
\name{add_significance_stars}
\alias{add_significance_stars}
\title{Add significance stars}
\usage{
add_significance_stars(
  x,
  pattern = ifelse(inherits(x, c("tbl_regression", "tbl_uvregression")),
    "{estimate}{stars}", "{p.value}{stars}"),
  thresholds = c(0.001, 0.01, 0.05),
  hide_ci = TRUE,
  hide_p = inherits(x, c("tbl_regression", "tbl_uvregression")),
  hide_se = FALSE
)
}
\arguments{
\item{x}{(\code{gtsummary})\cr
A \code{'gtsummary'} object with a \code{'p.value'} column}

\item{pattern}{(\code{string})\cr
glue-syntax string indicating what to display in formatted column.
Default is \code{"{estimate}{stars}"} for regression summaries and \code{"{p.value}{stars}"} otherwise.
A footnote is placed on the first column listed in the pattern.
Other common patterns are
\code{"{estimate}{stars} ({conf.low}, {conf.high})"} and
\code{"{estimate} ({conf.low} to {conf.high}){stars}"}}

\item{thresholds}{(\code{numeric})\cr
Thresholds for significance stars. Default is \code{c(0.001, 0.01, 0.05)}}

\item{hide_ci}{(scalar \code{logical})\cr
logical whether to hide confidence interval. Default is \code{TRUE}}

\item{hide_p}{(scalar \code{logical})\cr
logical whether to hide p-value. Default is \code{TRUE} for regression summaries, and \code{FALSE} otherwise.}

\item{hide_se}{(scalar \code{logical})\cr
logical whether to hide standard error. Default is \code{FALSE}}
}
\value{
a 'gtsummary' table
}
\description{
Add significance stars to estimates with small p-values
}
\examples{
\dontshow{if ((identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("car", "parameters"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
tbl <-
  lm(time ~ ph.ecog + sex, survival::lung) |>
  tbl_regression(label = list(ph.ecog = "ECOG Score", sex = "Sex"))

# Example 1 ----------------------------------
tbl |>
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)

# Example 2 ----------------------------------
tbl |>
  add_significance_stars(
    pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
    hide_ci = TRUE, hide_se = TRUE
  ) |>
  modify_header(estimate = "**Beta (95\% CI)**") |>
  modify_abbreviation("CI = Confidence Interval")

# Example 3 ----------------------------------
# Use '  \n' to put a line break between beta and SE
tbl |>
  add_significance_stars(
    hide_se = TRUE,
    pattern = "{estimate}{stars}  \n({std.error})"
  ) |>
  modify_header(estimate = "**Beta  \n(SE)**") |>
  modify_abbreviation("SE = Standard Error") |>
  as_gt() |>
  gt::fmt_markdown(columns = everything()) |>
  gt::tab_style(
    style = "vertical-align:top",
    locations = gt::cells_body(columns = label)
  )

# Example 4 ----------------------------------
lm(marker ~ stage + grade, data = trial) |>
  tbl_regression() |>
  add_global_p() |>
  add_significance_stars(
    hide_p = FALSE,
    pattern = "{p.value}{stars}"
  )
\dontshow{\}) # examplesIf}
}
