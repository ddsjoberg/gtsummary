% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_table_styling.R
\name{modify_table_styling}
\alias{modify_table_styling}
\title{Modify Table Styling}
\usage{
modify_table_styling(
  x,
  columns,
  rows = NULL,
  label = NULL,
  spanning_header = NULL,
  hide = NULL,
  footnote = NULL,
  footnote_abbrev = NULL,
  align = NULL,
  missing_symbol = NULL,
  fmt_fun = NULL,
  text_format = NULL,
  undo_text_format = FALSE,
  text_interpret = c("md", "html")
)
}
\arguments{
\item{x}{gtsummary object}

\item{columns}{vector or selector of columns in \code{x$table_body}}

\item{rows}{predicate expression to select rows in \code{x$table_body}.
Can be used to style footnote, formatting functions, missing symbols,
and text formatting. Default is \code{NULL}. See details below.}

\item{label}{string of column label(s)}

\item{spanning_header}{string with text for spanning header}

\item{hide}{logical indicating whether to hide column from output}

\item{footnote}{string with text for footnote}

\item{footnote_abbrev}{string with abbreviation definition, e.g.
\code{"CI = Confidence Interval"}}

\item{align}{string indicating alignment of column, must be one of
\code{c("left", "right", "center")}}

\item{missing_symbol}{string indicating how missing values are formatted.}

\item{fmt_fun}{function that formats the statistics in the
columns/rows in \verb{columns=} and \verb{rows=}}

\item{text_format}{string indicated which type of text formatting to apply to the rows and columns.
Must be one of \code{c("bold", "italic", "indent")}}

\item{undo_text_format}{rarely used. Logical that undoes the indent, bold,
and italic styling when \code{TRUE}}

\item{text_interpret}{string, must be one of \code{"md"} or \code{"html"}}
}
\description{
This is a function meant for advanced users to gain
more control over the characteristics of the resulting
gtsummary table by directly modifying \code{.$table_styling}
}
\details{
Review the
\href{http://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary definition}
vignette for information on \code{.$table_styling} objects.
}
\section{rows argument}{

The rows argument accepts a predicate expression that is used to specify
rows to apply formatting. The expression must evaluate to a logical when
evaluated in \code{x$table_body}. For example, to apply formatting to the age rows
pass \code{rows = variable == "age"}. A vector of row numbers is NOT acceptable.

A couple of things to note when using the \verb{rows=} argument.
\enumerate{
\item You can use saved objects to create the predicate argument, e.g.
\code{rows = variable == letters[1]}.
\item The saved object cannot share a name with a column in \code{x$table_body}.
The reason for this is that in \code{tbl_merge()} the columns are renamed,
and the renaming process cannot disambiguate the \code{variable} column from
an external object named \code{variable} in the following expression
\code{rows = .data$variable = .env$variable}.
}
}

\seealso{
\code{modify_table_body()}

See \href{http://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary internals vignette}
}
