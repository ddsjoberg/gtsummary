## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(gtsummary)
library(purrr); library(dplyr); library(tibble)

tbl_regression_ex <-
  lm(age ~ grade + marker, trial) %>%
  tbl_regression() %>%
  bold_p(t = 0.5) 

tbl_summary_ex <-
  trial %>%
  select(trt, age, grade, response) %>%
  tbl_summary(by = trt)

## -----------------------------------------------------------------------------
tbl_summary_ex$table_body

## ---- echo=FALSE--------------------------------------------------------------
tribble(
  ~Column, ~Description,
  "column", "Column name from table_body",
  "label", "Label that will be displayed (if column is displayed in output)",
  "hide", "Logical indicating whether the column is hidden in the output",
  "align", "Specifies the alignment/justification of the column, e.g. 'center' or 'left'",
  "missing_emdash", "Indicates the rows to include an em-dash for missing cells. For example `row_ref == TRUE` in `tbl_regression()`",
  "indent", "String of R code that results in a logical vector that specifies rows to indent, e.g. `row_type != 'label'`",
  "text_interpret", "the {gt} function that is used to interpret the column label",
  "bold", "For columns that bold rows conditionally, the column includes a string of R code that results in a logical vector indicating the rows to bold For example, `row_type == 'label'`",
  "italic", "For columns that italicize rows conditionally, the column includes a string of R code that results in a logical vector indicating the rows to italicize. For example, `row_type == 'label'`",
  "fmt_fun", "If the column needs to be formatted, this list column contains the function that performs the formatting.  Note, this is the function object; not the character name of a function.",
  "footnote_abbrev", "Lists the abbreviation footnotes for a table.  All abbreviation footnotes are collated into a single footnote.  For example, 'OR = Odds Ratio' and 'CI = Confidence Interval' appear in a single footnote.",
  "footnote", "Lists the footnotes that will appear for each column.",
  "spanning_header", "Includes text printed above columns as spanning headers. See `tbl_merge(...)$table_header` output for example of use."
) %>%
  knitr::kable() 

## -----------------------------------------------------------------------------
tbl_regression_ex$table_header

## -----------------------------------------------------------------------------
tbl_regression_ex %>%
  pluck("table_body") %>%
  select(variable, row_type, label)

## -----------------------------------------------------------------------------
gtsummary:::table_header_fill_missing(
  table_header = tibble(column = names(tbl_regression_ex$table_body))
) 

## ---- eval = FALSE------------------------------------------------------------
#  print.gtsummary <- function(x) {
#    if (getOption("gtsummary.print_engine") == "gt") {
#      return(as_gt(x) %>% print())
#    }
#    else if (getOption("gtsummary.print_engine") == "kable") {
#      return(as_kable(x) %>% print())
#    }
#  }
#  

