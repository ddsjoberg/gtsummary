## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE------------------------------------------------------
library(dplyr)
library(knitr)
library(kableExtra)
library(gtsummary)

# printing trial data
head(trial) %>% kable()

## ---- message=FALSE------------------------------------------------------
trial2 =
  trial %>%
  select(trt, marker, stage)

fmt_table1(trial2)

## ------------------------------------------------------------------------
fmt_table1(trial2, by = "trt") %>% add_comparison()

## ------------------------------------------------------------------------
trial2 %>%
  # build base table 1
  fmt_table1(
    by = "trt",
    # change variable labels
    label = list(
      marker = "Pretreatment Marker Level, ng/mL",
      stage = "Clinical T Stage"
      ),
    # change statistics printed in table
    statistic = list(
      continuous = "{mean} ({sd})",
      categorical = "{n} / {N} ({p}%)"
    ),
    missing = "no"
  ) %>%
  # add p-values to table, perform t-test for the marker,
  # and round large pvalues to two decimal place
  add_comparison(
    test = list(marker = "t.test"),
    pvalue_fun = function(x) fmt_pvalue(x, digits = 2)
  ) %>%
  # add q-values (p-values adjusted for multiple testing)
  add_q(pvalue_fun = function(x) fmt_pvalue(x, digits = 2)) %>%
  # add overall column
  add_overall() %>%
  # add column with N
  add_n() %>%
  # add statistic labels
  add_stat_label() %>%
  # bold variable labels, italicize levels
  bold_labels() %>%
  italicize_levels() %>%
  # bold p-values under a given threshold (default 0.05)
  bold_p(t = 0.2) %>%
  # include percent in headers
  modify_header(
    stat_by = c("{level}", "N = {n} ({p}%)"),
    stat_overall = c("All Patients", "N = {N} (100%)")
  )

## ------------------------------------------------------------------------
tab1 = fmt_table1(trial2, by = "trt")
tab1

## ---- message=FALSE------------------------------------------------------
#get data frame from fmt_table1 object
tab1_df <- as_tibble(tab1)

## ---- message=FALSE------------------------------------------------------
# knit pretty table
tab1 %>%
  bold_labels() %>% # bold labels in here if you want
  as_tibble() %>%
  kable(
    row.names = FALSE,
    caption = "Table 1: Summary of Patient and Clinical Variables"
  ) %>%
  # Below, using kableExtra functions to do things like change table style, add 
  # grouped column header, footnote, and indent variable categories
  kable_styling(
    bootstrap_options = c("striped", "condensed", "hover"), #popular bootstrap styles
    font_size = 16,
    full_width = FALSE
  ) %>%
  add_header_above(c(" " = 1, "Treatment assignment" = 2)) %>%
  footnote(
    general = "Isn't this footnote so nice?",
    number = c("You can also add numbered or lettered footnotes", "Which is great.")
  ) %>%
  add_indent(indent_key(tab1)) 

## ------------------------------------------------------------------------
t = fmt_table1(trial2, by = "trt") %>% add_comparison()
ls(t)

## ------------------------------------------------------------------------
print.listof(t)

