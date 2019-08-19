## ----setup, include = FALSE----------------------------------------------
library(gtsummary)
library(dplyr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE------------------------------------------------------
# build logistic regression model
m1 = glm(response ~ age + stage + grade, trial, family = binomial(link = "logit"))

# format results into data frame
fmt_regression(m1, exponentiate = TRUE)

## ---- message=FALSE------------------------------------------------------
# format results into data frame with global p-values
fmt_regression(m1, exponentiate = TRUE) %>% 
  add_global()

## ---- message=FALSE------------------------------------------------------
# format results into data frame with global p-values
fmt_regression(m1, exponentiate = TRUE) %>% 
  bold_labels() %>% 
  italicize_levels()

## ---- message=FALSE------------------------------------------------------
# format results into data frame with global p-values
fmt_regression(
  m1, 
  exponentiate = TRUE, 
  pvalue_fun = function(x) fmt_pvalue(x, digits = 2)
) 

## ---- message=FALSE------------------------------------------------------
# format results into data frame with global p-values
fmt_regression(m1, exponentiate = TRUE) %>% 
  modify_header(label = " ", est = "Odds Ratio", ci = "95% Confidence Interval")

## ------------------------------------------------------------------------
fmt_m1 = fmt_regression(m1, exponentiate = TRUE)
fmt_m1

## ---- message=FALSE------------------------------------------------------
#make data frame out of fmt_regression object
mod1_df <- as_tibble(fmt_m1)

## ---- message=FALSE------------------------------------------------------
library(knitr)
library(kableExtra)

# knit pretty table with indent into HTM
fmt_m1 %>%
  bold_labels() %>%
  as_tibble() %>%
  kable(
    row.names = FALSE,
    caption = "Regression Summary"
  ) %>%
  # Below, using kableExtra functions to do things like change table style, add 
  # grouped column header, footnote, and indent variable categories
  kable_styling(
    bootstrap_options = c("striped", "condensed", "hover"), #popular bootstrap styles
    font_size = 16,
    full_width = F
  ) %>%
  add_header_above(c(" " = 1, "MVA" = 3)) %>% #add grouped header if needed
  footnote(general = "Values calculated using logistic regression predicting response.")%>%
  add_indent(indent_key(fmt_m1)) 

## ------------------------------------------------------------------------
ls(fmt_m1)

## ------------------------------------------------------------------------
print.listof(fmt_m1)

## ----fmt_uni_regression--------------------------------------------------
# rounding pvalues to 2 decimal places, 
# and adding global p-values,
# and bold_labels
fmt_uni_regression(
 trial,
 method = "glm",
 y = "response",
 method.args = list(family = binomial),
 exponentiate = TRUE,
 pvalue_fun = function(x) fmt_pvalue(x, digits = 2)
) %>%
  # overrides the default that shows p-values for each level
  add_global() %>%
  # adjusts global p-values for multiple testing (default method: FDR)
  add_q() %>%
  # bold p-values under a given threshold (default 0.05)
  bold_p() %>% 
  # now bold q-values under the threshold of 0.10
  bold_p(
    t = 0.10,
    q = TRUE
  ) %>%
  bold_labels()

