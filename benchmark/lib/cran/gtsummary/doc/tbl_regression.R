## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

gt_compact_fun <- function(x) {
  gt::tab_options(x, 
                  table.font.size = 'small',
                  data_row.padding = gt::px(1),
                  summary_row.padding = gt::px(1),
                  grand_summary_row.padding = gt::px(1),
                  footnotes.padding = gt::px(1),
                  source_notes.padding = gt::px(1),
                  row_group.padding = gt::px(1))
}

## ----message = FALSE, warning=FALSE-------------------------------------------
# install.packages("gtsummary")
library(gtsummary)

## ---- echo = FALSE------------------------------------------------------------
trial %>%
  purrr::imap_dfr(
    ~tibble::tibble(Variable = glue::glue("`{.y}`"), 
                    Class = class(.x),
                    Label = attr(.x, "label"))) %>%
  gt::gt() %>%
  gt::tab_source_note("Includes mix of continuous, dichotomous, and categorical variables") %>%
  gt::fmt_markdown(columns = vars(Variable)) %>%
  gt::cols_align("left") %>%
  gt_compact_fun()

## ---- message=FALSE-----------------------------------------------------------
# build logistic regression model
m1 <- glm(response ~ age + stage, trial, family = binomial)

# view raw model results
summary(m1)$coefficients

## ---- message=FALSE-----------------------------------------------------------
tbl_regression(m1, exponentiate = TRUE)

## ---- echo = FALSE------------------------------------------------------------
tibble::tribble(
  ~Argument,          ~Description,
  "`label`",            "modify variable labels in table", 
  "`exponentiate`",     "exponentiate model coefficients",
  "`include`",          "names of variables to include in output. Default is all variables",
  "`show_single_row`",  "By default, categorical variables are printed on multiple rows. If a variable is dichotomous and you wish to print the regression coefficient on a single row, include the variable name(s) here.", 
  "`conf.level`",       "confidence level of confidence interval",  
  "`intercept`",        "indicates whether to include the intercept",  
  "`estimate_fun`",     "function to round and format coefficient estimates",  
  "`pvalue_fun`",       "function to round and format p-values",
  "`tidy_fun`",         "function to specify/customize tidier function"
) %>%
  gt::gt() %>%
  gt::fmt_markdown(columns = vars(Argument)) %>%
  gt_compact_fun()

## ----echo = FALSE-------------------------------------------------------------
tibble::tribble(
  ~Function,             ~Description,
  "`add_global_p()`",    "adds the global p-value for a categorical variables",   
  "`add_q()`",           "add a column of q values to control for multiple comparisons"   
) %>%
  gt::gt() %>%
  gt::fmt_markdown(columns = vars(Function)) %>%
  gt_compact_fun()

## ----echo = FALSE-------------------------------------------------------------
tibble::tribble(
  ~Function,                     ~Description,
  "`modify_header()`",           "update column headers",   
  "`modify_footnote()`",         "update column footnote",   
  "`modify_spanning_header()`",  "update spanning headers",   
  "`bold_labels()`",             "bold variable labels",  
  "`bold_levels()`",             "bold variable levels",  
  "`italicize_labels()`",        "italicize variable labels",  
  "`italicize_levels()`",        "italicize variable levels",  
  "`bold_p()`",                  "bold significant p-values"
) %>%
  gt::gt() %>%
  gt::fmt_markdown(columns = vars(Function)) %>%
  gt_compact_fun()

## -----------------------------------------------------------------------------
m1 %>%
  tbl_regression(exponentiate = TRUE) %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*This data is simulated*"))

## ---- eval=TRUE---------------------------------------------------------------
# format results into data frame with global p-values
m1 %>%
  tbl_regression(
    exponentiate = TRUE, 
    pvalue_fun = ~style_pvalue(.x, digits = 2),
  ) %>% 
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

## ----tbl_uvregression---------------------------------------------------------
trial %>%
  select(response, age, grade) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)
  ) %>%
  add_global_p() %>%  # add global p-value 
  add_nevent() %>%    # add number of events of the outcome
  add_q() %>%         # adjusts global p-values for multiple testing
  bold_p() %>%        # bold p-values under a given threshold (default 0.05)
  bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
  bold_labels()

