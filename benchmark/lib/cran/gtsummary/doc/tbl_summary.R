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
                    Label = attr(.x, "label"))) %>%  gt::gt() %>%
  gt::tab_source_note("Includes mix of continuous, dichotomous, and categorical variables") %>%
  gt::fmt_markdown(columns = vars(Variable)) %>%
  gt::cols_align("left") %>%
  gt_compact_fun()

## ---- message=FALSE-----------------------------------------------------------
head(trial)

## -----------------------------------------------------------------------------
trial2 <- trial %>% select(trt, age, grade)

## ---- message=FALSE-----------------------------------------------------------
trial2 %>% tbl_summary()

## -----------------------------------------------------------------------------
trial2 %>% tbl_summary(by = trt) %>% add_p()

## ----echo=FALSE---------------------------------------------------------------
tibble::tribble(
  ~Argument,       ~Description,
  "`label`",       "specify the variable labels printed in table",
  "`type`",        "specify the variable type (e.g. continuous, categorical, etc.)",
  "`statistic`",   "change the summary statistics presented",
  "`digits`",      "number of digits the summary statistics will be rounded to",
  "`missing`",     "whether to display a row with the number of missing observations",
  "`missing_text`","text label for the missing number row",
  "`sort`",        "change the sorting of categorical levels by frequency",
  "`percent`",     "print column, row, or cell percentages"
) %>%
  gt::gt() %>%
  gt::fmt_markdown(columns = vars(Argument)) %>%
  gt_compact_fun()

## ---- eval = TRUE-------------------------------------------------------------
trial2 %>%
  tbl_summary(
    by = trt,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,
    label = grade ~ "Tumor Grade",
    missing_text = "(Missing)"
  )

## ---- echo = FALSE------------------------------------------------------------
tibble::tribble(
  ~`select_helper`, ~`varname`, ~`named_list`,
  '`all_continuous() ~ "{mean}"`', '`c("age", "marker") ~ "{mean}"`', '`list(age = "{mean}", marker = "{mean}")`',
  '`list(all_continuous() ~ "{mean}")`', '`c(age, marker) ~ "{mean}"`', NA_character_ ,
  NA_character_, '`list(c(age, marker) ~ "{mean}")`', NA_character_
) %>%
  gt::gt() %>%
  gt::fmt_markdown(everything()) %>%
  gt::cols_label(select_helper = gt::md("**Select with Helpers**"),
                 varname = gt::md("**Select by Variable Name**"),
                 named_list = gt::md("**Select with Named List**")) %>%
  gt::fmt_missing(columns = everything(), missing_text = "---") %>%
  gt::tab_options(table.font.size = "85%") %>%
  gt::cols_width(everything() ~ gt::px(390))

## ----out.width = "80%", echo = FALSE, fig.align='center'----------------------
# print picture of slide if in packagedown so not included in CRAN
if (pkgdown::in_pkgdown()) {
  knitr::include_graphics("https://github.com/ddsjoberg/gtsummary/raw/master/data-raw/crayon_images/crayon-selectors.png")
}

## ----echo = FALSE-------------------------------------------------------------
tibble::tribble(
  ~Function,             ~Description,
  "`add_p()`",           "add p-values to the output comparing values across groups",   
  "`add_overall()`",     "add a column with overall summary statistics",   
  "`add_n()`",           "add a column with N (or N missing) for each variable",   
  "`add_stat_label()`",  "add label for the summary statistics shown in each row",   
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

## ---- eval = TRUE-------------------------------------------------------------
trial2 %>%
  tbl_summary(by = trt) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Received**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  bold_labels()

## -----------------------------------------------------------------------------
trial2 %>%
  tbl_summary(by = trt, missing = "no") %>%
  add_n() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*This data is simulated*"))

## ---- eval=FALSE--------------------------------------------------------------
#  all_continuous()
#  all_categorical()

## -----------------------------------------------------------------------------
trial2 %>%
  select(age, trt) %>%
  tbl_summary(
    by = trt,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{N_nonmiss}",
                                     "{median} ({p25}, {p75})", 
                                     "{min}, {max}"),
    missing = "no"
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))

## ---- echo = FALSE------------------------------------------------------------
tibble::tribble(
  ~`Internal Object`, ~Description,
  "`.$table_body`",   "data frame that is printed as the gtsummary output table",
  "`.$table_header`", "contains instructions for formatting `.$table_body` when printed"
) %>%
  gt::gt() %>%
  gt::fmt_markdown(columns = everything()) %>%
  gt_compact_fun()

## -----------------------------------------------------------------------------
tbl_summary(trial2) %>% as_gt(return_calls = TRUE) %>% head(n = 4)

## ----as_gt2-------------------------------------------------------------------
tbl_summary(trial2, by = trt) %>%
  as_gt(include = -cols_align) %>%
  gt::tab_source_note(gt::md("*This data is simulated*"))

## -----------------------------------------------------------------------------
# loading the api data set
data(api, package = "survey")

## -----------------------------------------------------------------------------
svy_apiclus1 <- 
  survey::svydesign(
    id = ~dnum, 
    weights = ~pw, 
    data = apiclus1, 
    fpc = ~fpc
  ) 

## -----------------------------------------------------------------------------
svy_apiclus1 %>%
  tbl_svysummary(
    # stratify summary statistics by the "both" column
    by = both, 
    # summarize a subset of the columns
    include = c(api00, api99, both),
    # adding labels to table
    label = list(api00 ~ "API in 2000",
                 api99 ~ "API in 1999")
  ) %>%
  add_p() %>%   # comparing values by "both" column
  add_overall() %>%
  # adding spanning header
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Met Both Targets**")

## -----------------------------------------------------------------------------
Titanic %>%
  as_tibble() %>%
  survey::svydesign(data = ., ids = ~ 1, weights = ~ n) %>%
  tbl_svysummary(include = c(Age, Survived))

## -----------------------------------------------------------------------------
trial %>%
  tbl_cross(
    row = stage,
    col = trt,
    percent = "cell"
  ) %>%
  add_p()

