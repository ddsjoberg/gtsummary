## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE-----------------------------------------------------------
library(gtsummary); library(gt); library(dplyr)

trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p() 

## ---- message=TRUE------------------------------------------------------------
theme_gtsummary_journal(journal = "jama")

## ---- message=FALSE, echo=FALSE-----------------------------------------------
trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p() 

## ---- message=TRUE------------------------------------------------------------
theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

## ---- message=FALSE, echo=FALSE-----------------------------------------------
trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p() 

## ---- message=FALSE, echo = FALSE---------------------------------------------
set_gtsummary_theme(theme_gtsummary_journal(journal = "jama"))
set_gtsummary_theme(theme_gtsummary_compact())
set_gtsummary_theme(theme_gtsummary_language("es"))

## ---- message=FALSE, echo=FALSE-----------------------------------------------
trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p() 

## -----------------------------------------------------------------------------
reset_gtsummary_theme()

## -----------------------------------------------------------------------------
my_theme <-   
  list(
    # round large p-values to two places
    "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
    "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
    # report median (IQR) and n (percent) as default stats in `tbl_summary()`
    "tbl_summary-str:continuous_stat" = "{median} ({p25} - {p75})",
    "tbl_summary-str:categorical_stat" = "{n} ({p})"
  )

## ---- eval=FALSE--------------------------------------------------------------
#  set_gtsummary_theme(my_theme)

## ---- echo=FALSE--------------------------------------------------------------
gtsummary:::df_theme_elements %>%
  dplyr::filter(argument == FALSE) %>%
  dplyr::select(-argument) %>%
  dplyr::mutate(
    name = ifelse(!is.na(name), glue::glue("`{name}`"), NA_character_),
    example = ifelse(!is.na(example), glue::glue("`{example}`"), NA_character_)
  ) %>%
  dplyr::group_by(fn) %>%
  gt::gt() %>%
  gt::cols_align(columns = everything(), align = "left") %>%
  gt::cols_label(name = "Theme Element", desc = "Description",
                 example = "Example") %>%
  gt::fmt_markdown(columns = vars(name, desc, example)) %>%
  gt::fmt_missing(columns = everything(), missing_text = "") %>%
  gt::tab_options(table.font.size = 'small',
                  data_row.padding = gt::px(1),
                  row_group.padding = gt::px(1))


## ---- echo=FALSE--------------------------------------------------------------
gtsummary:::df_theme_elements %>%
  filter(argument == TRUE) %>%
  select(fn, name) %>%
  group_by(fn) %>%
  mutate(arg_list = paste0("`", name, "`", collapse = ", ")) %>%
  select(fn, arg_list) %>%
  distinct() %>%
  gt() %>%
  cols_label(arg_list = "Theme Element") %>%
  fmt_markdown(columns = vars(arg_list)) %>%
  tab_options(table.font.size = 'small',
                  data_row.padding = gt::px(1),
                  row_group.padding = gt::px(1))

