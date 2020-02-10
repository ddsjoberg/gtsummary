## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = TRUE,
  comment = "#>"
)

## ----exit_early, include = FALSE, eval = !requireNamespace("gt")--------------
#  knitr::knit_exit()

## ---- echo=FALSE, comment=""--------------------------------------------------
if (!requireNamespace("gt", quietly = TRUE)) {
  usethis::ui_oops(paste(
    "The gt package is required to build the gtsummary gallery,",
    "and was not available\non this platform during the",
    "package building process. To view the gallery,",
    "visit the package website.\n\n",
    "{usethis::ui_path('http://www.danieldsjoberg.com/gtsummary/')}"
  ))
  knitr::knit_exit()
}

## ----setup, message = FALSE---------------------------------------------------
library(gtsummary); library(gt); library(survival)
library(dplyr); library(stringr); library(purrr); library(forcats)

## -----------------------------------------------------------------------------
trial[c("trt", "age", "grade")] %>%
  tbl_summary(by = trt, missing = "no") %>%
  modify_header(stat_by = md("**{level}** N =  {n} ({style_percent(p)}%)")) %>%
  add_n() %>%
  bold_labels() %>%
  as_gt() %>%
  tab_spanner(columns = starts_with("stat_"), md("**Chemotherapy Treatment**"))

## -----------------------------------------------------------------------------
trial[!is.na(trial$response), c("response", "age", "grade")] %>%
  mutate(response = factor(response, labels = c("No Tumor Response", "Tumor Responded"))) %>%
  tbl_summary(
    by = response, 
    missing = "no",
    label = list(vars(age) ~ "Patient Age", vars(grade) ~ "Tumor Grade")
  ) %>%
  add_p(pvalue_fun = partial(style_pvalue, digits = 2)) %>%
  add_q()

## -----------------------------------------------------------------------------
trial[c("response", "age", "grade")] %>%
  mutate(
    response = factor(response, labels = c("No Tumor Response", "Tumor Responded")) %>% 
      fct_explicit_na(na_level = "Missing Response Status")
  ) %>%
  tbl_summary(
    by = response, 
    label = list(vars(age) ~ "Patient Age", vars(grade) ~ "Tumor Grade")
  )  

## -----------------------------------------------------------------------------
trial[c("response", "age", "grade")] %>%
  tbl_uvregression(
    method = glm,
    y = response, 
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) %>%
  add_nevent()

## -----------------------------------------------------------------------------
gt_r1 <- glm(response ~ age + trt, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)
gt_r2 <- coxph(Surv(ttdeath, death) ~ age + trt, trial) %>%
  tbl_regression(exponentiate = TRUE)
gt_t1 <- trial[c("age", "trt")] %>% tbl_summary(missing = "no") %>% add_n()

tbl_merge(
  list(gt_t1, gt_r1, gt_r2),
  tab_spanner = c("**Summary Statistics**", "**Tumor Response**", "**Time to Death**")
)

## -----------------------------------------------------------------------------
gt_model <-
  trial[c("ttdeath", "death", "stage", "grade")] %>%
  tbl_uvregression(
    method = coxph,
    y = Surv(ttdeath, death), 
    exponentiate = TRUE,
    hide_n = TRUE
  )

gt_eventn <-
  trial %>%
  filter(death ==  1) %>%
  select(stage, grade) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n}",
    label = list(vars(stage) ~ "T Stage", vars(grade) ~ "Grade")
  ) %>%
  modify_header(stat_0 = md("**Event N**"))

tbl_merge(list(gt_eventn, gt_model)) %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_gt(exclude = "tab_spanner")

## -----------------------------------------------------------------------------
tbl_reg <-
  trial[c("age", "marker", "trt")] %>%
  tbl_uvregression(
    method = lm,
    x = trt,
    show_single_row = "trt",
    hide_n = TRUE
  ) %>%
  modify_header(
    label = md("**Model Outcome**"),
    estimate = md("**Treatment Coef.**")
  ) 

tbl_reg %>%
  as_gt() %>%
  tab_footnote(
    footnote = "Values larger than 0 indicate larger values in the Drug group.", 
    locations = cells_column_labels(columns = vars(estimate))
  )

## -----------------------------------------------------------------------------
gt_sum <- 
  trial[c("age", "marker", "trt")] %>%
  mutate(trt = fct_rev(trt)) %>%
  tbl_summary(by = trt, 
              statistic = all_continuous() ~ "{mean} ({sd})",
              missing = "no") %>%
  add_n() %>%
  modify_header(stat_by = md("**{level}**"))


tbl_merge(list(gt_sum, tbl_reg))  %>%
  modify_header(estimate_2 = md("**Difference**")) %>%
  as_gt(exclude = "tab_spanner")

