## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE, warning=FALSE------------------------------------
library(gtsummary); library(gt); library(survival)
library(dplyr); library(stringr); library(purrr); library(forcats)

## -----------------------------------------------------------------------------
trial %>%
  select(trt, age, grade) %>%
  tbl_summary(
    by = trt, 
    missing = "no",
    statistic = all_continuous() ~ "{median} ({p25}, {p75})"
  ) %>%
  modify_header(stat_by = md("**{level}**<br>N =  {n} ({style_percent(p)}%)")) %>%
  add_n() %>%
  bold_labels() %>%
  modify_spanning_header(all_stat_cols() ~ "**Chemotherapy Treatment**")

## -----------------------------------------------------------------------------
trial %>%
  select(trt, age, marker) %>%
  tbl_summary(
    by = trt,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{N_nonmiss}",
                                     "{mean} ({sd})", 
                                     "{median} ({p25}, {p75})", 
                                     "{min}, {max}"),
    missing = "no"
  ) %>%
  italicize_levels()

## ---- message = FALSE---------------------------------------------------------
trial %>%
  select(response, age, grade) %>%
  mutate(response = factor(response, labels = c("No Tumor Response", "Tumor Responded"))) %>%
  tbl_summary(
    by = response, 
    missing = "no",
    label = list(age ~ "Patient Age", grade ~ "Tumor Grade")
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_q()

## -----------------------------------------------------------------------------
trial %>%
  select(response, age, grade) %>%
  mutate(
    response = factor(response, labels = c("No Tumor Response", "Tumor Responded")) %>% 
      fct_explicit_na(na_level = "Missing Response Status")
  ) %>%
  tbl_summary(
    by = response, 
    label = list(age ~ "Patient Age", grade ~ "Tumor Grade")
  )  

## -----------------------------------------------------------------------------
# imagine that each patient recieved Drug A and Drug B (adding ID showing their paired measurements)
trial_paired <-
  trial %>%
  select(trt, marker) %>%
  group_by(trt) %>%
  mutate(id = row_number()) %>%
  ungroup()

# you must first delete incomplete pairs from the data, then you can build the table
trial_paired %>%
  # delete missing marker values
  filter(!is.na(marker)) %>%
  # keep IDs with both measurements
  group_by(id) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  # summarize data
  tbl_summary(by = trt, include = -id) %>%
  add_p(test = marker ~ "paired.t.test", group = id)

## -----------------------------------------------------------------------------
# table summarizing data with no p-values
t0 <- trial %>%
  select(grade, age, response) %>%
  tbl_summary(by = grade, missing = "no") %>%
  modify_header(stat_by = md("**{level}**"))

# table comparing grade I and II
t1 <- trial %>%
  select(grade, age, response) %>%
  filter(grade %in% c("I", "II")) %>%
  tbl_summary(by = grade, missing = "no") %>%
  add_p() %>%
  modify_header(p.value ~ md("**I vs. II**")) %>%
  # hide summary stat columns
  modify_table_header(all_stat_cols(), hide = TRUE)

# table comparing grade I and II
t2 <- trial %>%
  select(grade, age, response) %>%
  filter(grade %in% c("I", "III")) %>%
  tbl_summary(by = grade, missing = "no") %>%
  add_p()  %>%
  modify_header(p.value ~ md("**I vs. III**")) %>%
  # hide summary stat columns
  modify_table_header(all_stat_cols(), hide = TRUE)

# merging the 3 tables together, and adding additional gt formatting
tbl_merge(list(t0, t1, t2)) %>%
  modify_spanning_header(
    list(
      all_stat_cols() ~ "**Tumor Grade**",
      starts_with("p.value") ~ "**p-values**"
    )
  )

## -----------------------------------------------------------------------------
# define function for lower and upper bounds of the mean CI
ll <- function(x) t.test(x)$conf.int[1]
ul <- function(x) t.test(x)$conf.int[2]

t1 <-
  trial %>%
  select(age, marker) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({sd})", missing = "no") %>%
  modify_header(stat_0 ~ "**Mean (SD)**")

t2 <-
  trial %>%
  select(age, marker) %>%
  tbl_summary(statistic = all_continuous() ~ "{ll}, {ul}", missing = "no") %>%
  modify_header(stat_0 ~ "**95% CI for Mean**")

tbl_merge(list(t1, t2)) %>%
  modify_footnote(everything() ~ NA_character_) %>%
  modify_spanning_header(everything() ~ NA_character_)

## -----------------------------------------------------------------------------
trial %>%
  select(response, age, grade) %>%
  tbl_uvregression(
    method = glm,
    y = response, 
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) %>%
  add_nevent()

## -----------------------------------------------------------------------------
gt_r1 <- glm(response ~ trt + grade, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)
gt_r2 <- coxph(Surv(ttdeath, death) ~ trt + grade, trial) %>%
  tbl_regression(exponentiate = TRUE)
gt_t1 <- trial[c("trt", "grade")] %>% 
  tbl_summary(missing = "no") %>% 
  add_n() %>%
  modify_header(stat_0 ~ "**n (%)**") %>%
  modify_footnote(stat_0 ~ NA_character_)

tbl_merge(
  list(gt_t1, gt_r1, gt_r2),
  tab_spanner = c(NA_character_, "**Tumor Response**", "**Time to Death**")
)

## -----------------------------------------------------------------------------
gt_model <-
  trial %>%
  select(ttdeath, death, stage, grade) %>%
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
    label = list(stage ~ "T Stage", grade ~ "Grade")
  ) %>%
  modify_header(stat_0 ~ "**Event N**") %>%
  modify_footnote(everything() ~ NA_character_)

tbl_merge(list(gt_eventn, gt_model)) %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_spanning_header(everything() ~ NA_character_)

## -----------------------------------------------------------------------------
tbl_reg <-
  trial %>%
  select(age, marker, trt) %>%
  tbl_uvregression(
    method = lm,
    x = trt,
    show_single_row = "trt",
    hide_n = TRUE
  ) %>%
  modify_header(list(
    label ~"**Model Outcome**",
    estimate ~ "**Treatment Coef.**"
  )) 

tbl_reg %>%
  modify_footnote(estimate ~ "Values larger than 0 indicate larger values in the Drug group.")

## -----------------------------------------------------------------------------
gt_sum <- 
  trial %>%
  select(age, marker, trt) %>%
  mutate(trt = fct_rev(trt)) %>%
  tbl_summary(by = trt, 
              statistic = all_continuous() ~ "{mean} ({sd})",
              missing = "no") %>%
  add_n() %>%
  modify_header(stat_by = md("**{level}**"))


tbl_merge(list(gt_sum, tbl_reg))  %>%
  modify_header(estimate_2 ~ "**Difference**") %>%
  modify_spanning_header(everything() ~ NA_character_)

## -----------------------------------------------------------------------------
my_tidy <- function(x, exponentiate =  FALSE, conf.level = 0.95, ...) {
  dplyr::bind_cols(
    broom::tidy(x, exponentiate = exponentiate, conf.int = FALSE),
    # calculate the confidence intervals, and save them in a tibble
    stats::confint.default(x) %>%
      tibble::as_tibble() %>%
      rlang::set_names(c("conf.low", "conf.high"))  )
}

lm(age ~ grade + marker, trial) %>%
  tbl_regression(tidy_fun = my_tidy)

