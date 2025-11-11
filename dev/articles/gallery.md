# FAQ + Gallery

FAQ and Gallery showing various tables possible with the {gtsummary}
package.

``` r
library(gtsummary)
```

------------------------------------------------------------------------

## Frequently Asked Questions

### Data Summary Tables

Headers, Labels and Formatting

1.  [How do I **modify column headers** in a table?](#table-header)

2.  [How do I **change the value** of the by levels?](#modify-pvalue)

3.  [How do I added a **spanning header** row to a
    table?](#table-header)

4.  [How do I change **variable labels**?](#modify-pvalue)

5.  [How do I **italicize or bold labels** in a table?](#table-header)

6.  [How do I **italicize or bold levels** in a table?](#continuous2)

7.  [How do I reduce **font size and cell padding** in a
    table?](#side-by-side)

Adding and Modifying Statistics

8.  [How do I add the **number of observations** to a summary
    table?](#table-header)

9.  [How do I show **additional summary statistics** as a new
    row?](#continuous2)

10. [How do I include a column for **missing values of a grouping
    variable**?](#include-missing)

11. [How do I add a column with the **confidence around the
    mean**?](#mean-ci)

12. [How do I add a column for the **difference between
    groups**?](#add_difference)

13. [How do I summarize a continuous variable by one, two (or more)
    categorical variables?](#cross_tab_continuous)

14. [How do I **stratify a summary table** by more than one
    variable?](#tbl_strata)

15. [How do I change the **p-values format**?](#modify-pvalue)

16. [How do I add a p-value for each group compared to a **single
    reference group**?](#compare-to-ref)

17. [How do I add a correction for **multiple
    testing**?](#modify-pvalue)

18. [How do I **combine** a summary table with a regression
    table?](#side-by-side)

Statistical Tests

19. [How do I do a **paired** t-test or McNemar’s test?](#paired-test)

------------------------------------------------------------------------

### Regression Tables

Headers, Labels and Formatting

1.  [How do I change the text in a **footnote**?](#tbl_uvregression-x)

2.  [How do I add **significance stars** for low
    p-values?](#add_significance_stars)

3.  [How do I **reduce font size** and cell padding in a
    table?](#side-by-side)

Creating and Combining Tables

4.  [How do I **combine** the results of two related regression models
    into one table?](#side-by-side)

5.  [How do I **combine** a regression table with a summary
    table?](#side-by-side)

6.  [How do I create a regression table for **multiple models** with the
    same covariate(s) and different outcomes?](#tbl_uvregression-x)

Adding and Modifying Statistics

7.  [How do I add total **event numbers** to a regression
    table?](#add_nevent)

8.  [How do I add event numbers for **each level** of a categorical
    covariate?](#add_nevent-levels)

9.  [How do I format a **Wald confidence interval**?](#wald-ci)

10. [How do I use **robust standard errors**?](#use_robust_errors)

------------------------------------------------------------------------

## Summary Tables

Add a spanning header over the group columns for increased clarity, and
modify column headers. Using
[`bold_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)
formats the labels as bold, but labels can also be italicized using
[`italicize_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
or combined to format with both bold and italics.

``` r
trial |> 
  tbl_summary(
    by = trt,
    include = c(age, grade),
    missing = "no",
    statistic = all_continuous() ~ "{median} ({p25}, {p75})"
  ) |> 
  modify_header(all_stat_cols() ~ "**{level}**  \nN = {n} ({style_percent(p)}%)") |> 
  add_n() |> 
  bold_labels() |> 
  modify_spanning_header(all_stat_cols() ~ "**Chemotherapy Treatment**")
```

[TABLE]

------------------------------------------------------------------------

Show continuous summary statistics on multiple lines. Levels are
italicized here using the
[`italicize_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)
function, but the
[`bold_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)
function can be used instead to create bold text, or both functions can
be used together to get text that is both bold and in italics.

``` r
trial |> 
  tbl_summary(
    by = trt,
    include = c(age, marker),
    type = all_continuous() ~ "continuous2",
    statistic =
      all_continuous() ~ c("{N_nonmiss}",
                           "{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}"),
    missing = "no"
  ) |> 
  italicize_levels()
```

[TABLE]

------------------------------------------------------------------------

Modify the function that formats the p-values, change variable labels,
updating tumor response header, and add a correction for multiple
testing.

``` r
trial |> 
  mutate(response = factor(response, labels = c("No Tumor Response", "Tumor Responded"))) |> 
  tbl_summary(
    by = response,
    include = c(age, grade),
    missing = "no",
    label = list(age ~ "Patient Age", grade ~ "Tumor Grade")
  ) |> 
  add_p(pvalue_fun = label_style_pvalue(digits = 2)) |> 
  add_q()
```

[TABLE]

------------------------------------------------------------------------

Include missing tumor response as column using
[`forcats::fct_na_value_to_level()`](https://forcats.tidyverse.org/reference/fct_na_value_to_level.html).

``` r
trial |> 
  mutate(
    response = 
      factor(response, labels = c("No Tumor Response", "Tumor Responded")) |> 
      forcats::fct_na_value_to_level(level = "Missing Response Status")
  ) |> 
  tbl_summary(
    by = response,
    include = c(age, grade),
    label = list(age ~ "Patient Age", grade ~ "Tumor Grade")
  )
```

[TABLE]

------------------------------------------------------------------------

Report treatment differences between two groups. This is often needed in
randomized trials. In this example, we report the difference in tumor
response and marker level between two chemotherapy treatments.

``` r
trial |> 
  tbl_summary(
    by = trt,
    include = c(response, marker),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}%"
    ),
    missing = "no"
  ) |> 
  add_difference() |> 
  add_n() |> 
  modify_header(all_stat_cols() ~ "**{level}**")
```

| **Characteristic**                                                                              | **N** | **Drug A**¹ | **Drug B**¹ | **Difference**² | **95% CI**² | **p-value**² |
|-------------------------------------------------------------------------------------------------|-------|-------------|-------------|-----------------|-------------|--------------|
| Tumor Response                                                                                  | 193   | 29%         | 34%         | -4.2%           | -18%, 9.9%  | 0.6          |
| Marker Level (ng/mL)                                                                            | 190   | 1.02 (0.89) | 0.82 (0.83) | 0.20            | -0.05, 0.44 | 0.12         |
| ¹ %; Mean (SD)                                                                                  |       |             |             |                 |             |              |
| ² 2-sample test for equality of proportions with continuity correction; Welch Two Sample t-test |       |             |             |                 |             |              |
| Abbreviation: CI = Confidence Interval                                                          |       |             |             |                 |             |              |

------------------------------------------------------------------------

Paired t-test and McNemar’s test. The data is expected in a long format
with 2 rows per participant.

``` r
# imagine that each patient received Drug A and Drug B (adding ID showing their paired measurements)
trial_paired <-
  trial |> 
  select(trt, marker, response) |> 
  mutate(.by = trt, id = dplyr::row_number())

# you must first delete incomplete pairs from the data, then you can build the table
trial_paired |> 
  # delete missing values
  tidyr::drop_na() |> 
  # keep IDs with both measurements
  dplyr::filter(.by = id, dplyr::n() == 2) |> 
  # summarize data
  tbl_summary(by = trt, include = -id) |> 
  add_p(
    test = list(marker ~ "paired.t.test",
                response ~ "mcnemar.test"),
    group = id
  )
```

[TABLE]

------------------------------------------------------------------------

Include p-values comparing all groups to a single reference group.

``` r
# table summarizing data with no p-values
small_trial <- trial |> select(grade, age, response)
t0 <- small_trial |> 
  tbl_summary(by = grade, missing = "no") |> 
  modify_header(all_stat_cols() ~ "**{level}**")

# table comparing grade I and II
t1 <- small_trial |> 
  dplyr::filter(grade %in% c("I", "II")) |> 
  tbl_summary(by = grade, missing = "no") |> 
  add_p() |> 
  modify_header(p.value ~ "**I vs. II**") |> 
  # hide summary stat columns
  modify_column_hide(all_stat_cols())

# table comparing grade I and II
t2 <- small_trial |> 
  dplyr::filter(grade %in% c("I", "III")) |> 
  tbl_summary(by = grade, missing = "no") |> 
  add_p() |> 
  modify_header(p.value = "**I vs. III**") |> 
  # hide summary stat columns
  modify_column_hide(all_stat_cols())

# merging the 3 tables together, and adding additional gt formatting
tbl_merge(list(t0, t1, t2)) |> 
  modify_spanning_header(
    all_stat_cols() ~ "**Tumor Grade**",
    starts_with("p.value") ~ "**p-values**"
  )
```

[TABLE]

------------------------------------------------------------------------

Add 95% confidence interval around the mean as an additional column

``` r
trial |> 
  tbl_summary(
    include = c(age, marker),
    statistic = all_continuous() ~ "{mean} ({sd})", 
    missing = "no"
  ) |> 
  modify_header(stat_0 = "**Mean (SD)**") |> 
  remove_footnote_header(stat_0) |> 
  add_ci()
```

| **Characteristic**                     | **Mean (SD)** | **95% CI** |
|----------------------------------------|---------------|------------|
| Age                                    | 47 (14)       | 45, 49     |
| Marker Level (ng/mL)                   | 0.92 (0.86)   | 0.79, 1.0  |
| Abbreviation: CI = Confidence Interval |               |            |

------------------------------------------------------------------------

It’s often needed to summarize a continuous variable by one, two, or
more categorical variables. The example below shows a table summarizing
a continuous variable by two categorical variables. To summarize by more
than two categorical variables, use `tbl_continuous` in conjunction with
`tbl_strata` (see an example of `tbl_strata` [here](#tbl_strata)).

``` r
trial |> 
  tbl_continuous(variable = marker, by = trt, include = grade) |> 
  modify_spanning_header(all_stat_cols() ~ "**Treatment Assignment**")
```

[TABLE]

------------------------------------------------------------------------

Build a summary table stratified by more than one variable.

``` r
trial |> 
  select(trt, grade, age, stage) |> 
  mutate(grade = paste("Grade", grade)) |> 
  tbl_strata(
    strata = grade,
    ~ .x |> 
      tbl_summary(by = trt, missing = "no") |> 
      modify_header(all_stat_cols() ~ "**{level}**")
  )
```

[TABLE]

------------------------------------------------------------------------

## Regression Tables

Include number of observations and the number of events in a univariate
regression table.

``` r
trial |> 
  tbl_uvregression(
    method = glm,
    y = response,
    include = c(age, grade),
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |> 
  add_nevent()
```

[TABLE]

------------------------------------------------------------------------

Include two related models side-by-side with descriptive statistics. We
also use the compact table theme that reduces cell padding and font
size.

``` r
gt_r1 <- glm(response ~ trt + grade, trial, family = binomial) |> 
  tbl_regression(exponentiate = TRUE)
gt_r2 <- survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade, trial) |> 
  tbl_regression(exponentiate = TRUE)
gt_t1 <- trial |> 
  tbl_summary(include = c(trt, grade), missing = "no") |> 
  add_n() |> 
  modify_header(stat_0 = "**n (%)**") |> 
  remove_footnote_header(stat_0)

theme_gtsummary_compact()
#> Setting theme "Compact"
tbl_merge(
  list(gt_t1, gt_r1, gt_r2),
  tab_spanner = c(NA_character_, "**Tumor Response**", "**Time to Death**")
)
```

[TABLE]

------------------------------------------------------------------------

Include the number of events at each level of a categorical predictor.

``` r
trial |> 
  tbl_uvregression(
    method = survival::coxph,
    y = survival::Surv(ttdeath, death),
    include = c(stage, grade),
    exponentiate = TRUE,
    hide_n = TRUE
  ) |> 
  add_nevent(location = "level")
```

[TABLE]

------------------------------------------------------------------------

Regression model where the covariate remains the same, and the outcome
changes.

``` r
trial |> 
  tbl_uvregression(
    method = lm,
    x = trt,
    show_single_row = "trt",
    hide_n = TRUE,
    include = c(age, marker)
  ) |> 
  modify_header(label = "**Model Outcome**",
                estimate = "**Treatment Coef.**") |> 
  modify_footnote_header("Values larger than 0 indicate larger values in the Drug B group.", columns = estimate)
```

| **Model Outcome**                                                  | **Treatment Coef.**¹ | **95% CI**  | **p-value** |
|--------------------------------------------------------------------|----------------------|-------------|-------------|
| Age                                                                | 0.44                 | -3.7, 4.6   | 0.8         |
| Marker Level (ng/mL)                                               | -0.20                | -0.44, 0.05 | 0.12        |
| ¹ Values larger than 0 indicate larger values in the Drug B group. |                      |             |             |
| Abbreviation: CI = Confidence Interval                             |                      |             |             |

------------------------------------------------------------------------

Implement a custom tidier to report Wald confidence intervals. The Wald
confidence intervals are calculated using
[`confint.default()`](https://rdrr.io/r/stats/confint.html).

``` r
my_tidy <- function(x, exponentiate = FALSE, conf.level = 0.95, ...) {
  dplyr::bind_cols(
    broom::tidy(x, exponentiate = exponentiate, conf.int = FALSE),
    # calculate the confidence intervals, and save them in a tibble
    dplyr::case_when(
      exponentiate ~ exp(confint.default(x)),
      .default = confint.default(x)
    ) |> 
      dplyr::as_tibble(.name_repair = "minimal") |> 
      rlang::set_names(c("conf.low", "conf.high"))
  )
}

lm(age ~ grade + marker, trial) |> 
  tbl_regression(tidy_fun = my_tidy)
```

[TABLE]

------------------------------------------------------------------------

Use significance stars on estimates with low p-values.

``` r
trial |> 
  tbl_uvregression(
    method = survival::coxph,
    y = survival::Surv(ttdeath, death),
    include = c(stage, grade),
    exponentiate = TRUE,
  ) |> 
  add_significance_stars()
```

[TABLE]

------------------------------------------------------------------------

To use robust standard errors in a regression model, the model is
prepared use usual, and the variance-covariance matrix of the model is
modified via an appropriate function, such as `vcovCL` from the sandwich
package.

``` r
dat <- trial |> 
  mutate(subject_id = dplyr::row_number(), .by = trt)
lmod <- glm(response ~ trt + grade, data = dat, family = binomial(link = "logit"))

cov <- sandwich::vcovCL(lmod, cluster = ~ subject_id, vcov_type = "HC0")
```

Once you have the robust variance-covariance matrix, you can use it with
`tidy_robust` to calculate adjusted confidence intervals and p-values.

Robust errors generally have only a small impact on the confidence
intervals and p-values. For demonstration purposes, we therefore show 2
digits for p-values.

A standard, non-robust regression table can be made as follows:

``` r
tbl_standard <- 
  tbl_regression(
    lmod,
    pvalue_fun = label_style_pvalue(digits = 2),
    exponentiate = TRUE
  )
tbl_standard
```

[TABLE]

In order to use the robust errors, pass the variance-covariance matrix
to the `tidy_robust` function, as shown below.

``` r
tbl_robust <- 
  tbl_regression(
    lmod,
    pvalue_fun = label_style_pvalue(digits = 2),
    exponentiate = TRUE,
    tidy_fun = \(x, ...) tidy_robust(x, vcov = cov, ...))
tbl_robust
```

[TABLE]

Comparing the tables side-by-side, we see that the confidence intervals
and p-values are very similar.

``` r
tbl_merge(
  list(tbl_standard, tbl_robust), 
  tab_spanner = c("**Standard errors**", "**Robust errors**")
)
```

[TABLE]

Global p-values can also be calculated with robust errors in the same
manner via the `tidy_wald_test` function. Again, the following example
demonstrates the non-robust approach and the robust approach
side-by-side.

``` r
tbl_merge(
  list(
    tbl_standard |> add_global_p(anova_fun = tidy_wald_test),
    tbl_robust |>
      add_global_p(anova_fun = \(x, ...) tidy_wald_test(x, vcov = cov))
  ), 
  tab_spanner = c("**Standard errors**", "**Robust errors**")
)
```

[TABLE]
