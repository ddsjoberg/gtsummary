# ARD-first Tables

``` r
library(gtsummary)
library(cards)
theme_gtsummary_compact()
#> Setting theme "Compact"
```

## Introduction

Analysis Results Datasets (ARDs) are part of the [CDISC Analysis Results
Standard](https://www.cdisc.org/standards/foundational/analysis-results-standard),
which aims to facilitate automation, reproducibility, reusability, and
traceability of analysis results data (ARD). ARDs are highly structured
and generalized data frame objects in which store the results of both
simple and complex statistical results. The {gtsummary} package utilizes
ARDs (via the {cards} and {cardx} packages) to perform all calculations.

In this tutorial, we will review how to use the native {gtsummary}
functions to build standard and highly customized tables. There are two
basic approaches to creating summary tables utilizing ARDs:

1.  As ARDs power every calculation and tabulation in the {gtsummary},
    ARDs can be extracted from any table created with the `tbl_*()`
    functions (and their add-ons).
2.  One can also create ARDs first, then pass the ARD to a `tbl_ard_*()`
    function that will convert the ARD to a summary table.

Both methods will be covered in this tutorial.

## Extract ARD summary table

Many standard tables are simple to create with `tbl_*()` functions, such
as, demographics tables, adverse event summary tables, and more. After
the table is created, the ARD can be extracted using the
[`gather_ard()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/gather_ard.md)
function.

#### Demographics Summary

Begin by building the summary table with
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).

``` r
tbl_demo <-
  ADSL |> 
  dplyr::mutate(AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80"))) |> 
  # building summary table by treatment arm
  tbl_summary(
    by = ARM, 
    include = c(AGE, AGEGR1, SEX),
    type = list(AGE = "continuous2"),
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
    label = list(AGEGR1 = "Age Group")
  ) |> 
  # add an overall column with all treatments combined
  add_overall() |> 
  add_stat_label()
tbl_demo
```

[TABLE]

Now that we have a summary table, we can extract and save the ARD.

``` r
gather_ard(tbl_demo) |> bind_ard()
#> ℹ 8 rows with duplicated statistic values have been removed.
#> • See cards::bind_ard(.distinct) (`?cards::bind_ard()`) for details.
#> {cards} data frame: 167 x 12
#>    group1 group1_level variable variable_level stat_name stat_label  stat
#> 1     ARM      Placebo   AGEGR1            <65         n          n    14
#> 2     ARM      Placebo   AGEGR1            <65         N          N    86
#> 3     ARM      Placebo   AGEGR1            <65         p          % 0.163
#> 4     ARM      Placebo   AGEGR1          65-80         n          n    42
#> 5     ARM      Placebo   AGEGR1          65-80         N          N    86
#> 6     ARM      Placebo   AGEGR1          65-80         p          % 0.488
#> 7     ARM      Placebo   AGEGR1            >80         n          n    30
#> 8     ARM      Placebo   AGEGR1            >80         N          N    86
#> 9     ARM      Placebo   AGEGR1            >80         p          % 0.349
#> 10    ARM      Placebo      SEX              F         n          n    53
#> ℹ 157 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 5 more variables: context, fmt_fun, warning, error, gts_column
```

#### Adverse Event Summary

The adverse event example is similar to the example above; instead of
using
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
we use
[`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md).

``` r
tbl_ae <-
  ADAE |>
  # filter the data frame to print fewer AEs
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:3],
    AETERM %in% unique(cards::ADAE$AETERM)[1:3]
  ) |> 
  # create AE summary table
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
    overall_row = TRUE,
    label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
  ) |> 
  # add a column with overall estimates
  add_overall()
tbl_ae
```

[TABLE]

``` r

# return ARDs
gather_ard(tbl_ae) |> bind_ard()
#> {cards} data frame: 82 x 15
#>    group1 group1_level group2 group2_level                     variable
#> 1    <NA>                <NA>                                      TRTA
#> 2    <NA>                <NA>                                      TRTA
#> 3    <NA>                <NA>                                      TRTA
#> 4    <NA>                <NA>                                      TRTA
#> 5    <NA>                <NA>                                      TRTA
#> 6    <NA>                <NA>                                      TRTA
#> 7    <NA>                <NA>                                      TRTA
#> 8    <NA>                <NA>                                      TRTA
#> 9    <NA>                <NA>                                      TRTA
#> 10   TRTA      Placebo   <NA>              ..ard_hierarchical_overall..
#>    variable_level stat_name stat_label  stat stat_fmt
#> 1         Placebo         n          n    86       86
#> 2         Placebo         N          N   254      254
#> 3         Placebo         p          % 0.339     33.9
#> 4       Xanomeli…         n          n    84       84
#> 5       Xanomeli…         N          N   254      254
#> 6       Xanomeli…         p          % 0.331     33.1
#> 7       Xanomeli…         n          n    84       84
#> 8       Xanomeli…         N          N   254      254
#> 9       Xanomeli…         p          % 0.331     33.1
#> 10           TRUE         n          n    16       16
#> ℹ 72 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 5 more variables: context, fmt_fun, warning, error, gts_column
```

#### Other summaries

Other summary functions available include

- [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)
  for cross tabulations
- [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)
  for summaries of continuous variables stratified by two other
  categorical variables
- [`tbl_wide_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_wide_summary.md)
  for statistics represented in a wide table format, that is statistics
  in separate columns
- [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
  for survival endpoint summaries
- [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  for regression model summaries
- [`tbl_likert()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_likert.md)
  for Likert-scale summaries

## ARD-first summary tables

While the above examples are simple, there are cases when we must use a
two step process of building our ARD, then converting the ARD to a
summary table. Two common instances where one would want to create a
table from an ARD are 1. for tables that include more complex
statistical results, 2. for re-use purposes (e.g. extract an ARD from a
previously built table, and modify it for another purpose). For this
ARD-first approach, {gtsummary} has `tbl_ard_*()` functions to generate
summary tables.

- [`tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_summary.md)
  for ARDs with descriptive statistics for continuous, categorical and
  dichotomous variables
- [`tbl_ard_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_continuous.md)
  for ARDs summarizing continuous variables
- [`tbl_ard_wide_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_wide_summary.md)
  for ARD statistics represented in a wide table format - in separate
  columns
- [`tbl_ard_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_hierarchical.md)
  for ARDs containing nested or hierarchical data structures

#### Demographics Summary

In this example, we will build a simple demographics and baseline
characteristics table as outlined in the FDA Standard Safety Tables
Guidelines. This table has variables: a continuous variable summary for
AGE, a categorical variable summaries for AGEGR1 and SEX.

**Data ➡ ARD**

The {cards} package can be utilized to create the ARD from a data frame.
The package includes functions
[`ard_summary()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_summary.html)
for continuous summaries,
[`ard_tabulate()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_tabulate.html)
for categorical summaries, and
[`ard_tabulate_value()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_tabulate_value.html)
for dichotomous variables (and more).

The package also exports a helper function,
[`ard_stack()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack.html)
to simultaneously build these summaries along with optional ancillary
results for a nicer display.

``` r
ard_demo <-
  ADSL |> 
  dplyr::mutate(AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80"))) |> 
  ard_stack(
    # stratify all results by ARM
    .by = ARM, 
    # these are the results that will be calculated
    ard_summary(variables = "AGE"),
    ard_tabulate(variables = c("AGEGR1","SEX")),
    # optional arguments for additional results
    .attributes = TRUE,
    .total_n = TRUE,
    .overall = TRUE
  )
ard_demo
#> {cards} data frame: 111 x 11
#>    group1 group1_level variable variable_level stat_name stat_label   stat
#> 1     ARM      Placebo      AGE                        N          N     86
#> 2     ARM      Placebo      AGE                     mean       Mean 75.209
#> 3     ARM      Placebo      AGE                       sd         SD   8.59
#> 4     ARM      Placebo      AGE                   median     Median     76
#> 5     ARM      Placebo      AGE                      p25         Q1     69
#> 6     ARM      Placebo      AGE                      p75         Q3     82
#> 7     ARM      Placebo      AGE                      min        Min     52
#> 8     ARM      Placebo      AGE                      max        Max     89
#> 9     ARM      Placebo   AGEGR1            <65         n          n     14
#> 10    ARM      Placebo   AGEGR1            <65         N          N     86
#> ℹ 101 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 4 more variables: context, fmt_fun, warning, error
```

The optional arguments that can be specified to improve the appearance
of the table. - `.attributes` summary table will utilize the column
label attributes, if available - `.total_n` the total N is saved
internally, and will be used in the printed table. - `.overall` the
operations will be repeated without `.by` variable - `.missing` when
missing results are included, users can include missing counts or rates
for the variables.

**ARD ➡ Table**

After the ARD has been created, we can now create the summary table with
[`tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_summary.md).

``` r
ard_demo |> 
  tbl_ard_summary(
    by = ARM, 
    overall = TRUE,
    type = AGE ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
    label = list(AGEGR1 = "Age Group")
  ) |> 
  add_stat_label() |> 
  modify_header(all_stat_cols() ~ "**{level}**  \nN= {n}")
```

[TABLE]

#### Complex Summaries

The ARD to Table pipeline is most convenient when trying to consolidate
multiple analysis steps into an ARD to feed only the relevant stats to
the table building machinery. In the example below, we create a table
that mixing three types of analysis for assessing outcomes after
treatment: Kaplan-Meier estimate of survival, mean marker levels with
confidence intervals, and rate of tumor response with confidence
intervals.

First, we will create an ARD for each of these analyses, then combine
them with
[`cards::bind_ard()`](https://insightsengineering.github.io/cards/latest-tag/reference/bind_ard.html).

``` r
# ARD with the Kaplan-Meier survival estimates
ard_survival <-
  trial |> 
  cardx::ard_survival_survfit(
    y = survival::Surv(ttdeath, death),
    variables = "trt",
    times = c(12, 24)
  ) |> 
  # retain survival time statistics
  dplyr::filter(variable == "time") |> 
  update_ard_fmt_fun(stat_names = c("estimate", "conf.low", "conf.high"), fmt_fun = "xx%")

# ARD with the mean post-treatment marker level with 95%CI
ard_marker_level <-
  cardx::ard_stats_t_test_onesample(trial, variables = marker, by = trt) |> 
  update_ard_fmt_fun(stat_names = c("estimate", "conf.low", "conf.high"), fmt_fun = label_style_sigfig(digits = 2))

# ARD with the post-treatment response rate with 95%CI
ard_tumor_response <-
  cardx::ard_categorical_ci(trial, by = trt, variables = response, method = "wilson") |> 
  update_ard_fmt_fun(stat_names = c("estimate", "conf.low", "conf.high"), fmt_fun = "xx%")


# combine all the ARDs into a single ARD for the outcomes
ard_outcomes <- 
  cards::bind_ard(
    ard_survival,
    ard_marker_level, 
    ard_tumor_response
  )
```

If you inspect the ARDs, you’ll see that these analytic results have a
similar structure to the simple ARDs we extracted from the
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
results above.

- The
  [`cardx::ard_survival_survfit()`](https://insightsengineering.github.io/cardx/latest-tag/reference/ard_survival_survfit.html)
  ARD looks like the
  [`ard_tabulate()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_tabulate.html)
  result.
- The
  [`cardx::ard_stats_t_test_onesample()`](https://insightsengineering.github.io/cardx/latest-tag/reference/ard_stats_t_test_onesample.html)
  ARD looks like the
  [`ard_summary()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_summary.html)
  result.
- The
  [`cardx::ard_categorical_ci()`](https://insightsengineering.github.io/cardx/latest-tag/reference/ard_categorical_ci.html)
  ARD looks like the
  [`ard_tabulate_value()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_tabulate_value.html)
  result.

With the created ARD, we can now build a summary table.

``` r
ard_outcomes |> 
  tbl_ard_summary(
    by = trt, 
    type = response ~ "dichotomous",
    statistic =
      list(
        c(time, response) ~ "{estimate}% (95% CI {conf.low}%, {conf.high}%)",
        marker ~ "{estimate} (95% CI {conf.low}, {conf.high})"
      ),
    label = 
      list(time = "Overal Survival, months",
           marker = "Tumor Marker",
           response = "Tumor Response")
  ) |> 
  remove_footnote_header(columns = everything()) |> 
  modify_abbreviation(abbreviation = "CI = Confidence Interval") |> 
  modify_footnote_body(
    footnote = "Kaplan-Meier estimate", 
    columns = label, 
    rows = variable == "time" & row_type == "label"
  ) |> 
  modify_footnote_body("t-distribution based mean and CI", columns = "label", rows = variable == "marker") |> 
  modify_footnote_body("Wilson CI", columns = "label", rows = variable == "response")
```

[TABLE]

#### Final Thoughts

When creating the a custom summary table, you will want to utilize the
functions with the `tbl_ard_*()` prefix. It will be important to
familiarize yourself with the table structures that each of these
functions produce, so you know which to use to build your table.

If your table is a combination or mix of table types structures, you can
build each part of your table separately and use
[`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
and
[`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
to cobble together your final table.

Finally, some tables are entirely unique and would be difficult to
create under any framework. In these cases, it’s often much easier to
build a data frame and then convert it to a gtsummary table with
[`as_gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gtsummary.md).
Once converted, you can take advantage of styling that is available for
all gtsummary tables.
