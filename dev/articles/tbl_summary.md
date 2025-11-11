# tbl_summary() tutorial

## Introduction

The
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
function calculates **descriptive statistics** for continuous,
categorical, and dichotomous variables in **R**, and presents the
results in a **beautiful, customizable summary table** ready for
publication (for example, Table 1 or demographic tables).

This vignette will walk a reader through the
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
function, and the various functions available to modify and make
additions to an existing table summary object.

## Setup

Before going through the tutorial, install and load {gtsummary}.

``` r
# install.packages("gtsummary")
library(gtsummary)
```

## Example data set

We’ll be using the
[`trial`](https://www.danieldsjoberg.com/gtsummary/reference/trial.html)
data set throughout this example.

- This set contains data from 200 patients who received one of two types
  of chemotherapy (Drug A or Drug B). The outcomes are tumor response
  and death.

- Each variable in the data frame has been assigned an **attribute
  label** (i.e. `attr(trial$trt, "label") == "Chemotherapy Treatment")`
  with the **[labelled](http://larmarange.github.io/labelled/)**
  package. These labels are displayed in the {gtsummary} output table by
  default. Using {gtsummary} on a data frame without labels will simply
  print variable names in place of variable labels; there is also an
  option to add labels later.

| Variable                                                           | Class     | Label                  |
|--------------------------------------------------------------------|-----------|------------------------|
| `trt`                                                              | character | Chemotherapy Treatment |
| `age`                                                              | numeric   | Age                    |
| `marker`                                                           | numeric   | Marker Level (ng/mL)   |
| `stage`                                                            | factor    | T Stage                |
| `grade`                                                            | factor    | Grade                  |
| `response`                                                         | integer   | Tumor Response         |
| `death`                                                            | integer   | Patient Died           |
| `ttdeath`                                                          | numeric   | Months to Death/Censor |
| Includes mix of continuous, dichotomous, and categorical variables |           |                        |

``` r
head(trial)
#> # A tibble: 6 × 8
#>   trt      age marker stage grade response death ttdeath
#>   <chr>  <dbl>  <dbl> <fct> <fct>    <int> <int>   <dbl>
#> 1 Drug A    23  0.16  T1    II           0     0    24  
#> 2 Drug B     9  1.11  T2    I            1     0    24  
#> 3 Drug A    31  0.277 T1    II           0     0    24  
#> 4 Drug A    NA  2.07  T3    III          1     1    17.6
#> 5 Drug A    51  2.77  T4    III          1     1    16.4
#> 6 Drug B    39  0.613 T4    I            0     1    15.6
```

## Basic Usage

The default output from
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
is meant to be **publication ready**.

Let’s start by creating a table of summary statistics from the `trial`
data set. The
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
function can take, at minimum, a data frame as the only input, and
returns descriptive statistics for each column in the data frame.

``` r
trial |> tbl_summary(include = c(trt, age, grade))
```

[TABLE]

Note the sensible defaults with this basic usage; each of the defaults
may be customized.

- **Variable types** are automatically detected so that appropriate
  descriptive statistics are calculated.

- **Label attributes** from the data set are automatically printed.

- **Missing values** are listed as “Unknown” in the table.

- Variable levels are **indented** and **footnotes** are added.

For this study data the summary statistics should be **split by
treatment group**, which can be done by using the **`by=`** argument. To
compare two or more groups, include
[`add_p()`](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html)
with the function call, which detects variable type and uses an
appropriate statistical test.

``` r
trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p()
```

[TABLE]

## Customize Output

There are four primary ways to customize the output of the summary
table.

1.  Use
    [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
    function arguments
2.  Add additional data/information to a summary table with `add_*()`
    functions
3.  Modify summary table appearance with the {gtsummary} functions
4.  Modify table appearance with {gt} package functions

### Modifying `tbl_summary()` function arguments

The
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
function includes many input options for modifying the appearance.

| Argument       | Description                                                      |
|----------------|------------------------------------------------------------------|
| `label`        | specify the variable labels printed in table                     |
| `type`         | specify the variable type (e.g. continuous, categorical, etc.)   |
| `statistic`    | change the summary statistics presented                          |
| `digits`       | number of digits the summary statistics will be rounded to       |
| `missing`      | whether to display a row with the number of missing observations |
| `missing_text` | text label for the missing number row                            |
| `missing_stat` | statistic(s) to show on the missing row                          |
| `sort`         | change the sorting of categorical levels by frequency            |
| `percent`      | print column, row, or cell percentages                           |
| `include`      | list of variables to include in summary table                    |

Example modifying
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
arguments.

``` r
trial |>
  tbl_summary(
    by = trt,
    include = c(age, grade),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    label = list(grade = "Tumor Grade"),
    missing_text = "(Missing)"
  )
```

[TABLE]

There are multiple ways to specify the `statistic=` argument using a
single formula, a list of formulas, and a named list. The following
table shows equivalent ways to specify the mean statistic for continuous
variables `age` and `marker.` Any {gtsummary} function argument that
accepts formulas will accept each of these variations.

| **Select with Helpers**             | **Select by Variable Name**       | **Select with Named List**                |
|-------------------------------------|-----------------------------------|-------------------------------------------|
| `all_continuous() ~ "{mean}"`       | `c("age", "marker") ~ "{mean}"`   | `list(age = "{mean}", marker = "{mean}")` |
| `list(all_continuous() ~ "{mean}")` | `c(age, marker) ~ "{mean}"`       | —                                         |
| —                                   | `list(c(age, marker) ~ "{mean}")` | —                                         |

![](https://github.com/ddsjoberg/gtsummary/raw/main/data-raw/crayon_images/crayon-selectors.png)

### {gtsummary} functions to add information

The {gtsummary} package has functions to adding information or
statistics to
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
tables.

| Function                                                                                       | Description                                                                   |
|------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------|
| [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)                   | add p-values to the output comparing values across groups                     |
| [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)       | add a column with overall summary statistics                                  |
| [`add_n()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.md)                   | add a column with N (or N missing) for each variable                          |
| [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md) | add column for difference between two group, confidence interval, and p-value |
| [`add_stat_label()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat_label.md) | add label for the summary statistics shown in each row                        |
| [`add_stat()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat.md)             | generic function to add a column with user-defined values                     |
| [`add_q()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_q.md)                   | add a column of q values to control for multiple comparisons                  |

### {gtsummary} functions to format table

The {gtsummary} package comes with functions specifically made to modify
and format summary tables.

| Function                                                                                                       | Description                   |
|----------------------------------------------------------------------------------------------------------------|-------------------------------|
| [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)                          | update column headers         |
| [`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)       | update column header footnote |
| [`modify_footnote_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)         | update table body footnote    |
| [`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)                 | update spanning headers       |
| [`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md)                 | update table caption/title    |
| [`bold_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)      | bold variable labels          |
| [`bold_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)      | bold variable levels          |
| [`italicize_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md) | italicize variable labels     |
| [`italicize_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md) | italicize variable levels     |
| [`bold_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_p.md)                                 | bold significant p-values     |

Example adding
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)-family
functions

``` r
trial |>
  tbl_summary(by = trt, includ = c(age, grade)) |>
  add_p(pvalue_fun = label_style_pvalue(digits = 2)) |>
  add_overall() |>
  add_n() |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Received**") |>
  modify_footnote_header("Median (IQR) or Frequency (%)", columns = all_stat_cols()) |>
  modify_caption("**Table 1. Patient Characteristics**") |>
  bold_labels()
```

[TABLE]

**Table 1. Patient Characteristics**

### {gt} functions to format table

The [{gt} package](https://gt.rstudio.com/index.html) is packed with
many great functions for modifying table output—too many to list here.
Review the package’s website for a full listing.

To use the {gt} package functions with {gtsummary} tables, the summary
table must first be converted into a `gt` object. To this end, use the
[`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
function after modifications have been completed with {gtsummary}
functions.

``` r
trial |>
  tbl_summary(by = trt, include = c(age, grade), missing = "no") |>
  add_n() |>
  as_gt() |>
  gt::tab_source_note(gt::md("*This data is simulated*"))
```

[TABLE]

## Select Helpers

There is flexibility in how you select variables for {gtsummary}
arguments, which allows for many customization opportunities! For
example, if you want to show age and the marker levels to one decimal
place in
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
you can pass `digits = c(age, marker) ~ 1`. The selecting input is
flexible, and you may also pass quoted column names.

Going beyond typing out specific variables in your data set, you can
use:

1.  All [**{tidyselect}
    helpers**](https://tidyselect.r-lib.org/reference/index.html)
    available throughout the tidyverse, such as
    [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
    [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
    and
    [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
    (i.e. anything you can use with the
    [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
    function), can be used with {gtsummary}.

2.  Additional [**{gtsummary}
    selectors**](https://www.danieldsjoberg.com/gtsummary/reference/select_helpers.html)
    that are included in the package to supplement tidyselect functions.

    - **Summary type** There are two primary ways to select variables by
      their summary type. This is useful, for example, when you wish to
      report the mean and standard deviation for all continuous
      variables: `statistic = all_continuous() ~ "{mean} ({sd})"`.

      ``` r
      all_continuous()
      all_categorical()
      ```

      Dichotomous variables are, by default, included with
      [`all_categorical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md).

## Multi-line Continuous Summaries

Continuous variables may also be summarized on multiple lines—a common
format in some journals. To update the continuous variables to summarize
on multiple lines, update the summary type to `"continuous2"` (for
summaries on two or more lines).

``` r
trial |>
  tbl_summary(
    by = trt,
    include = age,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{N_nonmiss}",
      "{median} ({p25}, {p75})",
      "{min}, {max}"
    ),
    missing = "no"
  ) |>
  add_p(pvalue_fun = label_style_pvalue(digits = 2))
```

[TABLE]

## Advanced Customization

*The information in this section applies to all {gtsummary} objects.*

The {gtsummary} table has two important internal objects:

| Internal Object   | Description                                                   |
|-------------------|---------------------------------------------------------------|
| `.$table_body`    | data frame that is printed as the gtsummary output table      |
| `.$table_styling` | contains instructions for styling `.$table_body` when printed |

When you print output from the
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
function into the R console or into an R markdown document, the
`.$table_body` data frame is formatted using the instructions listed in
`.$table_styling`. The default printer converts the {gtsummary} object
to a {gt} object with
[`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
via a sequence of {gt} commands executed on `.$table_body`. Here’s an
example of the first few calls saved with
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md):

``` r
tbl_summary(trial) |>
  as_gt(return_calls = TRUE) |>
  head(n = 4)
#> $gt
#> gt::gt(data = x$table_body)
#> 
#> $fmt_missing
#> $fmt_missing[[1]]
#> gt::sub_missing(columns = gt::everything(), missing_text = "")
#> 
#> 
#> $cols_merge
#> list()
#> 
#> $cols_align
#> $cols_align[[1]]
#> gt::cols_align(columns = c("variable", "var_type", "row_type", 
#> "var_label", "stat_0"), align = "center")
#> 
#> $cols_align[[2]]
#> gt::cols_align(columns = "label", align = "left")
```

The {gt} functions are called in the order they appear, beginning with
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html).

If the user does not want a specific {gt} function to run (i.e. would
like to change default printing), any {gt} call can be excluded in the
[`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
function. In the example below, the default alignment is restored.

After the
[`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
function is run, additional formatting may be added to the table using
{gt} functions. In the example below, a source note is added to the
table.

``` r
tbl_summary(trial, by = trt, include = c(age, grade)) |>
  as_gt(include = -cols_align) |>
  gt::tab_source_note(gt::md("*This data is simulated*"))
```

[TABLE]

## Set Default Options with Themes

The {gtsummary}
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
function and the related functions have sensible defaults for rounding
and presenting results. If you, however, would like to change the
defaults there are a few options. The default options can be changed
using the {gtsummary} themes function
[`set_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md).
The package includes prespecified themes, and you can also create your
own. Themes can control baseline behavior, for example, how p-values and
percentages are rounded, which statistics are presented in
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
default statistical tests in
[`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md),
etc.

For details on creating a theme and setting personal defaults, review
the [themes
vignette](https://www.danieldsjoberg.com/gtsummary/dev/articles/themes.html).

## Survey Data

The {gtsummary} package also supports survey data (objects created with
the [{survey}](https://CRAN.R-project.org/package=survey) package) via
the
[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
function. The syntax for
[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
and
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
are nearly identical, and the examples above apply to survey summaries
as well.

To begin, install the {survey} package and load the `apiclus1` data set.

``` r
install.packages("survey")
```

``` r
# loading the api data set
data(api, package = "survey")
```

Before we begin, we convert the data frame to a survey object,
registering the ID and weighting columns, and setting the finite
population correction column.

``` r
svy_apiclus1 <-
  survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
```

After creating the survey object, we can now summarize it similarly to a
standard data frame using
[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
Like
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
accepts the `by=` argument and works with the
[`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
and
[`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
functions.

It is not possible to pass custom functions to the `statistic=` argument
of
[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
You must use one of the [pre-defined summary statistic
functions](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.html#statistic-argument)
(e.g. `{mean}`, `{median}`) which leverage functions from the {survey}
package to calculate weighted statistics.

``` r
svy_apiclus1 |>
  tbl_svysummary(
    # stratify summary statistics by the "both" column
    by = both,
    # summarize a subset of the columns
    include = c(api00, api99, both),
    # adding labels to table
    label = list(api00 = "API in 2000",
                 api99 = "API in 1999")
  ) |>
  add_p() |> # comparing values by "both" column
  add_overall() |>
  # adding spanning header
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Met Both Targets**")
```

[TABLE]

[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
can also handle weighted survey data where each row represents several
individuals:

``` r
Titanic |>
  as_tibble() |>
  survey::svydesign(data = _, ids = ~1, weights = ~n) |>
  tbl_svysummary(include = c(Age, Survived))
```

[TABLE]

## Cross Tables

Use
[`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)
to compare two categorical variables in your data.
[`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)
is a wrapper for
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
that:

- Automatically adds a spanning header to your table with the name or
  label of your comparison variable.
- Uses `percent = "cell"` by default.
- Adds row and column margin totals (customizable through the `margin`
  argument).
- Displays missing data in both row and column variables (customizable
  through the `missing` argument).

``` r
trial |>
  tbl_cross(
    row = stage,
    col = trt,
    percent = "cell"
  ) |>
  add_p()
```

[TABLE]
