
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/ddsjoberg/gtsummary/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ddsjoberg/gtsummary/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![Codecov test
coverage](https://codecov.io/gh/ddsjoberg/gtsummary/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ddsjoberg/gtsummary?branch=main)
[![gtsummary
downloads](https://cranlogs.r-pkg.org/badges/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![DOI:10.32614/RJ-2021-053](https://zenodo.org/badge/DOI/10.32614/RJ-2021-053.svg)](https://doi.org/10.32614/RJ-2021-053)
<!-- badges: end -->

## gtsummary <a href='https://github.com/ddsjoberg/gtsummary'><img src='man/figures/logo.png' alt = "Logo of gtsummary" align="right" height="120" /></a>

The {gtsummary} package provides an elegant and flexible way to create
publication-ready analytical and summary tables using the **R**
programming language. The {gtsummary} package summarizes data sets,
regression models, and more, using sensible defaults with highly
customizable capabilities.

The package is widely used across **clinical and pharmaceutical
research** for reporting clinical trials—from baseline demographics and
adverse event summaries to efficacy analyses. Every table is computed
from a structured, machine-readable [Analysis Results Dataset
(ARD)](https://www.danieldsjoberg.com/gtsummary/articles/tbl_ard-functions.html)—part
of the [CDISC Analysis Results
Standard](https://www.cdisc.org/standards/foundational/analysis-results-standard)—via
the pharmaverse [{cards}](https://pharmaverse.github.io/cards/) and
[{cardx}](https://pharmaverse.github.io/cardx/) packages, making results
traceable and straightforward to QC.

- [**Summarize data frames or
  tibbles**](https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html)
  easily in **R**. Perfect for presenting descriptive statistics,
  comparing group **demographics** (e.g creating a **Table 1** for
  medical journals), and more. Automatically detects continuous,
  categorical, and dichotomous variables in your data set, calculates
  appropriate descriptive statistics, and also includes amount of
  missingness in each variable.

- [**Summarize regression
  models**](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
  in R and include reference rows for categorical variables. Common
  regression models, such as logistic regression and Cox proportional
  hazards regression, are automatically identified and the tables are
  pre-filled with appropriate column headers (i.e. Odds Ratio and Hazard
  Ratio).

- [**Customize gtsummary
  tables**](https://www.danieldsjoberg.com/gtsummary/reference/index.html#section-general-formatting-styling-functions)
  using a growing list of formatting/styling functions.
  **[Bold](https://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)**
  labels,
  **[italicize](https://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)**
  levels, **[add
  p-value](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html)**
  to summary tables,
  **[style](https://www.danieldsjoberg.com/gtsummary/reference/style_percent.html)**
  the statistics however you choose,
  **[merge](https://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.html)**
  or
  **[stack](https://www.danieldsjoberg.com/gtsummary/reference/tbl_stack.html)**
  tables to present results side by side… there are so many
  possibilities to create the table of your dreams!

- **[Report statistics
  inline](https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html)**
  from summary tables and regression summary tables in **R markdown**.
  Make your reports completely reproducible!

By leveraging [{broom}](https://broom.tidymodels.org/),
[{gt}](https://gt.rstudio.com/), and
[{labelled}](http://larmarange.github.io/labelled/) packages,
{gtsummary} creates beautifully formatted, ready-to-share summary and
result tables in a single line of R code!

Check out the examples below, review the
[vignettes](https://www.danieldsjoberg.com/gtsummary/articles/) for a
detailed exploration of the output options, and view the
[gallery](https://www.danieldsjoberg.com/gtsummary/articles/gallery.html)
for various customization examples.

## Installation

The {gtsummary} package was written as a companion to the
[{gt}](https://gt.rstudio.com/) package from RStudio. You can install
{gtsummary} with the following code.

``` r
install.packages("gtsummary")
```

Install the development version with
`pak::pkg_install("ddsjoberg/gtsummary", dependencies = TRUE)`

## Examples

### Summary Table

Use
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
to summarize a data frame.

<!-- <img src = "https://github.com/ddsjoberg/gtsummary/raw/main/data-raw/misc_files/tbl_summary_demo1.gif" alt = "animated" width = "100%"> -->

Example basic table:

``` r
library(gtsummary)

# summarize the data with our package
table1 <-
  trial |> 
  tbl_summary(include = c(age, grade, response))
```

<img src="man/figures/README-tbl_summary_print_simple-1.png" alt="Example of basic table" width="25%" />

There are many **customization options** to **add information** (like
comparing groups) and **format results** (like bold labels) in your
table. See the
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html)
tutorial for many more options, or below for one example.

``` r
table2 <-
  tbl_summary(
    trial,
    include = c(age, grade, response),
    by = trt, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p() |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()
```

<img src="man/figures/README-tbl_summary_print_extra-1.png" alt="Example of table with customize options" width="55%" />

### Regression Models

Use
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
to easily and beautifully display regression model results in a table.
See the
[tutorial](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
for customization options.

``` r
mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)

t1 <- tbl_regression(mod1, exponentiate = TRUE)
```

<img src="man/figures/README-tbl_regression_printa-1.png" alt="Example of table using tbl_regression" width="40%" />

### Side-by-side Regression Models

You can also present side-by-side regression model results using
`tbl_merge()`

``` r
library(survival)

# build survival model table
t2 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) |> 
  tbl_regression(exponentiate = TRUE)

# merge tables
tbl_merge_ex1 <-
  tbl_merge(
    tbls = list(t1, t2),
    tab_spanner = c("**Tumor Response**", "**Time to Death**")
  )
```

<img src="man/figures/README-tbl_merge_ex1-1.png" alt="Example of tables merged with tbl_merge" width="60%" />

Review even more output options in the **[table
gallery](https://www.danieldsjoberg.com/gtsummary/articles/gallery.html)**.

## Clinical & Pharmaceutical Research

{gtsummary} is a natural fit for clinical trial reporting. Below we
highlight two features that clinical and pharmaceutical teams rely on:
safety tables and built-in QC via the ARD backend. Both examples use the
CDISC pilot data (`ADAE`/`ADSL`) shipped with the
[{cards}](https://pharmaverse.github.io/cards/) package.

### Adverse Event Tables

Use
[`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_hierarchical.html)
to build nested adverse event summaries by treatment arm—counts of
patients with an event, organized by System Organ Class and Preferred
Term—following FDA Standard Safety Table conventions.

``` r
library(cards) # provides the ADAE / ADSL CDISC pilot datasets

tbl_ae <-
  ADAE |>
  # filter to a few System Organ Classes and Preferred Terms for a compact display
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:3],
    AETERM %in% unique(cards::ADAE$AETERM)[1:3]
  ) |>
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = TRTA, # summarize by treatment arm
    denominator = cards::ADSL,
    id = USUBJID, # count unique patients, not events
    overall_row = TRUE,
    label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
  )
```

<img src="man/figures/README-tbl_ae-1.png" alt="Example of a nested adverse event table by treatment arm" width="70%" />

### Built-in QC with the ARD Backend

Because every {gtsummary} table is computed from an **Analysis Results
Dataset (ARD)**—a structured, machine-readable record of every
statistic, part of the [CDISC Analysis Results
Standard](https://www.cdisc.org/standards/foundational/analysis-results-standard)—you
can extract the numbers behind any table with
[`gather_ard()`](https://www.danieldsjoberg.com/gtsummary/reference/gather_ard.html).
Each statistic becomes one row, making it simple to QC results, trace a
value back to its calculation, or compare against an independently
double-programmed dataset.

For example, the adverse event counts behind the table above are stored
as individual records—one for each event count (`n`), denominator (`N`),
and percentage (`p`) in every treatment arm:

``` r
tbl_ae |>
  gather_ard() |>
  bind_ard() |>
  # keep the adverse event term counts and the columns identifying each statistic
  dplyr::filter(variable == "AETERM") |>
  dplyr::select(group1_level, variable_level, stat_name, stat)
#> # An ARD data frame: 27 × 4
#>    group1_level         variable_level            stat_name    stat
#>    <list>               <list>                    <chr>      <list>
#>  1 Placebo              DIARRHOEA                 n          9     
#>  2 Placebo              DIARRHOEA                 N         86     
#>  3 Placebo              DIARRHOEA                 p          0.105 
#>  4 Xanomeline High Dose DIARRHOEA                 n          4     
#>  5 Xanomeline High Dose DIARRHOEA                 N         84     
#>  6 Xanomeline High Dose DIARRHOEA                 p          0.0476
#>  7 Xanomeline Low Dose  DIARRHOEA                 n          5     
#>  8 Xanomeline Low Dose  DIARRHOEA                 N         84     
#>  9 Xanomeline Low Dose  DIARRHOEA                 p          0.0595
#> 10 Placebo              APPLICATION SITE ERYTHEMA n          3     
#> # ℹ 17 more rows
```

For the full CDISC/ARD workflow—including the `tbl_ard_*()` constructors
that build tables directly from an ARD—see the [**ARD-first
Tables**](https://www.danieldsjoberg.com/gtsummary/articles/tbl_ard-functions.html)
article.

## gtsummary + R Markdown

The **{gtsummary}** package was written to be a companion to the
**{gt}** package from RStudio. But not all output types are supported by
the **{gt}** package. Therefore, we have made it possible to print
**{gtsummary}** tables with various engines.

Review the **[gtsummary + R
Markdown](https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html)**
vignette for details.

<a href="https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html">
<img src="man/figures/gt_output_formats.PNG" alt="Comparison of engines and their output compatibility" width="55%" />
</a>

## Save Individual Tables

{gtsummary} tables can also be saved directly to file as an image, HTML,
Word, RTF, and LaTeX file.

``` r
tbl |>
  as_gt() |>
  gt::gtsave(filename = ".") # use extensions .png, .html, .docx, .rtf, .tex, .ltx
```

For submission-ready deliverables, convert to a
[{flextable}](https://davidgohel.github.io/flextable/) with
[`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/reference/as_flex_table.html)
for polished RTF and Word output, or save straight to a Word document
with `save_flex_docx()`.

## Cite gtsummary

``` text
> citation("gtsummary")

To cite gtsummary in publications use:

  Sjoberg DD, Whiting K, Curry M, Lavery JA, Larmarange J. Reproducible summary tables with the gtsummary package.
  The R Journal 2021;13:570–80. https://doi.org/10.32614/RJ-2021-053.

A BibTeX entry for LaTeX users is

  @Article{gtsummary,
    author = {Daniel D. Sjoberg and Karissa Whiting and Michael Curry and Jessica A. Lavery and Joseph Larmarange},
    title = {Reproducible Summary Tables with the gtsummary Package},
    journal = {{The R Journal}},
    year = {2021},
    url = {https://doi.org/10.32614/RJ-2021-053},
    doi = {10.32614/RJ-2021-053},
    volume = {13},
    issue = {1},
    pages = {570-580},
  }
```

## Contributing

Big thank you to `@jeffreybears` for the hex sticker!

Please note that the {gtsummary} project is released with a [Contributor
Code of
Conduct](https://www.danieldsjoberg.com/gtsummary/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms. Thank
you to all contributors!
