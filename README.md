
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/ddsjoberg/gtsummary/branch/master/graph/badge.svg)](https://codecov.io/gh/ddsjoberg/gtsummary?branch=master)
[![R build
status](https://github.com/ddsjoberg/gtsummary/workflows/R-CMD-check/badge.svg)](https://github.com/ddsjoberg/gtsummary/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![](https://cranlogs.r-pkg.org/badges/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## gtsummary <a href='https://github.com/ddsjoberg/gtsummary'><img src='man/figures/logo.png' align="right" height="120" /></a>

The {gtsummary} package provides an elegant and flexible way to create
publication-ready analytical and summary tables using the **R**
programming language. The {gtsummary} package summarizes data sets,
regression models, and more, using sensible defaults with highly
customizable capabilities.

  - [**Summarize data frames or
    tibbles**](http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html)
    easily in **R**. Perfect for presenting descriptive statistics,
    comparing group **demographics** (e.g creating a **Table 1** for
    medical journals), and more. Automatically detects continuous,
    categorical, and dichotomous variables in your data set, calculates
    appropriate descriptive statistics, and also includes amount of
    missingness in each variable.

  - [**Summarize regression
    models**](http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
    in R and include reference rows for categorical variables. Common
    regression models, such as logistic regression and Cox proportional
    hazards regression, are automatically identified and the tables are
    pre-filled with appropriate column headers (i.e. Odds Ratio and
    Hazard Ratio).

  - [**Customize gtsummary
    tables**](http://www.danieldsjoberg.com/gtsummary/reference/index.html#section-general-formatting-styling-functions)
    using a growing list of formatting/styling functions.
    **[Bold](http://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)**
    labels,
    **[italicize](http://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)**
    levels, **[add
    p-value](http://www.danieldsjoberg.com/gtsummary/reference/add_p.html)**
    to summary tables,
    **[style](http://www.danieldsjoberg.com/gtsummary/reference/style_percent.html)**
    the statistics however you choose,
    **[merge](http://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.html)**
    or
    **[stack](http://www.danieldsjoberg.com/gtsummary/reference/tbl_stack.html)**
    tables to present results side by side… there are so many
    possibilities to create the table of your dreams\!

  - **[Report statistics
    inline](http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#inline_text)**
    from summary tables and regression summary tables in **R markdown**.
    Make your reports completely reproducible\!

By leveraging [{broom}](https://broom.tidyverse.org/),
[{gt}](https://gt.rstudio.com/), and
[{labelled}](http://larmarange.github.io/labelled/) packages,
{gtsummary} creates beautifully formatted, ready-to-share summary and
result tables in a single line of R code\!

Check out the examples below, review the
[vignettes](http://www.danieldsjoberg.com/gtsummary/articles/) for a
detailed exploration of the output options, and view the
[gallery](http://www.danieldsjoberg.com/gtsummary/articles/gallery.html)
for various customization examples.

## Installation

The {gtsummary} package was written as a companion to the
[{gt}](https://gt.rstudio.com/) package from RStudio. You can install
{gtsummary} with the following code.

``` r
install.packages("gtsummary")
```

Install the development version of {gtsummary} with:

``` r
remotes::install_github("ddsjoberg/gtsummary")
```

## Examples

### Summary Table

Use
[`tbl_summary()`](http://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
to summarize a data frame.

<img src = "man/figures/tbl_summary_demo1.gif" alt = "animated" width = "100%">

Example basic table:

``` r
library(gtsummary)
# make dataset with a few variables to summarize
trial2 <- trial %>% dplyr::select(trt, age, grade, response)

# summarize the data with our package
table1 <- tbl_summary(trial2)
```

<img src="man/figures/README-tbl_summary_print_simple-1.png" width="30%" />

There are many **customization options** to **add information** (like
comparing groups) and **format results** (like bold labels) in your
table. See the
[`tbl_summary()`](http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html)
tutorial for many more options, or below for one example.

``` r
table2 <- tbl_summary(
  trial2,
  by = trt, # split table by group
  missing = "no" # don't list missing data separately
) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test if there's difference between groups
  bold_labels() 
```

<img src="man/figures/README-tbl_summary_print_extra-1.png" width="60%" />

### Regression Models

Use
[`tbl_regression()`](http://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
to easily and beautifully display regression model results in a table.
See the
[tutorial](http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
for customization options.

``` r
mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)

t1 <- tbl_regression(mod1, exponentiate = TRUE)
```

<img src="man/figures/README-tbl_regression_printa-1.png" width="40%" />

### Side-by-side Regression Models

You can also present side-by-side regression model results using
`tbl_merge()`

``` r
library(survival)

# build survival model table
t2 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  tbl_regression(exponentiate = TRUE)

# merge tables 
tbl_merge_ex1 <-
  tbl_merge(
    tbls = list(t1, t2),
    tab_spanner = c("**Tumor Response**", "**Time to Death**")
  )
```

<img src="man/figures/README-tbl_merge_ex1-1.png" width="60%" />

Review even more output options in the [table
gallery](http://www.danieldsjoberg.com/gtsummary/articles/gallery.html).

## gtsummary + R Markdown

<!-- This is copied from the rmarkdwon.Rmd vignette. May need to be updated periodically -->

The **{gtsummary}** package was written to be a companion to the
**{gt}** package from RStudio. But not all output types are supported by
the **{gt}** package. Therefore, we have made it possible to print
**{gtsummary}** tables with various engines.

Review the **[gtsummary + R
Markdown](http://www.danieldsjoberg.com/gtsummary/dev/articles/rmarkdown.html)**
vignette for details.

<!-- Printing table of output types. -->

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rovislwdot .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rovislwdot .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rovislwdot .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rovislwdot .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rovislwdot .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rovislwdot .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rovislwdot .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rovislwdot .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rovislwdot .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rovislwdot .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rovislwdot .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rovislwdot .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#rovislwdot .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rovislwdot .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rovislwdot .gt_from_md > :first-child {
  margin-top: 0;
}

#rovislwdot .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rovislwdot .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rovislwdot .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#rovislwdot .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rovislwdot .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#rovislwdot .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rovislwdot .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rovislwdot .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rovislwdot .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rovislwdot .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#rovislwdot .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rovislwdot .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#rovislwdot .gt_left {
  text-align: left;
}

#rovislwdot .gt_center {
  text-align: center;
}

#rovislwdot .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rovislwdot .gt_font_normal {
  font-weight: normal;
}

#rovislwdot .gt_font_bold {
  font-weight: bold;
}

#rovislwdot .gt_font_italic {
  font-style: italic;
}

#rovislwdot .gt_super {
  font-size: 65%;
}

#rovislwdot .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="rovislwdot" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table" style="table-layout: fixed; width: 490px">

<colgroup>

<col style="width: 110px"/>

<col style="width: 140px"/>

<col style="width: 60px"/>

<col style="width: 60px"/>

<col style="width: 60px"/>

<col style="width: 60px"/>

</colgroup>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

<strong>Print Engine</strong>

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

<strong>Function</strong>

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

<strong>HTML</strong>

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

<strong>PDF</strong>

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

<strong>RTF</strong>

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

<strong>Word</strong>

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<a href="https://gt.rstudio.com/index.html">gt</a>

</p>

</div>

</td>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<code>as\_gt()</code>

</p>

</div>

</td>

<td class="gt_row gt_center" style="background-color: rgba(186,255,201,0.8); color: #000000;">

<img cid="asmjihnwxtkq__icons8-smiling-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(186,225,255,0.8); color: #000000;">

<img cid="hblyvzsticep__icons8-under-construction-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(186,225,255,0.8); color: #000000;">

<img cid="apulesnvijgm__icons8-under-construction-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,179,186,0.8); color: #000000;">

<img cid="dezatolvsgun__icons8-no-entry-100.png" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAABkCAYAAABw4pVUAAAABmJLR0QA/wD/AP+gvaeTAAAHnklEQVR4nO2dS2xVRRjHf5UGWkADmGgQE7VoS6EK1IUYQdAgKg/B8AzBvRKNS8OKhxhINCSGxAdEFxjBBQq4lYgJ6MIgCEYsBVqIQcCIPFpaxGJdzGl67zdz7mvmvO45v+Sk9zTnzvznmzOP+80LMjIyMjIyKqMmagElUA9MABqBJu/zQ8AI7xrt/QW4AVzx/t4AOoA2oN272oDeELWXTRwzpBaYDMz2rhnAMEdh9wHHgP3edQi46SjsqqIWmAvsAnqA/pCuHmAn8CIwJPBUJoDxwBbgAuFlgt91wdMyPtAUFyGqKqsFWAMsp7Q38xRwgsG2oB34G9VOXAW6vedGAqNQbcoYVLszcE0CHi4hrtvAF8Am4NeSUpNgmoG9wH8Ufls7gG3ASmCsw/jHemFuBzqLaLgN7EF1IqqO4cA6VAPqZ4ArwA5UQx5WyX0ceB+4VEDXLe+ZkSFpCpxFwDn8E3wYWAIMjUqgF/dS4Cf8dZ4FFkakzwl1qDfLL4GHgAXEr+s9HfgGf907UCU+UTQDxzEn6DSqixt35gFnMKfhGOpHaiJYhur1yET0AmtRJScp1APrUdplerqAxdFJK43XUb0TKf4oCXqjDExAlQpTT2x1hLoK8hZVVOcaKNQmbo5Ql5GtmN0Sy6IUFRArMLt3tkYpKpe3Mf+mmBGlqICZBvyFnu71UYoCeA2zT2hylKJCYhLwO3r634xK0DL0Bvw8aqwiLTSg0iwb+tB7X43AdSHkKjAlbCExoAXl6JRd4uawBNQBPwsBPVR3m1GMaSjPc65NfkH9jgmc7ej1ZjX2psplJbpdPgo60kWGSD8IOtIE8TG6fRYEFdlw9DGE44RULBNCHXCEfBudY3AShlM2i4h6SbY7JCgmoPu+3nEdSSP64NJa15FUERvIt9U/OB553CsiOE2yvLZhU48ahs612ZeuAm9BHwNPwnhG1CxA/8E40UXAO0XA37kINCUcRPd8W9EA/CsCfd420BQxl3zb9VHaVCRftogAD1sKTBs16N3g9yoNrBa4KAJbYq8xdSwn34Z/UOG0VVncLuNu0nOaGIo+djLH7+E7CgT0irjfhepPZ5THLWC3+J+0bVHq0L2X06ylpZen0N3zZU0MnCUC6HCrL3XUoGY+5trUOFzhV2U9K+73u1KWUvqBb8X/njE96Jch8uEDtooyNBsaM8REParxzi1etksCWoGv0bvRSbguetpbLW0wToR7kxL9gVPFF9sthbQS7jK1oK4e7DPltAhTm51jqrIaxf0JSxHrqI5BrHrshxykLaWtjRki/fYnLUU8Yfn9OGHb9Ze1jTbAV2v4knzItsq6R9zHbT1IMfpzPsu0lEvRDDGVkAfE/SlLERmDyAx5UD5gypC7xP1lV2oyNFtKWxsz5E5x3+VMToa0pbR1SRnSbXgmozKKZogJ+aPQdnWs7M8nDZf6h4qwtH1WCrnfMyLAlCGyiqqaBfMxoGj7bMqQiuq5jJJwkiFZCXFHRRlyXdzf7UxOhrSltLUxQ86K+0dcqcnQnImd8gGTL0s6EzWPZJlcAu7NuU9i13eAi5bfl7bUHLemEiIfsp21/aPl9+OEbVoqctxOIRug8hugmmppCzlA9VgpXzIN4d5nKaQV2Edyh3D3YZ8Z94twSx7CBX3W9kpLMRlqclyuTQ+YHvJznZQ0ZSWjLKxm8swiPzc7Sd5IX5yoQd/msKx1/aappE+61ZgqppNvS9+ppH5V1k3UXKRcVrlSl0Kk7fagJmGXRbYcwQ2m5QjPVRJQLfoW4EvdaEwVcsHOeSz2mc+WtNlRg9pvMteG79oEaFr0+YKdxlQxn3zbWS/6BPhcBPqDbYAp4hD5trNeFg1qKzu5ccA8FwFXOQvJt9ltHG5q9pUI/AzVMYE6KEw7J8l1hlaYNp+JfAfOGLORfFs533wG1AEnuZH0BhFJFdCM/vJuDCIiUzEMbV/BhFCH3s0NbAMz0BuqfkLYVzBBmPajnB90pNsMka4IOtIEsArdLh+GEbGpWPYCT4cReUyZib6tX6j7UT4CXBMCrpHOjZQfxbyRcugdniWYtxpvCFtIhIxH7e4j3SMvRyXoVfR68wLpKCktmDfjfyNKUaB+IEpRV6juNmUmaq97me51EWrKw3SgSy/VOVtlFeYzqWJzoMsA2ZFHMWQ15kPBjpFsN4vfMYB9qHY01ixGdftMVdgGkuVqGY7yQ5mOjL1OhL2pcmlCP2Nk4OogwBMDHLIQ/wOMj2K/KiB0ih29+j3xPHp1NmpU1E934tvElyh8VPYR1IEwUU4xGoaaHSJdQrJkB+4oDIt64n18958FdFXd8d25NKFOCTD1xHKvs8AnqD6/7VKIXMahZqF/SuEjxfs9jbsJua2Iqv6eCKxBue1Ny+okZ1Cbf51ELSBqR5Wobu/vDe+5EcBo1Ns8BmXMRtSLMJHS/Gx9qD2KNwG/lZSaKqIBtRe6dNBFcZ33tKTJOerLENSpC59hPvo7qKvLi3MOFtM7XRK3LieoKmwyqnGfjZrK7+o0nz6U12C/dx0kZtunxzFDJHWoNmCgLWhCHe86ErUzwigGe0DdKC9sl/e5E2hjsN1pI2YZkJGRkZGR4Yr/AbXaYDVZCpwQAAAAAElFTkSuQmCC" style="height:30px;">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<a href="https://bookdown.org/yihui/rmarkdown-cookbook/kable.html">kable</a>

</p>

</div>

</td>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<code>as\_kable()</code>

</p>

</div>

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,255,186,0.8); color: #000000;">

<img cid="ylitqmavgphj__icons8-neutral-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,255,186,0.8); color: #000000;">

<img cid="rikqvdplgbsz__icons8-neutral-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,255,186,0.8); color: #000000;">

<img cid="lbzgnyxkcwth__icons8-neutral-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,255,186,0.8); color: #000000;">

<img cid="tkmcsurvonaz__icons8-neutral-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<a href="https://davidgohel.github.io/flextable/articles/overview.html">flextable</a>

</p>

</div>

</td>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<code>as\_flextable()</code>

</p>

</div>

</td>

<td class="gt_row gt_center" style="background-color: rgba(186,255,201,0.8); color: #000000;">

<img cid="asmjihnwxtkq__icons8-smiling-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(186,255,201,0.8); color: #000000;">

<img cid="xydrpfuiamgh__icons8-smiling-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,179,186,0.8); color: #000000;">

<img cid="xtblfvmdysuq__icons8-no-entry-100.png" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAABkCAYAAABw4pVUAAAABmJLR0QA/wD/AP+gvaeTAAAHnklEQVR4nO2dS2xVRRjHf5UGWkADmGgQE7VoS6EK1IUYQdAgKg/B8AzBvRKNS8OKhxhINCSGxAdEFxjBBQq4lYgJ6MIgCEYsBVqIQcCIPFpaxGJdzGl67zdz7mvmvO45v+Sk9zTnzvznmzOP+80LMjIyMjIyKqMmagElUA9MABqBJu/zQ8AI7xrt/QW4AVzx/t4AOoA2oN272oDeELWXTRwzpBaYDMz2rhnAMEdh9wHHgP3edQi46SjsqqIWmAvsAnqA/pCuHmAn8CIwJPBUJoDxwBbgAuFlgt91wdMyPtAUFyGqKqsFWAMsp7Q38xRwgsG2oB34G9VOXAW6vedGAqNQbcoYVLszcE0CHi4hrtvAF8Am4NeSUpNgmoG9wH8Ufls7gG3ASmCsw/jHemFuBzqLaLgN7EF1IqqO4cA6VAPqZ4ArwA5UQx5WyX0ceB+4VEDXLe+ZkSFpCpxFwDn8E3wYWAIMjUqgF/dS4Cf8dZ4FFkakzwl1qDfLL4GHgAXEr+s9HfgGf907UCU+UTQDxzEn6DSqixt35gFnMKfhGOpHaiJYhur1yET0AmtRJScp1APrUdplerqAxdFJK43XUb0TKf4oCXqjDExAlQpTT2x1hLoK8hZVVOcaKNQmbo5Ql5GtmN0Sy6IUFRArMLt3tkYpKpe3Mf+mmBGlqICZBvyFnu71UYoCeA2zT2hylKJCYhLwO3r634xK0DL0Bvw8aqwiLTSg0iwb+tB7X43AdSHkKjAlbCExoAXl6JRd4uawBNQBPwsBPVR3m1GMaSjPc65NfkH9jgmc7ej1ZjX2psplJbpdPgo60kWGSD8IOtIE8TG6fRYEFdlw9DGE44RULBNCHXCEfBudY3AShlM2i4h6SbY7JCgmoPu+3nEdSSP64NJa15FUERvIt9U/OB553CsiOE2yvLZhU48ahs612ZeuAm9BHwNPwnhG1CxA/8E40UXAO0XA37kINCUcRPd8W9EA/CsCfd420BQxl3zb9VHaVCRftogAD1sKTBs16N3g9yoNrBa4KAJbYq8xdSwn34Z/UOG0VVncLuNu0nOaGIo+djLH7+E7CgT0irjfhepPZ5THLWC3+J+0bVHq0L2X06ylpZen0N3zZU0MnCUC6HCrL3XUoGY+5trUOFzhV2U9K+73u1KWUvqBb8X/njE96Jch8uEDtooyNBsaM8REParxzi1etksCWoGv0bvRSbguetpbLW0wToR7kxL9gVPFF9sthbQS7jK1oK4e7DPltAhTm51jqrIaxf0JSxHrqI5BrHrshxykLaWtjRki/fYnLUU8Yfn9OGHb9Ze1jTbAV2v4knzItsq6R9zHbT1IMfpzPsu0lEvRDDGVkAfE/SlLERmDyAx5UD5gypC7xP1lV2oyNFtKWxsz5E5x3+VMToa0pbR1SRnSbXgmozKKZogJ+aPQdnWs7M8nDZf6h4qwtH1WCrnfMyLAlCGyiqqaBfMxoGj7bMqQiuq5jJJwkiFZCXFHRRlyXdzf7UxOhrSltLUxQ86K+0dcqcnQnImd8gGTL0s6EzWPZJlcAu7NuU9i13eAi5bfl7bUHLemEiIfsp21/aPl9+OEbVoqctxOIRug8hugmmppCzlA9VgpXzIN4d5nKaQV2Edyh3D3YZ8Z94twSx7CBX3W9kpLMRlqclyuTQ+YHvJznZQ0ZSWjLKxm8swiPzc7Sd5IX5yoQd/msKx1/aappE+61ZgqppNvS9+ppH5V1k3UXKRcVrlSl0Kk7fagJmGXRbYcwQ2m5QjPVRJQLfoW4EvdaEwVcsHOeSz2mc+WtNlRg9pvMteG79oEaFr0+YKdxlQxn3zbWS/6BPhcBPqDbYAp4hD5trNeFg1qKzu5ccA8FwFXOQvJt9ltHG5q9pUI/AzVMYE6KEw7J8l1hlaYNp+JfAfOGLORfFs533wG1AEnuZH0BhFJFdCM/vJuDCIiUzEMbV/BhFCH3s0NbAMz0BuqfkLYVzBBmPajnB90pNsMka4IOtIEsArdLh+GEbGpWPYCT4cReUyZib6tX6j7UT4CXBMCrpHOjZQfxbyRcugdniWYtxpvCFtIhIxH7e4j3SMvRyXoVfR68wLpKCktmDfjfyNKUaB+IEpRV6juNmUmaq97me51EWrKw3SgSy/VOVtlFeYzqWJzoMsA2ZFHMWQ15kPBjpFsN4vfMYB9qHY01ixGdftMVdgGkuVqGY7yQ5mOjL1OhL2pcmlCP2Nk4OogwBMDHLIQ/wOMj2K/KiB0ih29+j3xPHp1NmpU1E934tvElyh8VPYR1IEwUU4xGoaaHSJdQrJkB+4oDIt64n18958FdFXd8d25NKFOCTD1xHKvs8AnqD6/7VKIXMahZqF/SuEjxfs9jbsJua2Iqv6eCKxBue1Ny+okZ1Cbf51ELSBqR5Wobu/vDe+5EcBo1Ns8BmXMRtSLMJHS/Gx9qD2KNwG/lZSaKqIBtRe6dNBFcZ33tKTJOerLENSpC59hPvo7qKvLi3MOFtM7XRK3LieoKmwyqnGfjZrK7+o0nz6U12C/dx0kZtunxzFDJHWoNmCgLWhCHe86ErUzwigGe0DdKC9sl/e5E2hjsN1pI2YZkJGRkZGR4Yr/AbXaYDVZCpwQAAAAAElFTkSuQmCC" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(186,255,201,0.8); color: #000000;">

<img cid="pukqylznvfat__icons8-smiling-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<a href="http://haozhu233.github.io/kableExtra/">kableExtra</a>

</p>

</div>

</td>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<code>as\_kable\_extra()</code>

</p>

</div>

</td>

<td class="gt_row gt_center" style="background-color: rgba(186,255,201,0.8); color: #000000;">

<img cid="asmjihnwxtkq__icons8-smiling-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(186,255,201,0.8); color: #000000;">

<img cid="xydrpfuiamgh__icons8-smiling-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,179,186,0.8); color: #000000;">

<img cid="xtblfvmdysuq__icons8-no-entry-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,179,186,0.8); color: #000000;">

<img cid="dezatolvsgun__icons8-no-entry-100.png" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAABkCAYAAABw4pVUAAAABmJLR0QA/wD/AP+gvaeTAAAHnklEQVR4nO2dS2xVRRjHf5UGWkADmGgQE7VoS6EK1IUYQdAgKg/B8AzBvRKNS8OKhxhINCSGxAdEFxjBBQq4lYgJ6MIgCEYsBVqIQcCIPFpaxGJdzGl67zdz7mvmvO45v+Sk9zTnzvznmzOP+80LMjIyMjIyKqMmagElUA9MABqBJu/zQ8AI7xrt/QW4AVzx/t4AOoA2oN272oDeELWXTRwzpBaYDMz2rhnAMEdh9wHHgP3edQi46SjsqqIWmAvsAnqA/pCuHmAn8CIwJPBUJoDxwBbgAuFlgt91wdMyPtAUFyGqKqsFWAMsp7Q38xRwgsG2oB34G9VOXAW6vedGAqNQbcoYVLszcE0CHi4hrtvAF8Am4NeSUpNgmoG9wH8Ufls7gG3ASmCsw/jHemFuBzqLaLgN7EF1IqqO4cA6VAPqZ4ArwA5UQx5WyX0ceB+4VEDXLe+ZkSFpCpxFwDn8E3wYWAIMjUqgF/dS4Cf8dZ4FFkakzwl1qDfLL4GHgAXEr+s9HfgGf907UCU+UTQDxzEn6DSqixt35gFnMKfhGOpHaiJYhur1yET0AmtRJScp1APrUdplerqAxdFJK43XUb0TKf4oCXqjDExAlQpTT2x1hLoK8hZVVOcaKNQmbo5Ql5GtmN0Sy6IUFRArMLt3tkYpKpe3Mf+mmBGlqICZBvyFnu71UYoCeA2zT2hylKJCYhLwO3r634xK0DL0Bvw8aqwiLTSg0iwb+tB7X43AdSHkKjAlbCExoAXl6JRd4uawBNQBPwsBPVR3m1GMaSjPc65NfkH9jgmc7ej1ZjX2psplJbpdPgo60kWGSD8IOtIE8TG6fRYEFdlw9DGE44RULBNCHXCEfBudY3AShlM2i4h6SbY7JCgmoPu+3nEdSSP64NJa15FUERvIt9U/OB553CsiOE2yvLZhU48ahs612ZeuAm9BHwNPwnhG1CxA/8E40UXAO0XA37kINCUcRPd8W9EA/CsCfd420BQxl3zb9VHaVCRftogAD1sKTBs16N3g9yoNrBa4KAJbYq8xdSwn34Z/UOG0VVncLuNu0nOaGIo+djLH7+E7CgT0irjfhepPZ5THLWC3+J+0bVHq0L2X06ylpZen0N3zZU0MnCUC6HCrL3XUoGY+5trUOFzhV2U9K+73u1KWUvqBb8X/njE96Jch8uEDtooyNBsaM8REParxzi1etksCWoGv0bvRSbguetpbLW0wToR7kxL9gVPFF9sthbQS7jK1oK4e7DPltAhTm51jqrIaxf0JSxHrqI5BrHrshxykLaWtjRki/fYnLUU8Yfn9OGHb9Ze1jTbAV2v4knzItsq6R9zHbT1IMfpzPsu0lEvRDDGVkAfE/SlLERmDyAx5UD5gypC7xP1lV2oyNFtKWxsz5E5x3+VMToa0pbR1SRnSbXgmozKKZogJ+aPQdnWs7M8nDZf6h4qwtH1WCrnfMyLAlCGyiqqaBfMxoGj7bMqQiuq5jJJwkiFZCXFHRRlyXdzf7UxOhrSltLUxQ86K+0dcqcnQnImd8gGTL0s6EzWPZJlcAu7NuU9i13eAi5bfl7bUHLemEiIfsp21/aPl9+OEbVoqctxOIRug8hugmmppCzlA9VgpXzIN4d5nKaQV2Edyh3D3YZ8Z94twSx7CBX3W9kpLMRlqclyuTQ+YHvJznZQ0ZSWjLKxm8swiPzc7Sd5IX5yoQd/msKx1/aappE+61ZgqppNvS9+ppH5V1k3UXKRcVrlSl0Kk7fagJmGXRbYcwQ2m5QjPVRJQLfoW4EvdaEwVcsHOeSz2mc+WtNlRg9pvMteG79oEaFr0+YKdxlQxn3zbWS/6BPhcBPqDbYAp4hD5trNeFg1qKzu5ccA8FwFXOQvJt9ltHG5q9pUI/AzVMYE6KEw7J8l1hlaYNp+JfAfOGLORfFs533wG1AEnuZH0BhFJFdCM/vJuDCIiUzEMbV/BhFCH3s0NbAMz0BuqfkLYVzBBmPajnB90pNsMka4IOtIEsArdLh+GEbGpWPYCT4cReUyZib6tX6j7UT4CXBMCrpHOjZQfxbyRcugdniWYtxpvCFtIhIxH7e4j3SMvRyXoVfR68wLpKCktmDfjfyNKUaB+IEpRV6juNmUmaq97me51EWrKw3SgSy/VOVtlFeYzqWJzoMsA2ZFHMWQ15kPBjpFsN4vfMYB9qHY01ixGdftMVdgGkuVqGY7yQ5mOjL1OhL2pcmlCP2Nk4OogwBMDHLIQ/wOMj2K/KiB0ih29+j3xPHp1NmpU1E934tvElyh8VPYR1IEwUU4xGoaaHSJdQrJkB+4oDIt64n18958FdFXd8d25NKFOCTD1xHKvs8AnqD6/7VKIXMahZqF/SuEjxfs9jbsJua2Iqv6eCKxBue1Ny+okZ1Cbf51ELSBqR5Wobu/vDe+5EcBo1Ns8BmXMRtSLMJHS/Gx9qD2KNwG/lZSaKqIBtRe6dNBFcZ33tKTJOerLENSpC59hPvo7qKvLi3MOFtM7XRK3LieoKmwyqnGfjZrK7+o0nz6U12C/dx0kZtunxzFDJHWoNmCgLWhCHe86ErUzwigGe0DdKC9sl/e5E2hjsN1pI2YZkJGRkZGR4Yr/AbXaYDVZCpwQAAAAAElFTkSuQmCC" style="height:30px;">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<a href="https://tibble.tidyverse.org/">tibble</a>

</p>

</div>

</td>

<td class="gt_row gt_left">

<div class="gt_from_md">

<p>

<code>as\_tibble()</code>

</p>

</div>

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,223,186,0.8); color: #000000;">

<img cid="sxbtknzecaoi__icons8-disappointed-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,223,186,0.8); color: #000000;">

<img cid="uijcgdykfvpw__icons8-disappointed-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,223,186,0.8); color: #000000;">

<img cid="fqcrdntwejhy__icons8-disappointed-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

<td class="gt_row gt_center" style="background-color: rgba(255,223,186,0.8); color: #000000;">

<img cid="zhtagonuckle__icons8-disappointed-100.png" src="data:image/png;base64,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" style="height:30px;">

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

<!-- Printing key for table of output types. -->

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#erztunkysz .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: x-small;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#erztunkysz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#erztunkysz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#erztunkysz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#erztunkysz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#erztunkysz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#erztunkysz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#erztunkysz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#erztunkysz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#erztunkysz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#erztunkysz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#erztunkysz .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#erztunkysz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#erztunkysz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#erztunkysz .gt_from_md > :first-child {
  margin-top: 0;
}

#erztunkysz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#erztunkysz .gt_row {
  padding-top: 3px;
  padding-bottom: 3px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#erztunkysz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#erztunkysz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#erztunkysz .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#erztunkysz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#erztunkysz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#erztunkysz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#erztunkysz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#erztunkysz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#erztunkysz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#erztunkysz .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#erztunkysz .gt_left {
  text-align: left;
}

#erztunkysz .gt_center {
  text-align: center;
}

#erztunkysz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#erztunkysz .gt_font_normal {
  font-weight: normal;
}

#erztunkysz .gt_font_bold {
  font-weight: bold;
}

#erztunkysz .gt_font_italic {
  font-style: italic;
}

#erztunkysz .gt_super {
  font-size: 65%;
}

#erztunkysz .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="erztunkysz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

<strong>Key</strong>

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_center" style="background-color: rgba(186,255,201,0.8); color: #000000;">

<img cid="ynwskqhgxvct__icons8-smiling-100.png" src="data:image/png;base64,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" style="height:20px;">

</td>

<td class="gt_row gt_left">

Output fully supported

</td>

</tr>

<tr>

<td class="gt_row gt_center" style="background-color: rgba(255,255,186,0.8); color: #000000;">

<img cid="zjscqtarhoml__icons8-neutral-100.png" src="data:image/png;base64,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" style="height:20px;">

</td>

<td class="gt_row gt_left">

Formatted output, but missing indentation, footnotes, spanning headers

</td>

</tr>

<tr>

<td class="gt_row gt_center" style="background-color: rgba(255,223,186,0.8); color: #000000;">

<img cid="hiwkugratsyp__icons8-disappointed-100.png" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAABkCAYAAABw4pVUAAAABmJLR0QA/wD/AP+gvaeTAAAJ70lEQVR4nO2dfYxVRxXAf+/xsYBst5YmhqawS6FAQCtabauuWGApbVJiqFpTFzaYoMagGGOaNJoYrV+tH21aTbRNtRVsa7CmifYPNWgjSG2rVFsKZbeVthCgCm6LbGHZL/zj3Af3zsx9737MvfP2vftLJry3vHvm3Jm5c2bOnJkLBQUFBQUFySi5ViACU4CFwHxggff5EuAtXnqr9y/Am8Dr3r8DwMvAPqAX6PM+D+aoe2zqsUImAu8EurzUiVSKDUaAZ4FtXtoBnLYku6EoI4W/GTgBnMkpnQB+DqzwdGh6ZgO3AQfJrxLC0kHgO55OznDVZV0K3AKsAyZF+P1+YA/nbEEv0I/YijcQewEwHTgfsSkzELszH7E7ixDbU4shYAvSUF6KdDfjmEuAh5G+vFprPQDcj1TYxRbzvxjoAR6g9lM5AjwIzLGYf90wCfgC0pLDCuA4YkO6yO/JvRy4CzhWRa+TwNeAlpx0ypxrkG4m7Ib3IK12qisFvbx7PF3C9OxFGsu4ZSLSskYx3+A/kUKY4Eg/EyVgNfAUZp3HkCdqsisFk9JB+E0dAj7uTLPo3IToarqHvwLt7lSLx3XIbFm9iWHgDqDVnWqxOQ+4E9FdvZ9+YJU71aLRgwwbVeX7gHc71CstlwMvot/XELDWoV5V2YTZXvwamRuMd1qRIbvJrtzsUC8j38TcRX3WpVIZsRFzF/YNl0r5+Ry6coPADS6VypjViJdAve8vuVQKoBu9m3od8dA2OlcCR9G7r/WuFLoO3YD3A5e5UsgBlyH3rBr63Edfs4H/KoqcpDmeDJUr0V1C/chcLBcmAU8oCowAa/JSoA65Ht3QP0VOM/o70I1ZI46m4mIa3Hwv60xXIobLn+kvs850HPErdCO/IqvMWpAgAX+GLyLuhQKhFfEKq16KTFz3X1UyGmZ8u0Oy4r3oC3Bftp1JO/pE6Ae2M2kg7kIfgVpdeVR9OIcYX17bvGkDjhAssy22hM9HfwTHw3qGa7rRu/i5NgT/TBH8DPUZYFeP7CJYdvemFTgLiezzC23mCWBcbkR3q6RaabxNEbiHIsIvDmXgBYJl+O00wtTYpZ70OjYd6wmW4askbNQrFUHHgWlWVGwupiLRlf6yXB7242o1tU75vhUZTxfE4xTwqPI3tWxrMhk9Cn1patWal+XovU2UmOazfFARcIBiqJuGMnqM1wfCfmhC9VBu84QUJGMM+KPyt2WmH4ZViPrjx9NqVKCVobFCTExFokb8j5fNLQHNymyCZXqKiFv1ligX/isjBZuRlwmWrRYQYuqyFijf99jXq2nZq3yfr/4gSoX0WlOnQC1LtayNFaLWmusKWQO8hoQdfTTB9R9DAtoO494xuk/5rlWIiZ0E+7kPWVYqDj0E12KOxby+RHC72ggJZskWWUawbHdEuWi3ctHbs9KuBjejh6m+lkDOvxUZo7iLwX2HosuzUS5SRwKp/PcJaEM2f6oxTkMk63JuwLxn5QHyj5jpUHTYH+UiNUT0goyUM3EN4qZRC+806aLpr0fG/arcw97/5cWFSv5Ho1ykrhDmEQ55KXLEhRqEV1Hahh1bhnn78xjytMyzkEctWpS8Ix2Ek1eFlID3AQ8RfpDAM0Q7fSEq84B/hORVOSjgKrJzpCaqELUVzbCkTCsyQOgGfkj1kxSGkeXjLBpDC/Bdqp8mcQC4G/gEsBh7IU8zlHwidVm2jHo3+kpZlPRn5HimrFkCbE+g3xvevSWhQ5EVyag/p1yUZNg7AX0jS7U0BvwWWYfJm6XAY5jtV1jqJ9mBBzWHvRMNF/1P+Z6kyxpFup1a9CHLm1tw5zPb7qXFyKRxDQYfk8Iwco9xuVD5rpa1kV8QrMVPJcgYZAj7PNKa+pHH8y/Aj4FPI8cl1SuLEB1/gui8n3P3sRu5tyR8hmDZblZ/YHpC+pTvtVpLGH/A3Sw/LXvRPbM2qOm4NTkXa3okCxKTqEJUj+Ria+oUqN10JE/6FPQl3Fl29WpK2gmW6SkMO6tMT8gg8KTyt8gL8gWhqJE8T2A4ojYs6uRx5XtRIelRy/BPcS7uJPh4HaQIlEtDGfEs+8v0/XEETEYmLX4BNjyuzcoKgmUZGkoa1mUNYSFAuOAsatk9QjRPRoAu9FottiPEZxp6b3N1EkH1umHnImADsqi0C/gPMjIc9D7v8v5vAzDTiYZB1hMsw8QbdkDOQvcLc7WlrXJ86++J55UdBX6HLNO6GJSYtrR9K41A06bPj6TTMTadwNNEr4Sw9CQxRzYWUDd9DmIhTvo+Regu8mltLcCPiPdERHli8joIuYQcFu3P/x4bguehL3feZENwFS5CDisOK9jdwK3IPsiZiLtnivd5JXIg5fNVrt9J9vZlnZLnMBbjAx5UhB8mu5imd6FPoippO/FWFZci6xkmWYe8vLKgDQnq8+enrX2kYQ6y4dOfwZ02M/BYgh4XVmkA3STrKktIa1XPHjmDBHRksX5/t5LPABm8KOYrSibDyInPtliEOW5qB/A2C/Jnosctn0EiPxZakF/hCvQu/haL8s/Sgn44l60DzC7AfJT3/dg1wJOROYqaTy/ytre0nI9scPLL3kuGg4gu9FHPVgtyH0MvpHvJZs5TBn5qyO83FmQ/osgcIwdP+ffRb2ZjCnkbDPIeJtuhdRlpSGq+G1LI3GSQd3s6NaMxCX1IOozMpONSQlwJfllPk8/bdqYCf1fyfoVkDeHD6HZjJzEPB0jDLHQDfJL4gW4lgvs3Bkge5ZKEuQQdf0n2n1yFfvxhrgcpV1iFnaPGb0RGOkdw84KUa5GKOIpsf4uD6ajx08jk1Alr0Y18P/GPHC/jdkWyRPwBRCd6ZYwiAdpO2YhuzAbJ3wmZJ6vRJ8pngC+6VMrPrejKDSPHbzcamzBvZfi6S6VMfB7zK48exc6kyzXnIceqq/dXl688qrAW8wbLl4D3ONQrLVegz8ArBty5zajFKswOwhFkLaLNnWqxaUMchaYu6hgOR1NxaSd8PeMIyT23eVHNQ1yZ9I278Npar159jvp79WoZGUH9DbPOlVev5jYDz4IV6K+58KcXkMgMlyFG04BPogck+NNeGiiktvL6bvVgTdX1shVpoXk8NWVkcncPetyUP71Jg72+208Hsp/Q9GJGfzqMLHmux+5KWzvyJGwhfJnYP4/abDn/mrgyrHORVbQeoi3evIJ0GfuQxaQ+xG0xgGxTHvB+Nx1ZJJqOLHotQJyVC5EVyY4IeQ0hi1i3E3HbciMxCwkcM51vknd61dOlOF8S6c+XIy3zOPlVwnHkdRxXUycvGajHucAEJPqky0udRDy9MwIjyGb9bV7ajnRRdUM9VohKC2IL/PZgDmInWjlnM+CcTTnhfd6P2JyK3enFsI2soKCgoKCgEfg/DAcXjgAZEpUAAAAASUVORK5CYII=" style="height:20px;">

</td>

<td class="gt_row gt_left">

No formatted output

</td>

</tr>

<tr>

<td class="gt_row gt_center" style="background-color: rgba(255,179,186,0.8); color: #000000;">

<img cid="isapedyukgox__icons8-no-entry-100.png" src="data:image/png;base64,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" style="height:20px;">

</td>

<td class="gt_row gt_left">

Output not supported

</td>

</tr>

<tr>

<td class="gt_row gt_center" style="background-color: rgba(186,225,255,0.8); color: #000000;">

<img cid="gmcwvhesnfpt__icons8-under-construction-100.png" src="data:image/png;base64,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" style="height:20px;">

</td>

<td class="gt_row gt_left">

Under development

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

## Contributing

Please note that the {gtsummary} project is released with a [Contributor
Code of
Conduct](http://www.danieldsjoberg.com/gtsummary/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms. A big
thank you to all contributors\!  
[@ablack3](https://github.com/ablack3),
[@ahinton-mmc](https://github.com/ahinton-mmc),
[@davidkane9](https://github.com/davidkane9),
[@ddsjoberg](https://github.com/ddsjoberg),
[@emilyvertosick](https://github.com/emilyvertosick),
[@jeanmanguy](https://github.com/jeanmanguy),
[@jennybc](https://github.com/jennybc),
[@jflynn264](https://github.com/jflynn264),
[@jwilliman](https://github.com/jwilliman),
[@karissawhiting](https://github.com/karissawhiting),
[@leejasme](https://github.com/leejasme),
[@ltin1214](https://github.com/ltin1214),
[@margarethannum](https://github.com/margarethannum),
[@michaelcurry1123](https://github.com/michaelcurry1123),
[@MyKo101](https://github.com/MyKo101),
[@oranwutang](https://github.com/oranwutang),
[@ryzhu75](https://github.com/ryzhu75),
[@sammo3182](https://github.com/sammo3182),
[@slobaugh](https://github.com/slobaugh),
[@tormodb](https://github.com/tormodb), and
[@zabore](https://github.com/zabore)
