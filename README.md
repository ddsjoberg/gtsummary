
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/ddsjoberg/gtsummary.svg?branch=master)](https://travis-ci.org/ddsjoberg/gtsummary)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/gtsummary?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/gtsummary)
[![Coverage
status](https://codecov.io/gh/ddsjoberg/gtsummary/branch/master/graph/badge.svg)](https://codecov.io/github/ddsjoberg/gtsummary?branch=master)

## gtsummary

A collection of functions commonly used in the work of the
biostatisticians. The goal of gtsummary is to make reporting of tabular
analytic results simple, beautiful, and
reproducible.  
<!-- Update the list of contributors from the git shell `git shortlog -s -n` -->

## Installation

You can install the production version of gtsummary with:

``` r
install.packages("remotes")
remotes::install_github("ddsjoberg/gtsummary")
```

and the development version with:

``` r
remotes::install_github("ddsjoberg/gtsummary", ref = "dev")
```

## Examples

The vignettes/tutorials for the primary gtsummary functions have
detailed examples and can be found at
[danieldsjoberg.com/gtsummary](http://www.danieldsjoberg.com/gtsummary).
Each vignette is an Rmarkdown file (\*.Rmd) and a copy of the files can
be found here:
<https://github.com/ddsjoberg/gtsummary/tree/master/vignettes>.

### Summary Table

``` r
library(gtsummary)
#> Loading required package: gt
tbl_summary(trial, by = "trt") %>% 
  add_comparison() 
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#xcjnhjqggs .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #000000;
  font-size: 16px;
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
}

#xcjnhjqggs .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
}

#xcjnhjqggs .gt_title {
  color: #000000;
  font-size: 125%;
  /* heading.title.font.size */
  padding-top: 4px;
  /* heading.top.padding */
  padding-bottom: 1px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xcjnhjqggs .gt_subtitle {
  color: #000000;
  font-size: 85%;
  /* heading.subtitle.font.size */
  padding-top: 1px;
  padding-bottom: 4px;
  /* heading.bottom.padding */
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xcjnhjqggs .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* heading.border.bottom.color */
}

#xcjnhjqggs .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  padding-top: 4px;
  padding-bottom: 4px;
}

#xcjnhjqggs .gt_col_heading {
  color: #000000;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 16px;
  /* column_labels.font.size */
  font-weight: initial;
  /* column_labels.font.weight */
  vertical-align: middle;
  padding: 10px;
  margin: 10px;
}

#xcjnhjqggs .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#xcjnhjqggs .gt_group_heading {
  padding: 8px;
  color: #000000;
  background-color: #FFFFFF;
  /* stub_group.background.color */
  font-size: 16px;
  /* stub_group.font.size */
  font-weight: initial;
  /* stub_group.font.weight */
  border-top-style: solid;
  /* stub_group.border.top.style */
  border-top-width: 2px;
  /* stub_group.border.top.width */
  border-top-color: #A8A8A8;
  /* stub_group.border.top.color */
  border-bottom-style: solid;
  /* stub_group.border.bottom.style */
  border-bottom-width: 2px;
  /* stub_group.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* stub_group.border.bottom.color */
  vertical-align: middle;
}

#xcjnhjqggs .gt_empty_group_heading {
  padding: 0.5px;
  color: #000000;
  background-color: #FFFFFF;
  /* stub_group.background.color */
  font-size: 16px;
  /* stub_group.font.size */
  font-weight: initial;
  /* stub_group.font.weight */
  border-top-style: solid;
  /* stub_group.border.top.style */
  border-top-width: 2px;
  /* stub_group.border.top.width */
  border-top-color: #A8A8A8;
  /* stub_group.border.top.color */
  border-bottom-style: solid;
  /* stub_group.border.bottom.style */
  border-bottom-width: 2px;
  /* stub_group.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* stub_group.border.bottom.color */
  vertical-align: middle;
}

#xcjnhjqggs .gt_striped {
  background-color: #f2f2f2;
}

#xcjnhjqggs .gt_row {
  padding: 10px;
  /* row.padding */
  margin: 10px;
  vertical-align: middle;
}

#xcjnhjqggs .gt_stub {
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #A8A8A8;
  padding-left: 12px;
}

#xcjnhjqggs .gt_stub.gt_row {
  background-color: #FFFFFF;
}

#xcjnhjqggs .gt_summary_row {
  background-color: #FFFFFF;
  /* summary_row.background.color */
  padding: 6px;
  /* summary_row.padding */
  text-transform: inherit;
  /* summary_row.text_transform */
}

#xcjnhjqggs .gt_first_summary_row {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
}

#xcjnhjqggs .gt_table_body {
  border-top-style: solid;
  /* field.border.top.style */
  border-top-width: 2px;
  /* field.border.top.width */
  border-top-color: #A8A8A8;
  /* field.border.top.color */
  border-bottom-style: solid;
  /* field.border.bottom.style */
  border-bottom-width: 2px;
  /* field.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* field.border.bottom.color */
}

#xcjnhjqggs .gt_footnote {
  font-size: 90%;
  /* footnote.font.size */
  padding: 4px;
  /* footnote.padding */
}

#xcjnhjqggs .gt_sourcenote {
  font-size: 90%;
  /* sourcenote.font.size */
  padding: 4px;
  /* sourcenote.padding */
}

#xcjnhjqggs .gt_center {
  text-align: center;
}

#xcjnhjqggs .gt_left {
  text-align: left;
}

#xcjnhjqggs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xcjnhjqggs .gt_font_normal {
  font-weight: normal;
}

#xcjnhjqggs .gt_font_bold {
  font-weight: bold;
}

#xcjnhjqggs .gt_font_italic {
  font-style: italic;
}

#xcjnhjqggs .gt_super {
  font-size: 65%;
}

#xcjnhjqggs .gt_footnote_glyph {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="xcjnhjqggs" style="overflow-x:auto;">

<!--gt table start-->

<table class="gt_table">

<tr>

<th class="gt_col_heading gt_left" rowspan="1" colspan="1">

<strong>Characteristic</strong><sup class='gt_footnote_glyph'>1</sup>

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

<strong>Drug</strong>, N = 107

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

<strong>Placebo</strong>, N = 93

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

<strong>p-value</strong><sup class='gt_footnote_glyph'>2</sup>

</th>

</tr>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Age, yrs

</td>

<td class="gt_row gt_center">

47 (39, 58)

</td>

<td class="gt_row gt_center">

46 (36,
54)

</td>

<td class="gt_row gt_center">

0.3

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped" style="text-align:left;text-indent:10px;">

Unknown

</td>

<td class="gt_row gt_center gt_striped">

3

</td>

<td class="gt_row gt_center gt_striped">

5

</td>

<td class="gt_row gt_center gt_striped">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Marker Level, ng/mL

</td>

<td class="gt_row gt_center">

0.61 (0.22, 1.20)

</td>

<td class="gt_row gt_center">

0.72 (0.22,
1.63)

</td>

<td class="gt_row gt_center">

0.4

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped" style="text-align:left;text-indent:10px;">

Unknown

</td>

<td class="gt_row gt_center gt_striped">

4

</td>

<td class="gt_row gt_center gt_striped">

4

</td>

<td class="gt_row gt_center gt_striped">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

T
Stage

</td>

<td class="gt_row gt_center">

</td>

<td class="gt_row gt_center">

</td>

<td class="gt_row gt_center">

0.13

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped" style="text-align:left;text-indent:10px;">

T1

</td>

<td class="gt_row gt_center gt_striped">

25 (23%)

</td>

<td class="gt_row gt_center gt_striped">

26 (28%)

</td>

<td class="gt_row gt_center gt_striped">

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">

T2

</td>

<td class="gt_row gt_center">

26 (24%)

</td>

<td class="gt_row gt_center">

23
(25%)

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped" style="text-align:left;text-indent:10px;">

T3

</td>

<td class="gt_row gt_center gt_striped">

29 (27%)

</td>

<td class="gt_row gt_center gt_striped">

13 (14%)

</td>

<td class="gt_row gt_center gt_striped">

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">

T4

</td>

<td class="gt_row gt_center">

27 (25%)

</td>

<td class="gt_row gt_center">

31 (33%)

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped">

Grade

</td>

<td class="gt_row gt_center gt_striped">

</td>

<td class="gt_row gt_center gt_striped">

</td>

<td class="gt_row gt_center gt_striped">

0.3

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">

I

</td>

<td class="gt_row gt_center">

38 (36%)

</td>

<td class="gt_row gt_center">

29
(31%)

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped" style="text-align:left;text-indent:10px;">

II

</td>

<td class="gt_row gt_center gt_striped">

34 (32%)

</td>

<td class="gt_row gt_center gt_striped">

24 (26%)

</td>

<td class="gt_row gt_center gt_striped">

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">

III

</td>

<td class="gt_row gt_center">

35 (33%)

</td>

<td class="gt_row gt_center">

40 (43%)

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped">

Tumor Response

</td>

<td class="gt_row gt_center gt_striped">

52 (51%)

</td>

<td class="gt_row gt_center gt_striped">

30 (33%)

</td>

<td class="gt_row gt_center gt_striped">

0.017

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">

Unknown

</td>

<td class="gt_row gt_center">

6

</td>

<td class="gt_row gt_center">

3

</td>

<td class="gt_row gt_center">

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="4" class="gt_footnote">

<sup class='gt_footnote_glyph'><em>1</em></sup> Statistics presented:
median (IQR) for continuous variables, n (%) for categorical
variables<br /><sup class='gt_footnote_glyph'><em>2</em></sup>
Statistical tests performed: Wilcoxon signed-rank test, chi-square
test

</td>

</tr>

</tfoot>

</table>

<!--gt table end-->

</div>

<!--/html_preserve-->

### Regression Models

``` r
mod1 = glm(am ~ mpg + factor(cyl), mtcars, family = binomial(link = "logit"))
tbl_regression(
  mod1, exponentiate = TRUE, 
  label = list(`factor(cyl)` = "No. of Cylinders", mpg = "Miles per Gallon")
)
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#kfthzkfrfn .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #000000;
  font-size: 16px;
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
}

#kfthzkfrfn .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
}

#kfthzkfrfn .gt_title {
  color: #000000;
  font-size: 125%;
  /* heading.title.font.size */
  padding-top: 4px;
  /* heading.top.padding */
  padding-bottom: 1px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kfthzkfrfn .gt_subtitle {
  color: #000000;
  font-size: 85%;
  /* heading.subtitle.font.size */
  padding-top: 1px;
  padding-bottom: 4px;
  /* heading.bottom.padding */
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kfthzkfrfn .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* heading.border.bottom.color */
}

#kfthzkfrfn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  padding-top: 4px;
  padding-bottom: 4px;
}

#kfthzkfrfn .gt_col_heading {
  color: #000000;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 16px;
  /* column_labels.font.size */
  font-weight: initial;
  /* column_labels.font.weight */
  vertical-align: middle;
  padding: 10px;
  margin: 10px;
}

#kfthzkfrfn .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#kfthzkfrfn .gt_group_heading {
  padding: 8px;
  color: #000000;
  background-color: #FFFFFF;
  /* stub_group.background.color */
  font-size: 16px;
  /* stub_group.font.size */
  font-weight: initial;
  /* stub_group.font.weight */
  border-top-style: solid;
  /* stub_group.border.top.style */
  border-top-width: 2px;
  /* stub_group.border.top.width */
  border-top-color: #A8A8A8;
  /* stub_group.border.top.color */
  border-bottom-style: solid;
  /* stub_group.border.bottom.style */
  border-bottom-width: 2px;
  /* stub_group.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* stub_group.border.bottom.color */
  vertical-align: middle;
}

#kfthzkfrfn .gt_empty_group_heading {
  padding: 0.5px;
  color: #000000;
  background-color: #FFFFFF;
  /* stub_group.background.color */
  font-size: 16px;
  /* stub_group.font.size */
  font-weight: initial;
  /* stub_group.font.weight */
  border-top-style: solid;
  /* stub_group.border.top.style */
  border-top-width: 2px;
  /* stub_group.border.top.width */
  border-top-color: #A8A8A8;
  /* stub_group.border.top.color */
  border-bottom-style: solid;
  /* stub_group.border.bottom.style */
  border-bottom-width: 2px;
  /* stub_group.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* stub_group.border.bottom.color */
  vertical-align: middle;
}

#kfthzkfrfn .gt_striped {
  background-color: #f2f2f2;
}

#kfthzkfrfn .gt_row {
  padding: 10px;
  /* row.padding */
  margin: 10px;
  vertical-align: middle;
}

#kfthzkfrfn .gt_stub {
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #A8A8A8;
  padding-left: 12px;
}

#kfthzkfrfn .gt_stub.gt_row {
  background-color: #FFFFFF;
}

#kfthzkfrfn .gt_summary_row {
  background-color: #FFFFFF;
  /* summary_row.background.color */
  padding: 6px;
  /* summary_row.padding */
  text-transform: inherit;
  /* summary_row.text_transform */
}

#kfthzkfrfn .gt_first_summary_row {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
}

#kfthzkfrfn .gt_table_body {
  border-top-style: solid;
  /* field.border.top.style */
  border-top-width: 2px;
  /* field.border.top.width */
  border-top-color: #A8A8A8;
  /* field.border.top.color */
  border-bottom-style: solid;
  /* field.border.bottom.style */
  border-bottom-width: 2px;
  /* field.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* field.border.bottom.color */
}

#kfthzkfrfn .gt_footnote {
  font-size: 90%;
  /* footnote.font.size */
  padding: 4px;
  /* footnote.padding */
}

#kfthzkfrfn .gt_sourcenote {
  font-size: 90%;
  /* sourcenote.font.size */
  padding: 4px;
  /* sourcenote.padding */
}

#kfthzkfrfn .gt_center {
  text-align: center;
}

#kfthzkfrfn .gt_left {
  text-align: left;
}

#kfthzkfrfn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kfthzkfrfn .gt_font_normal {
  font-weight: normal;
}

#kfthzkfrfn .gt_font_bold {
  font-weight: bold;
}

#kfthzkfrfn .gt_font_italic {
  font-style: italic;
}

#kfthzkfrfn .gt_super {
  font-size: 65%;
}

#kfthzkfrfn .gt_footnote_glyph {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="kfthzkfrfn" style="overflow-x:auto;">

<!--gt table start-->

<table class="gt_table">

<tr>

<th class="gt_col_heading gt_left" rowspan="1" colspan="1">

<strong>N = 32</strong>

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

<strong>OR</strong><sup class='gt_footnote_glyph'>1</sup>

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

<strong>95% CI</strong><sup class='gt_footnote_glyph'>1</sup>

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

<strong>p-value</strong>

</th>

</tr>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Miles per Gallon

</td>

<td class="gt_row gt_center">

1.45

</td>

<td class="gt_row gt_center">

1.03, 2.40

</td>

<td class="gt_row gt_center">

0.080

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped">

No. of
Cylinders

</td>

<td class="gt_row gt_center gt_striped">

</td>

<td class="gt_row gt_center gt_striped">

</td>

<td class="gt_row gt_center gt_striped">

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">

4

</td>

<td class="gt_row gt_center">

—

</td>

<td class="gt_row gt_center">

—

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_striped" style="text-align:left;text-indent:10px;">

6

</td>

<td class="gt_row gt_center gt_striped">

2.08

</td>

<td class="gt_row gt_center gt_striped">

0.13, 39.0

</td>

<td class="gt_row gt_center gt_striped">

0.6

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">

8

</td>

<td class="gt_row gt_center">

2.02

</td>

<td class="gt_row gt_center">

0.04, 119

</td>

<td class="gt_row gt_center">

0.7

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="4" class="gt_footnote">

<sup class='gt_footnote_glyph'><em>1</em></sup> OR = Odds Ratio, CI =
Confidence Interval

</td>

</tr>

</tfoot>

</table>

<!--gt table end-->

</div>

<!--/html_preserve-->
