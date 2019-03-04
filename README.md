
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/ddsjoberg/gtsummary.svg?branch=master)](https://travis-ci.org/ddsjoberg/gtsummary)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/gtsummary?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/gtsummary)
[![Coverage
status](https://codecov.io/gh/ddsjoberg/gtsummary/branch/master/graph/badge.svg)](https://codecov.io/github/ddsjoberg/gtsummary?branch=master)  
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
install.packages("remotes")
remotes::install_github("ddsjoberg/gtsummary", ref = "dev")
```

## Examples

The vignettes/tutorials for the primary gtsummary functions have
detailed examples and can be found at
[danieldsjoberg.com/gtsummary](http://www.danieldsjoberg.com/gtsummary).
Each vignette is an Rmarkdown file (\*.Rmd) and a copy of the files can
be found here:
<https://github.com/ddsjoberg/gtsummary/tree/master/vignettes>.

### Table 1

``` r
library(gtsummary)
tbl_summary(trial, by = "trt") %>% 
  add_comparison() 
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#kjxfwrhiin .gt_table {
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

#kjxfwrhiin .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
}

#kjxfwrhiin .gt_title {
  color: #000000;
  font-size: 125%;
  /* heading.title.font.size */
  padding-top: 4px;
  /* heading.top.padding */
  padding-bottom: 1px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kjxfwrhiin .gt_subtitle {
  color: #000000;
  font-size: 85%;
  /* heading.subtitle.font.size */
  padding-top: 1px;
  padding-bottom: 4px;
  /* heading.bottom.padding */
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kjxfwrhiin .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* heading.border.bottom.color */
}

#kjxfwrhiin .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  padding-top: 4px;
  padding-bottom: 4px;
}

#kjxfwrhiin .gt_col_heading {
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

#kjxfwrhiin .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#kjxfwrhiin .gt_group_heading {
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

#kjxfwrhiin .gt_empty_group_heading {
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

#kjxfwrhiin .gt_striped {
  background-color: #f2f2f2;
}

#kjxfwrhiin .gt_row {
  padding: 10px;
  /* row.padding */
  margin: 10px;
  vertical-align: middle;
}

#kjxfwrhiin .gt_stub {
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #A8A8A8;
  padding-left: 12px;
}

#kjxfwrhiin .gt_stub.gt_row {
  background-color: #FFFFFF;
}

#kjxfwrhiin .gt_summary_row {
  background-color: #FFFFFF;
  /* summary_row.background.color */
  padding: 6px;
  /* summary_row.padding */
  text-transform: inherit;
  /* summary_row.text_transform */
}

#kjxfwrhiin .gt_first_summary_row {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
}

#kjxfwrhiin .gt_table_body {
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

#kjxfwrhiin .gt_footnote {
  font-size: 90%;
  /* footnote.font.size */
  padding: 4px;
  /* footnote.padding */
}

#kjxfwrhiin .gt_sourcenote {
  font-size: 90%;
  /* sourcenote.font.size */
  padding: 4px;
  /* sourcenote.padding */
}

#kjxfwrhiin .gt_center {
  text-align: center;
}

#kjxfwrhiin .gt_left {
  text-align: left;
}

#kjxfwrhiin .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kjxfwrhiin .gt_font_normal {
  font-weight: normal;
}

#kjxfwrhiin .gt_font_bold {
  font-weight: bold;
}

#kjxfwrhiin .gt_font_italic {
  font-style: italic;
}

#kjxfwrhiin .gt_super {
  font-size: 65%;
}

#kjxfwrhiin .gt_footnote_glyph {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="kjxfwrhiin" style="overflow-x:auto;">

<!--gt table start-->

<table class="gt_table">

<tr>

<th class="gt_col_heading gt_left" rowspan="1" colspan="1">

<strong>Characteristic</strong>

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

stat\_1

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

stat\_2

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

<strong>p-value</strong>

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

30
(33%)

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

#lwnvagzrgq .gt_table {
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

#lwnvagzrgq .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
}

#lwnvagzrgq .gt_title {
  color: #000000;
  font-size: 125%;
  /* heading.title.font.size */
  padding-top: 4px;
  /* heading.top.padding */
  padding-bottom: 1px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lwnvagzrgq .gt_subtitle {
  color: #000000;
  font-size: 85%;
  /* heading.subtitle.font.size */
  padding-top: 1px;
  padding-bottom: 4px;
  /* heading.bottom.padding */
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lwnvagzrgq .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* heading.border.bottom.color */
}

#lwnvagzrgq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  padding-top: 4px;
  padding-bottom: 4px;
}

#lwnvagzrgq .gt_col_heading {
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

#lwnvagzrgq .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#lwnvagzrgq .gt_group_heading {
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

#lwnvagzrgq .gt_empty_group_heading {
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

#lwnvagzrgq .gt_striped {
  background-color: #f2f2f2;
}

#lwnvagzrgq .gt_row {
  padding: 10px;
  /* row.padding */
  margin: 10px;
  vertical-align: middle;
}

#lwnvagzrgq .gt_stub {
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #A8A8A8;
  padding-left: 12px;
}

#lwnvagzrgq .gt_stub.gt_row {
  background-color: #FFFFFF;
}

#lwnvagzrgq .gt_summary_row {
  background-color: #FFFFFF;
  /* summary_row.background.color */
  padding: 6px;
  /* summary_row.padding */
  text-transform: inherit;
  /* summary_row.text_transform */
}

#lwnvagzrgq .gt_first_summary_row {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
}

#lwnvagzrgq .gt_table_body {
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

#lwnvagzrgq .gt_footnote {
  font-size: 90%;
  /* footnote.font.size */
  padding: 4px;
  /* footnote.padding */
}

#lwnvagzrgq .gt_sourcenote {
  font-size: 90%;
  /* sourcenote.font.size */
  padding: 4px;
  /* sourcenote.padding */
}

#lwnvagzrgq .gt_center {
  text-align: center;
}

#lwnvagzrgq .gt_left {
  text-align: left;
}

#lwnvagzrgq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lwnvagzrgq .gt_font_normal {
  font-weight: normal;
}

#lwnvagzrgq .gt_font_bold {
  font-weight: bold;
}

#lwnvagzrgq .gt_font_italic {
  font-style: italic;
}

#lwnvagzrgq .gt_super {
  font-size: 65%;
}

#lwnvagzrgq .gt_footnote_glyph {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="lwnvagzrgq" style="overflow-x:auto;">

<!--gt table start-->

<table class="gt_table">

<tr>

<th class="gt_col_heading gt_left" rowspan="1" colspan="1">

<strong>N = 32</strong>

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

<strong>Coefficient</strong>

</th>

<th class="gt_col_heading gt_center" rowspan="1" colspan="1">

<strong>Confidence Interval</strong>

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

</td>

<td class="gt_row gt_center">

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

</table>

<!--gt table end-->

</div>

<!--/html_preserve-->
