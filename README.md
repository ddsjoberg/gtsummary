
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/ddsjoberg/gtsummary.svg?branch=master)](https://travis-ci.org/ddsjoberg/gtsummary) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/gtsummary?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/gtsummary) [![Coverage status](https://codecov.io/gh/ddsjoberg/gtsummary/branch/master/graph/badge.svg)](https://codecov.io/github/ddsjoberg/gtsummary?branch=master)

gtsummary
---------

A collection of functions commonly used in the work of the biostatisticians. The goal of gtsummary is to make reporting of tabular analytic results simple, beautiful, and reproducible.
<!-- Update the list of contributors from the git shell `git shortlog -s -n` -->

Installation
------------

You can install the production version of gtsummary with:

``` r
install.packages("remotes")
remotes::install_github("ddsjoberg/gtsummary")
```

and the development version with:

``` r
remotes::install_github("ddsjoberg/gtsummary", ref = "dev")
```

Examples
--------

The vignettes/tutorials for the primary gtsummary functions have detailed examples and can be found at [danieldsjoberg.com/gtsummary](http://www.danieldsjoberg.com/gtsummary). Each vignette is an Rmarkdown file (\*.Rmd) and a copy of the files can be found here: <https://github.com/ddsjoberg/gtsummary/tree/master/vignettes>.

### Summary Table

``` r
library(gtsummary)
#> Loading required package: gt
tbl_summary(trial, by = "trt") %>% 
  add_comparison() 
```

<!--html_preserve-->
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', 'Helvetica Neue', Arial, sans-serif;
}

#wofeugddal .gt_table {
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

#wofeugddal .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
}

#wofeugddal .gt_title {
  color: #000000;
  font-size: 125%;
  /* heading.title.font.size */
  padding-top: 4px;
  /* heading.top.padding */
  padding-bottom: 1px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wofeugddal .gt_subtitle {
  color: #000000;
  font-size: 85%;
  /* heading.subtitle.font.size */
  padding-top: 1px;
  padding-bottom: 4px;
  /* heading.bottom.padding */
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wofeugddal .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* heading.border.bottom.color */
}

#wofeugddal .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  padding-top: 4px;
  padding-bottom: 4px;
}

#wofeugddal .gt_col_heading {
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

#wofeugddal .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#wofeugddal .gt_group_heading {
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

#wofeugddal .gt_empty_group_heading {
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

#wofeugddal .gt_striped tr:nth-child(even) {
  background-color: #f2f2f2;
}

#wofeugddal .gt_row {
  padding: 10px;
  /* row.padding */
  margin: 10px;
  vertical-align: middle;
}

#wofeugddal .gt_stub {
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #A8A8A8;
  padding-left: 12px;
}

#wofeugddal .gt_stub.gt_row {
  background-color: #FFFFFF;
}

#wofeugddal .gt_summary_row {
  background-color: #FFFFFF;
  /* summary_row.background.color */
  padding: 6px;
  /* summary_row.padding */
  text-transform: inherit;
  /* summary_row.text_transform */
}

#wofeugddal .gt_first_summary_row {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
}

#wofeugddal .gt_table_body {
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

#wofeugddal .gt_footnote {
  font-size: 90%;
  /* footnote.font.size */
  padding: 4px;
  /* footnote.padding */
}

#wofeugddal .gt_sourcenote {
  font-size: 90%;
  /* sourcenote.font.size */
  padding: 4px;
  /* sourcenote.padding */
}

#wofeugddal .gt_center {
  text-align: center;
}

#wofeugddal .gt_left {
  text-align: left;
}

#wofeugddal .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wofeugddal .gt_font_normal {
  font-weight: normal;
}

#wofeugddal .gt_font_bold {
  font-weight: bold;
}

#wofeugddal .gt_font_italic {
  font-style: italic;
}

#wofeugddal .gt_super {
  font-size: 65%;
}

#wofeugddal .gt_footnote_glyph {
  font-style: italic;
  font-size: 65%;
}
</style>
<!--gt table start-->
<table class="gt_table">
<tr>
<th class="gt_col_heading gt_left" rowspan="1" colspan="1">
<strong>Characteristic</strong>
</th>
<th class="gt_col_heading gt_center" rowspan="1" colspan="1">
<strong>Drug</strong>, N = 107
</th>
<th class="gt_col_heading gt_center" rowspan="1" colspan="1">
<strong>Placebo</strong>, N = 93
</th>
<th class="gt_col_heading gt_center" rowspan="1" colspan="1">
<strong>p-value</strong>
</th>
</tr>
<tbody class="gt_table_body gt_striped">
<tr>
<td class="gt_row gt_left">
Age, yrs
</td>
<td class="gt_row gt_center">
47 (39, 58)
</td>
<td class="gt_row gt_center">
46 (36, 54)
</td>
<td class="gt_row gt_center">
0.3
</td>
</tr>
<tr>
<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">
Unknown
</td>
<td class="gt_row gt_center">
3
</td>
<td class="gt_row gt_center">
5
</td>
<td class="gt_row gt_center">
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
0.72 (0.22, 1.63)
</td>
<td class="gt_row gt_center">
0.4
</td>
</tr>
<tr>
<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">
Unknown
</td>
<td class="gt_row gt_center">
4
</td>
<td class="gt_row gt_center">
4
</td>
<td class="gt_row gt_center">
</td>
</tr>
<tr>
<td class="gt_row gt_left">
T Stage
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
<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">
T1
</td>
<td class="gt_row gt_center">
25 (23%)
</td>
<td class="gt_row gt_center">
26 (28%)
</td>
<td class="gt_row gt_center">
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
23 (25%)
</td>
<td class="gt_row gt_center">
</td>
</tr>
<tr>
<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">
T3
</td>
<td class="gt_row gt_center">
29 (27%)
</td>
<td class="gt_row gt_center">
13 (14%)
</td>
<td class="gt_row gt_center">
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
<td class="gt_row gt_left">
Grade
</td>
<td class="gt_row gt_center">
</td>
<td class="gt_row gt_center">
</td>
<td class="gt_row gt_center">
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
29 (31%)
</td>
<td class="gt_row gt_center">
</td>
</tr>
<tr>
<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">
II
</td>
<td class="gt_row gt_center">
34 (32%)
</td>
<td class="gt_row gt_center">
24 (26%)
</td>
<td class="gt_row gt_center">
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
<td class="gt_row gt_left">
Tumor Response
</td>
<td class="gt_row gt_center">
52 (51%)
</td>
<td class="gt_row gt_center">
30 (33%)
</td>
<td class="gt_row gt_center">
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
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', 'Helvetica Neue', Arial, sans-serif;
}

#qhiliqnijl .gt_table {
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

#qhiliqnijl .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
}

#qhiliqnijl .gt_title {
  color: #000000;
  font-size: 125%;
  /* heading.title.font.size */
  padding-top: 4px;
  /* heading.top.padding */
  padding-bottom: 1px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qhiliqnijl .gt_subtitle {
  color: #000000;
  font-size: 85%;
  /* heading.subtitle.font.size */
  padding-top: 1px;
  padding-bottom: 4px;
  /* heading.bottom.padding */
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qhiliqnijl .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* heading.border.bottom.color */
}

#qhiliqnijl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  padding-top: 4px;
  padding-bottom: 4px;
}

#qhiliqnijl .gt_col_heading {
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

#qhiliqnijl .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#qhiliqnijl .gt_group_heading {
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

#qhiliqnijl .gt_empty_group_heading {
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

#qhiliqnijl .gt_striped tr:nth-child(even) {
  background-color: #f2f2f2;
}

#qhiliqnijl .gt_row {
  padding: 10px;
  /* row.padding */
  margin: 10px;
  vertical-align: middle;
}

#qhiliqnijl .gt_stub {
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #A8A8A8;
  padding-left: 12px;
}

#qhiliqnijl .gt_stub.gt_row {
  background-color: #FFFFFF;
}

#qhiliqnijl .gt_summary_row {
  background-color: #FFFFFF;
  /* summary_row.background.color */
  padding: 6px;
  /* summary_row.padding */
  text-transform: inherit;
  /* summary_row.text_transform */
}

#qhiliqnijl .gt_first_summary_row {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
}

#qhiliqnijl .gt_table_body {
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

#qhiliqnijl .gt_footnote {
  font-size: 90%;
  /* footnote.font.size */
  padding: 4px;
  /* footnote.padding */
}

#qhiliqnijl .gt_sourcenote {
  font-size: 90%;
  /* sourcenote.font.size */
  padding: 4px;
  /* sourcenote.padding */
}

#qhiliqnijl .gt_center {
  text-align: center;
}

#qhiliqnijl .gt_left {
  text-align: left;
}

#qhiliqnijl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qhiliqnijl .gt_font_normal {
  font-weight: normal;
}

#qhiliqnijl .gt_font_bold {
  font-weight: bold;
}

#qhiliqnijl .gt_font_italic {
  font-style: italic;
}

#qhiliqnijl .gt_super {
  font-size: 65%;
}

#qhiliqnijl .gt_footnote_glyph {
  font-style: italic;
  font-size: 65%;
}
</style>
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
<tbody class="gt_table_body gt_striped">
<tr>
<td class="gt_row gt_left">
Miles per Gallon
</td>
<td class="gt_row gt_center">
1.4
</td>
<td class="gt_row gt_center">
1.0, 2.4
</td>
<td class="gt_row gt_center">
0.080
</td>
</tr>
<tr>
<td class="gt_row gt_left">
No. of Cylinders
</td>
<td class="gt_row gt_center">
</td>
<td class="gt_row gt_center">
</td>
<td class="gt_row gt_center">
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
<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">
6
</td>
<td class="gt_row gt_center">
2.1
</td>
<td class="gt_row gt_center">
0.13, 39
</td>
<td class="gt_row gt_center">
0.6
</td>
</tr>
<tr>
<td class="gt_row gt_left" style="text-align:left;text-indent:10px;">
8
</td>
<td class="gt_row gt_center">
2.0
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

<!--/html_preserve-->
