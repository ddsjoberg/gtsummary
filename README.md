
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/ddsjoberg/clintable.svg?branch=master)](https://travis-ci.org/ddsjoberg/clintable)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/clintable?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/clintable)
[![Coverage
status](https://codecov.io/gh/ddsjoberg/clintable/branch/master/graph/badge.svg)](https://codecov.io/github/ddsjoberg/clintable?branch=master)  
A collection of functions commonly used in the work of the
biostatisticians. The goal of **clintable** is to make reporting of
tabular analytic results simple, beautiful, and
reproducible.  
<!-- Update the list of contributors from the git shell `git shortlog -s -n` -->

## Installation

You can install the production version of **clintable** with:

``` r
install.packages("remotes")
remotes::install_url("https://github.com/ddsjoberg/clintable/archive/master.zip")
```

and the development version with:

``` r
install.packages("remotes")
remotes::install_url("https://github.com/ddsjoberg/clintable/archive/dev.zip")
```

## Examples

The vignettes/tutorials for the primary **clintable** functions have
detailed examples and can be found at
[danieldsjoberg.com/clintable](http://www.danieldsjoberg.com/clintable).
Each vignette is an Rmarkdown file (\*.Rmd) and a copy of the files can
be found here:
<https://github.com/ddsjoberg/clintable/tree/master/vignettes>.

### Table 1

``` r
library(clintable)
fmt_table1(trial, by = "trt") %>% 
  add_comparison() %>% 
  bold_labels()
```

| Variable                | Drug              | Placebo           | p-value |
| :---------------------- | :---------------- | :---------------- | :------ |
|                         | N = 107           | N = 93            |         |
| **Age, yrs**            | 47 (39, 58)       | 46 (36, 54)       | 0.3     |
| Unknown                 | 3                 | 5                 |         |
| **Marker Level, ng/mL** | 0.61 (0.22, 1.20) | 0.72 (0.22, 1.63) | 0.4     |
| Unknown                 | 4                 | 4                 |         |
| **T Stage**             |                   |                   | 0.13    |
| T1                      | 25 (23%)          | 26 (28%)          |         |
| T2                      | 26 (24%)          | 23 (25%)          |         |
| T3                      | 29 (27%)          | 13 (14%)          |         |
| T4                      | 27 (25%)          | 31 (33%)          |         |
| **Grade**               |                   |                   | 0.3     |
| I                       | 38 (36%)          | 29 (31%)          |         |
| II                      | 34 (32%)          | 24 (26%)          |         |
| III                     | 35 (33%)          | 40 (43%)          |         |
| **Tumor Response**      | 52 (51%)          | 30 (33%)          | 0.017   |
| Unknown                 | 6                 | 3                 |         |

### Regression Models

``` r
mod1 = glm(am ~ mpg + factor(cyl), mtcars, family = binomial(link = "logit"))
fmt_regression(
  mod1, exponentiate = TRUE, 
  label = list(`factor(cyl)` = "No. of Cylinders", mpg = "Miles per Gallon")
  )
```

| N = 32           | OR   | 95% CI     | p-value |
| :--------------- | :--- | :--------- | :------ |
| Miles per Gallon | 1.45 | 1.03, 2.40 | 0.080   |
| No. of Cylinders |      |            |         |
| 4                | Ref. |            |         |
| 6                | 2.08 | 0.13, 39.0 | 0.6     |
| 8                | 2.02 | 0.04, 119  | 0.7     |
