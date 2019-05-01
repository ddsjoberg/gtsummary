
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/ddsjoberg/gtsummary.svg?branch=master)](https://travis-ci.org/ddsjoberg/gtsummary)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/gtsummary?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/gtsummary)
[![Coverage
status](https://codecov.io/gh/ddsjoberg/gtsummary/branch/master/graph/badge.svg)](https://codecov.io/github/ddsjoberg/gtsummary?branch=master)
<!-- badges: end -->

## gtsummary

The {gtsummary} package creates presentation-ready tables summarizing
data sets, regression models, and more. The code to create the tables is
concise and highly customizable. The resulting tables are georgeous\!
Data frames can be summarized with any function, e.g. mean(), median(),
or a user-written function. Regression models are summarized and include
the reference row for categorical variables in the model. Common
regression models, such as, logistic regression and Cox proportional
hazards regression, are automatically identified and the tables are
pre-filled with the appropriate column headers (i.e. Odds Ratio, and
Hazard Ratio).

{gtsummary} uses the [{gt}](https://gt.rstudio.com/) package enabling
each table to be customized to user preferences. The package also uses
[{broom}](https://broom.tidyverse.org/) to perform initial tidying of
the regression models, which means many types of model are supported out
of the box. If you label your data (which I recommend\!), the labels
will be used in the table output. With {gtsummary} and labelled data,
you get a perfectly formatted, ready-to-share tables in a single line of
code\! Check out the examples below.

## Installation

You can install the production version of {gtsummary} with:

``` r
install.packages("remotes")
remotes::install_github("ddsjoberg/gtsummary")
```

and the development version with:

``` r
remotes::install_github("ddsjoberg/gtsummary", ref = "dev")
```

## Examples

The vignettes/tutorials for {gtsummary} functions have detailed examples
and can be found at
[danieldsjoberg.com/gtsummary](http://www.danieldsjoberg.com/gtsummary).

### Summary Table

``` r
library(gtsummary)
#> Loading required package: gt
t1 <-
  tbl_summary(
    data = trial[c("trt", "age", "grade", "response")],
    by = "trt"
  ) %>%
  add_comparison() 
```

![](man/figures/README-tbl_summary.png)

### Regression Models

``` r
mod1 <- 
  glm(response ~ trt + age + grade, trial, family = binomial(link = "logit"))
t2 <-
  tbl_regression(mod1,
                 exponentiate = TRUE)
```

![](man/figures/README-tbl_regression.png)

Please note that the {gtsummary} project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms. A big thank you to all
contributors\!  
<!-- UPDATE THIS LIST! usethis::use_tidy_thanks() -->
[@ddsjoberg](https://github.com/ddsjoberg),
[@karissawhiting](https://github.com/karissawhiting), and
[@michaelcurry1123](https://github.com/michaelcurry1123)
