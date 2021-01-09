
<!-- README.md is generated from README.Rmd. Please edit that file -->

# broom.helpers <img src="man/figures/broom.helpers.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R build
status](https://github.com/larmarange/broom.helpers/workflows/R-CMD-check/badge.svg)](https://github.com/larmarange/broom.helpers/actions)
[![Codecov test
coverage](https://codecov.io/gh/larmarange/broom.helpers/branch/master/graph/badge.svg)](https://codecov.io/gh/larmarange/broom.helpers?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/broom.helpers)](https://CRAN.R-project.org/package=broom.helpers)
[![DOI](https://zenodo.org/badge/286680847.svg)](https://zenodo.org/badge/latestdoi/286680847)
<!-- badges: end -->

The broom.helpers package provides suite of functions to work with
regression model `broom::tidy()` tibbles. The suite includes functions
to group regression model terms by variable, insert reference and header
rows for categorical variables, add variable labels, and more.

## Installation

To install stable version:

``` r
install.packages("broom.helpers")
```

To install development version:

``` r
devtools::install_github("larmarange/broom.helpers")
```

## Examples

### all-in-one wrapper

``` r
mod1 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
library(broom.helpers)
ex1 <- mod1 %>% tidy_plus_plus()
ex1
#> # A tibble: 4 x 16
#>   term  variable var_label var_class var_type var_nlevels contrasts
#>   <chr> <chr>    <chr>     <chr>     <chr>          <int> <chr>    
#> 1 Sepa~ Sepal.W~ Sepal.Wi~ numeric   continu~          NA <NA>     
#> 2 Spec~ Species  Species   factor    categor~           3 contr.tr~
#> 3 Spec~ Species  Species   factor    categor~           3 contr.tr~
#> 4 Spec~ Species  Species   factor    categor~           3 contr.tr~
#> # ... with 9 more variables: contrasts_type <chr>, reference_row <lgl>,
#> #   label <chr>, estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>
dplyr::glimpse(ex1)
#> Rows: 4
#> Columns: 16
#> $ term           <chr> "Sepal.Width", "Speciessetosa", "Speciesversicolor",...
#> $ variable       <chr> "Sepal.Width", "Species", "Species", "Species"
#> $ var_label      <chr> "Sepal.Width", "Species", "Species", "Species"
#> $ var_class      <chr> "numeric", "factor", "factor", "factor"
#> $ var_type       <chr> "continuous", "categorical", "categorical", "categor...
#> $ var_nlevels    <int> NA, 3, 3, 3
#> $ contrasts      <chr> NA, "contr.treatment", "contr.treatment", "contr.tre...
#> $ contrasts_type <chr> NA, "treatment", "treatment", "treatment"
#> $ reference_row  <lgl> NA, TRUE, FALSE, FALSE
#> $ label          <chr> "Sepal.Width", "setosa", "versicolor", "virginica"
#> $ estimate       <dbl> 0.8035609, 0.0000000, 1.4587431, 1.9468166
#> $ std.error      <dbl> 0.1063390, NA, 0.1121079, 0.1000150
#> $ statistic      <dbl> 7.556598, NA, 13.011954, 19.465255
#> $ p.value        <dbl> 4.187340e-12, NA, 3.478232e-26, 2.094475e-42
#> $ conf.low       <dbl> 0.5933983, NA, 1.2371791, 1.7491525
#> $ conf.high      <dbl> 1.013723, NA, 1.680307, 2.144481

mod2 <- glm(
  response ~ poly(age, 3) + stage + grade * trt,
  na.omit(gtsummary::trial),
  family = binomial,
  contrasts = list(
    stage = contr.treatment(4, base = 3),
    grade = contr.sum
  )
)
ex2 <- mod2 %>% 
  tidy_plus_plus(
    exponentiate = TRUE,
    variable_labels = c(age = "Age (in years)"),
    add_header_rows = TRUE,
    show_single_row = "trt"
  )
ex2
#> # A tibble: 17 x 17
#>    term  variable var_label var_class var_type var_nlevels header_row contrasts
#>    <chr> <chr>    <chr>     <chr>     <chr>          <int> <lgl>      <chr>    
#>  1 <NA>  age      Age (in ~ nmatrix.3 continu~          NA TRUE       <NA>     
#>  2 poly~ age      Age (in ~ nmatrix.3 continu~          NA FALSE      <NA>     
#>  3 poly~ age      Age (in ~ nmatrix.3 continu~          NA FALSE      <NA>     
#>  4 poly~ age      Age (in ~ nmatrix.3 continu~          NA FALSE      <NA>     
#>  5 <NA>  stage    T Stage   factor    categor~           4 TRUE       contr.tr~
#>  6 stag~ stage    T Stage   factor    categor~           4 FALSE      contr.tr~
#>  7 stag~ stage    T Stage   factor    categor~           4 FALSE      contr.tr~
#>  8 stag~ stage    T Stage   factor    categor~           4 FALSE      contr.tr~
#>  9 stag~ stage    T Stage   factor    categor~           4 FALSE      contr.tr~
#> 10 <NA>  grade    Grade     factor    categor~           3 TRUE       contr.sum
#> 11 grad~ grade    Grade     factor    categor~           3 FALSE      contr.sum
#> 12 grad~ grade    Grade     factor    categor~           3 FALSE      contr.sum
#> 13 grad~ grade    Grade     factor    categor~           3 FALSE      contr.sum
#> 14 trtD~ trt      Chemothe~ character dichoto~           2 NA         contr.tr~
#> 15 <NA>  grade:t~ Grade * ~ <NA>      interac~          NA TRUE       <NA>     
#> 16 grad~ grade:t~ Grade * ~ <NA>      interac~          NA FALSE      <NA>     
#> 17 grad~ grade:t~ Grade * ~ <NA>      interac~          NA FALSE      <NA>     
#> # ... with 9 more variables: contrasts_type <chr>, reference_row <lgl>,
#> #   label <chr>, estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>
dplyr::glimpse(ex2)
#> Rows: 17
#> Columns: 17
#> $ term           <chr> NA, "poly(age, 3)1", "poly(age, 3)2", "poly(age, 3)3...
#> $ variable       <chr> "age", "age", "age", "age", "stage", "stage", "stage...
#> $ var_label      <chr> "Age (in years)", "Age (in years)", "Age (in years)"...
#> $ var_class      <chr> "nmatrix.3", "nmatrix.3", "nmatrix.3", "nmatrix.3", ...
#> $ var_type       <chr> "continuous", "continuous", "continuous", "continuou...
#> $ var_nlevels    <int> NA, NA, NA, NA, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, NA, NA...
#> $ header_row     <lgl> TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE...
#> $ contrasts      <chr> NA, NA, NA, NA, "contr.treatment(base=3)", "contr.tr...
#> $ contrasts_type <chr> NA, NA, NA, NA, "treatment", "treatment", "treatment...
#> $ reference_row  <lgl> NA, NA, NA, NA, NA, FALSE, FALSE, TRUE, FALSE, NA, F...
#> $ label          <chr> "Age (in years)", "Age (in years)", "Age (in years)²...
#> $ estimate       <dbl> NA, 20.2416394, 1.2337899, 0.4931553, NA, 1.0047885,...
#> $ std.error      <dbl> NA, 2.3254455, 2.3512842, 2.3936657, NA, 0.4959893, ...
#> $ statistic      <dbl> NA, 1.29340459, 0.08935144, -0.29533409, NA, 0.00963...
#> $ p.value        <dbl> NA, 0.1958712, 0.9288026, 0.7677387, NA, 0.9923154, ...
#> $ conf.low       <dbl> NA, 0.225454425, 0.007493208, 0.004745694, NA, 0.379...
#> $ conf.high      <dbl> NA, 2315.587655, 100.318341, 74.226179, NA, 2.683385...
```

### fine control

``` r
ex3 <- mod1 %>%
  # perform initial tidying of model
  tidy_and_attach() %>%
  # add reference row
  tidy_add_reference_rows() %>%
  # add term labels
  tidy_add_term_labels() %>%
  # remove intercept
  tidy_remove_intercept
ex3
#> # A tibble: 4 x 14
#>   term  variable var_label var_class var_type var_nlevels contrasts
#>   <chr> <chr>    <chr>     <chr>     <chr>          <int> <chr>    
#> 1 Sepa~ Sepal.W~ Sepal.Wi~ numeric   continu~          NA <NA>     
#> 2 Spec~ Species  Species   factor    categor~           3 contr.tr~
#> 3 Spec~ Species  Species   factor    categor~           3 contr.tr~
#> 4 Spec~ Species  Species   factor    categor~           3 contr.tr~
#> # ... with 7 more variables: contrasts_type <chr>, reference_row <lgl>,
#> #   label <chr>, estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>
dplyr::glimpse(ex3)
#> Rows: 4
#> Columns: 14
#> $ term           <chr> "Sepal.Width", "Speciessetosa", "Speciesversicolor",...
#> $ variable       <chr> "Sepal.Width", "Species", "Species", "Species"
#> $ var_label      <chr> "Sepal.Width", "Species", "Species", "Species"
#> $ var_class      <chr> "numeric", "factor", "factor", "factor"
#> $ var_type       <chr> "continuous", "categorical", "categorical", "categor...
#> $ var_nlevels    <int> NA, 3, 3, 3
#> $ contrasts      <chr> NA, "contr.treatment", "contr.treatment", "contr.tre...
#> $ contrasts_type <chr> NA, "treatment", "treatment", "treatment"
#> $ reference_row  <lgl> NA, TRUE, FALSE, FALSE
#> $ label          <chr> "Sepal.Width", "setosa", "versicolor", "virginica"
#> $ estimate       <dbl> 0.8035609, NA, 1.4587431, 1.9468166
#> $ std.error      <dbl> 0.1063390, NA, 0.1121079, 0.1000150
#> $ statistic      <dbl> 7.556598, NA, 13.011954, 19.465255
#> $ p.value        <dbl> 4.187340e-12, NA, 3.478232e-26, 2.094475e-42

ex4 <- mod2 %>%
  # perform initial tidying of model
  tidy_and_attach(exponentiate = TRUE) %>%
  # add variable labels, including a custom value for age
  tidy_add_variable_labels(labels = c(age = "Age in years")) %>%
  # add reference rows for categorical variables
  tidy_add_reference_rows() %>%
  # add a, estimate value of reference terms
  tidy_add_estimate_to_reference_rows(exponentiate = TRUE) %>%
  # add header rows for categorical variables
  tidy_add_header_rows()
ex4
#> # A tibble: 20 x 15
#>    term  variable var_label var_class var_type var_nlevels header_row contrasts
#>    <chr> <chr>    <chr>     <chr>     <chr>          <int> <lgl>      <chr>    
#>  1 (Int~ (Interc~ (Interce~ <NA>      interce~          NA NA         <NA>     
#>  2 <NA>  age      Age in y~ nmatrix.3 continu~          NA TRUE       <NA>     
#>  3 poly~ age      Age in y~ nmatrix.3 continu~          NA FALSE      <NA>     
#>  4 poly~ age      Age in y~ nmatrix.3 continu~          NA FALSE      <NA>     
#>  5 poly~ age      Age in y~ nmatrix.3 continu~          NA FALSE      <NA>     
#>  6 <NA>  stage    T Stage   factor    categor~           4 TRUE       contr.tr~
#>  7 stag~ stage    T Stage   factor    categor~           4 FALSE      contr.tr~
#>  8 stag~ stage    T Stage   factor    categor~           4 FALSE      contr.tr~
#>  9 stag~ stage    T Stage   factor    categor~           4 FALSE      contr.tr~
#> 10 stag~ stage    T Stage   factor    categor~           4 FALSE      contr.tr~
#> 11 <NA>  grade    Grade     factor    categor~           3 TRUE       contr.sum
#> 12 grad~ grade    Grade     factor    categor~           3 FALSE      contr.sum
#> 13 grad~ grade    Grade     factor    categor~           3 FALSE      contr.sum
#> 14 grad~ grade    Grade     factor    categor~           3 FALSE      contr.sum
#> 15 <NA>  trt      Chemothe~ character dichoto~           2 TRUE       contr.tr~
#> 16 trtD~ trt      Chemothe~ character dichoto~           2 FALSE      contr.tr~
#> 17 trtD~ trt      Chemothe~ character dichoto~           2 FALSE      contr.tr~
#> 18 <NA>  grade:t~ Grade * ~ <NA>      interac~          NA TRUE       <NA>     
#> 19 grad~ grade:t~ Grade * ~ <NA>      interac~          NA FALSE      <NA>     
#> 20 grad~ grade:t~ Grade * ~ <NA>      interac~          NA FALSE      <NA>     
#> # ... with 7 more variables: contrasts_type <chr>, reference_row <lgl>,
#> #   label <chr>, estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>
dplyr::glimpse(ex4)
#> Rows: 20
#> Columns: 15
#> $ term           <chr> "(Intercept)", NA, "poly(age, 3)1", "poly(age, 3)2",...
#> $ variable       <chr> "(Intercept)", "age", "age", "age", "age", "stage", ...
#> $ var_label      <chr> "(Intercept)", "Age in years", "Age in years", "Age ...
#> $ var_class      <chr> NA, "nmatrix.3", "nmatrix.3", "nmatrix.3", "nmatrix....
#> $ var_type       <chr> "intercept", "continuous", "continuous", "continuous...
#> $ var_nlevels    <int> NA, NA, NA, NA, NA, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2,...
#> $ header_row     <lgl> NA, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, F...
#> $ contrasts      <chr> NA, NA, NA, NA, NA, "contr.treatment(base=3)", "cont...
#> $ contrasts_type <chr> NA, NA, NA, NA, NA, "treatment", "treatment", "treat...
#> $ reference_row  <lgl> NA, NA, NA, NA, NA, NA, FALSE, FALSE, TRUE, FALSE, N...
#> $ label          <chr> "(Intercept)", "Age in years", "Age in years", "Age ...
#> $ estimate       <dbl> 0.5266376, NA, 20.2416394, 1.2337899, 0.4931553, NA,...
#> $ std.error      <dbl> 0.4130930, NA, 2.3254455, 2.3512842, 2.3936657, NA, ...
#> $ statistic      <dbl> -1.55229592, NA, 1.29340459, 0.08935144, -0.29533409...
#> $ p.value        <dbl> 0.1205914, NA, 0.1958712, 0.9288026, 0.7677387, NA, ...
```
