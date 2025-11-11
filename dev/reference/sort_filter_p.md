# Sort/filter by p-values

Sort/filter by p-values

## Usage

``` r
sort_p(x, q = FALSE)

filter_p(x, q = FALSE, t = 0.05)
```

## Arguments

- x:

  (`gtsummary`)  
  An object created using gtsummary functions

- q:

  (scalar `logical`)  
  When `TRUE` will check the q-value column rather than the p-value.
  Default is `FALSE`.

- t:

  (scalar `numeric`)  
  Threshold below which values will be retained. Default is 0.05.

## Author

Karissa Whiting, Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
trial %>%
  select(age, grade, response, trt) %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  filter_p(t = 0.8) %>%
  sort_p()


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**p-value**²

Tumor Response

28 (29%)

33 (34%)

0.5

    Unknown

3

4

  

Age

46 (37, 60)

48 (39, 56)

0.7

    Unknown

7

4

  

¹ Median (Q1, Q3); n (%)

² Wilcoxon rank sum test; Pearson’s Chi-squared test

\# Example 2 ----------------------------------
[glm](https://rdrr.io/r/stats/glm.html)(response ~ trt + grade, trial,
family = [binomial](https://rdrr.io/r/stats/family.html)(link =
"logit")) [%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)(exponentiate
= TRUE) [%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
sort_p()

[TABLE]
