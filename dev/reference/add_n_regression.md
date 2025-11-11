# Add N to regression table

Add N to regression table

## Usage

``` r
# S3 method for class 'tbl_regression'
add_n(x, location = "label", ...)

# S3 method for class 'tbl_uvregression'
add_n(x, location = "label", ...)
```

## Arguments

- x:

  (`tbl_regression`, `tbl_uvregression`)  
  a `tbl_regression` or `tbl_uvregression` table

- location:

  (`character`)  
  location to place Ns. Select one or more of `c('label', 'level')`.
  Default is `'label'`.

  When `"label"` total Ns are placed on each variable's label row. When
  `"level"` level counts are placed on the variable level for
  categorical variables, and total N on the variable's label row for
  continuous.

- ...:

  These dots are for future extensions and must be empty.

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  select(response, age, grade) |>
  tbl_uvregression(
    y = response,
    exponentiate = TRUE,
    method = glm,
    method.args = list(family = binomial),
    hide_n = TRUE
  ) |>
  add_n(location = "label")


  

Characteristic
```

**N**

**OR**

**95% CI**

**p-value**

Age

183

1.02

1.00, 1.04

0.10

Grade

193

  

  

  

    I

  

—

—

  

    II

  

0.95

0.45, 2.00

0.9

    III

  

1.10

0.52, 2.29

0.8

Abbreviations: CI = Confidence Interval, OR = Odds Ratio

\# Example 2 ----------------------------------
[glm](https://rdrr.io/r/stats/glm.html)(response ~ age + grade, trial,
family = binomial) \|\>
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)(exponentiate
= TRUE) \|\>
[add_n](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.md)(location
= "level")

[TABLE]
