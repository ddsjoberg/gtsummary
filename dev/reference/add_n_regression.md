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
