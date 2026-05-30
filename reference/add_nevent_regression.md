# Add event N

Add event N

## Usage

``` r
add_nevent(x, ...)

# S3 method for class 'tbl_regression'
add_nevent(x, location = "label", ...)

# S3 method for class 'tbl_uvregression'
add_nevent(x, location = "label", ...)
```

## Arguments

- x:

  (`tbl_regression`, `tbl_uvregression`)  
  a `tbl_regression` or `tbl_uvregression` table

- ...:

  These dots are for future extensions and must be empty.

- location:

  (`character`)  
  location to place Ns. Select one or more of `c('label', 'level')`.
  Default is `'label'`.

  When `"label"` total Ns are placed on each variable's label row. When
  `"level"` level counts are placed on the variable level for
  categorical variables, and total N on the variable's label row for
  continuous.

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  select(response, trt, grade) |>
  tbl_uvregression(
    y = response,
    exponentiate = TRUE,
    method = glm,
    method.args = list(family = binomial),
  ) |>
  add_nevent()


  

Characteristic
```
