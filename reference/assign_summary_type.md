# Assign Default Summary Type

Function inspects data and assigns a summary type when not specified in
the `type` argument.

## Usage

``` r
assign_summary_type(data, variables, value, type = NULL, cat_threshold = 10L)
```

## Arguments

- data:

  (`data.frame`)  
  a data frame

- variables:

  (`character`)  
  character vector of column names in `data`

- value:

  (`named list`)  
  named list of values to show for dichotomous variables, where the
  names are the variables

- type:

  (`named list`)  
  named list of summary types, where names are the variables

- cat_threshold:

  (`integer`)  
  for base R numeric classes with fewer levels than this threshold will
  default to a categorical summary. Default is `10L`

## Value

named list

## Examples

``` r
assign_summary_type(
  data = trial,
  variables = c("age", "grade", "response"),
  value = NULL
)
#> $age
#> [1] "continuous"
#> 
#> $grade
#> [1] "categorical"
#> 
#> $response
#> [1] "dichotomous"
#> 
```
