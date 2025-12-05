# Assign Test

This function is used to assign default tests for
[`add_p()`](https://www.danieldsjoberg.com/gtsummary/reference/add_p.md)
and
[`add_difference()`](https://www.danieldsjoberg.com/gtsummary/reference/add_difference.md).

## Usage

``` r
assign_tests(x, ...)

# S3 method for class 'tbl_summary'
assign_tests(
  x,
  include,
  by = x$inputs$by,
  test = NULL,
  group = NULL,
  adj.vars = NULL,
  summary_type = x$inputs$type,
  calling_fun = c("add_p", "add_difference"),
  ...
)

# S3 method for class 'tbl_svysummary'
assign_tests(
  x,
  include,
  by = x$inputs$by,
  test = NULL,
  group = NULL,
  adj.vars = NULL,
  summary_type = x$inputs$type,
  calling_fun = c("add_p", "add_difference"),
  ...
)

# S3 method for class 'tbl_continuous'
assign_tests(x, include, by, cont_variable, test = NULL, group = NULL, ...)

# S3 method for class 'tbl_survfit'
assign_tests(x, include, test = NULL, ...)
```

## Arguments

- x:

  (`gtsummary`)  
  a table of class `'gtsummary'`

- ...:

  Passed to
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html),
  [`rlang::warn()`](https://rlang.r-lib.org/reference/abort.html) or
  [`rlang::inform()`](https://rlang.r-lib.org/reference/abort.html).

- include:

  (`character`)  
  Character vector of column names to assign a default tests.

- by:

  (`string`)  
  a single stratifying column name

- test:

  (named `list`)  
  a named list of tests.

- group:

  (`string`)  
  a variable name indicating the grouping column for correlated data.
  Default is `NULL`.

- adj.vars:

  (`character`)  
  Variables to include in adjusted calculations (e.g. in ANCOVA models).

- summary_type:

  (named `list`)  
  named list of summary types

- calling_fun:

  (`string`)  
  Must be one of `'add_p'` and `'add_difference'`. Depending on the
  context, different defaults are set.

- cont_variable:

  (`string`)  
  a column name of the continuous summary variable in
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_continuous.md)

## Value

A table of class `'gtsummary'`

## Examples

``` r
trial |>
  tbl_summary(
    by = trt,
    include = c(age, stage)
  ) |>
  assign_tests(include = c("age", "stage"), calling_fun = "add_p")
#> $age
#> function (data, variable, by, test.args, conf.level = 0.95, ...) 
#> {
#>     check_empty(c("group", "adj.vars"), ...)
#>     dplyr::mutate(rlang::inject(cardx::ard_stats_wilcox_test(data = .data_pre_processing(data, 
#>         factor = by, numeric = variable), variables = all_of(variable), 
#>         by = all_of(by), conf.int = TRUE, conf.level = conf.level, 
#>         !!!test.args)), stat = dplyr::case_when(.data$stat_name %in% 
#>         "method" & .data$stat %in% "Wilcoxon rank sum test with continuity correction" ~ 
#>         list("Wilcoxon rank sum test"), .default = .data$stat))
#> }
#> <bytecode: 0x55e937bb0390>
#> <environment: namespace:gtsummary>
#> attr(,"test_name")
#> [1] "wilcox.test"
#> 
#> $stage
#> function (data, variable, by, test.args, ...) 
#> {
#>     add_p_test_chisq.test(data = data, variable = variable, by = by, 
#>         test.args = c(list(correct = FALSE), test.args), ...)
#> }
#> <bytecode: 0x55e934630f28>
#> <environment: namespace:gtsummary>
#> attr(,"test_name")
#> [1] "chisq.test.no.correct"
#> 
```
