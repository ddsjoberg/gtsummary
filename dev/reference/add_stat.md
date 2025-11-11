# Add a custom statistic

The function allows a user to add a new column (or columns) of
statistics to an existing `tbl_summary`, `tbl_svysummary`, or
`tbl_continuous` object.

## Usage

``` r
add_stat(x, fns, location = everything() ~ "label")
```

## Arguments

- x:

  (`tbl_summary`/`tbl_svysummary`/`tbl_continuous`)  
  A gtsummary table of class `'tbl_summary'`, `'tbl_svysummary'`, or
  `'tbl_continuous'`.

- fns:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Indicates the functions that create the statistic. See details below.

- location:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Indicates the location the new statistics are placed. The values must
  be one of `c("label", "level", "missing")`. When `"label"`, a single
  statistic is placed on the variable label row. When `"level"` the
  statistics are placed on the variable level rows. The length of the
  vector of statistics returned from the `fns` function must match the
  dimension of levels. Default is to place the new statistics on the
  label row.

## Value

A 'gtsummary' of the same class as the input

## Details

The returns from custom functions passed in `fns=` are required to
follow a specified format. Each of these function will execute on a
single variable.

1.  Each function must return a tibble or a vector. If a vector is
    returned, it will be converted to a tibble with one column and
    number of rows equal to the length of the vector.

2.  When `location='label'`, the returned statistic from the custom
    function must be a tibble with one row. When `location='level'` the
    tibble must have the same number of rows as there are levels in the
    variable (excluding the row for unknown values).

3.  Each function may take the following arguments:
    `foo(data, variable, by, tbl, ...)`

    - `data=` is the input data frame passed to
      [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)

    - `variable=` is a string indicating the variable to perform the
      calculation on. This is the variable in the label column of the
      table.

    - `by=` is a string indicating the by variable from `tbl_summary=`,
      if present

    - `tbl=` the original
      [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)/[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
      object is also available to utilize

The user-defined function does not need to utilize each of these inputs.
It's encouraged the user-defined function accept `...` as each of the
arguments *will* be passed to the function, even if not all inputs are
utilized by the user's function, e.g. `foo(data, variable, by, ...)`

- Use
  [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
  to update the column headers

- Use
  [`modify_fmt_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_fmt_fun.md)
  to update the functions that format the statistics

- Use
  [`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)
  to add a explanatory footnote

If you return a tibble with column names `p.value` or `q.value`, default
p-value formatting will be applied, and you may take advantage of
subsequent p-value formatting functions, such as
[`bold_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_p.md)
or
[`add_q()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_q.md).

## Examples

``` r
# Example 1 ----------------------------------
# fn returns t-test pvalue
my_ttest <- function(data, variable, by, ...) {
  t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
}

trial |>
  tbl_summary(
    by = trt,
    include = c(trt, age, marker),
    missing = "no"
  ) |>
  add_stat(fns = everything() ~ my_ttest) |>
  modify_header(add_stat_1 = "**p-value**", all_stat_cols() ~ "**{level}**")


  

Characteristic
```

**Drug A**¹

**Drug B**¹

**p-value**

Age

46 (37, 60)

48 (39, 56)

0.834

Marker Level (ng/mL)

0.84 (0.23, 1.60)

0.52 (0.18, 1.21)

0.116

¹ Median (Q1, Q3)

\# Example 2 ---------------------------------- \# fn returns t-test
test statistic and pvalue my_ttest2 \<- function(data, variable, by,
...) {
[t.test](https://rdrr.io/r/stats/t.test.html)(data\[\[variable\]\] ~
[as.factor](https://rdrr.io/r/base/factor.html)(data\[\[by\]\])) \|\>
broom::[tidy](https://generics.r-lib.org/reference/tidy.html)()
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
dplyr::[mutate](https://dplyr.tidyverse.org/reference/mutate.html)( stat
=
glue::[glue](https://glue.tidyverse.org/reference/glue.html)("t={style_sigfig(statistic)},
{style_pvalue(p.value, prepend_p = TRUE)}") )
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
dplyr::[pull](https://dplyr.tidyverse.org/reference/pull.html)(stat) }
trial \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(
by = trt, include = [c](https://rdrr.io/r/base/c.html)(trt, age,
marker), missing = "no" ) \|\> add_stat(fns =
[everything](https://tidyselect.r-lib.org/reference/everything.html)() ~
my_ttest2) \|\>
[modify_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)(add_stat_1
= "\*\*Treatment Comparison\*\*")

[TABLE]

\# Example 3 ---------------------------------- \# return test statistic
and p-value is separate columns my_ttest3 \<- function(data, variable,
by, ...) {
[t.test](https://rdrr.io/r/stats/t.test.html)(data\[\[variable\]\] ~
[as.factor](https://rdrr.io/r/base/factor.html)(data\[\[by\]\]))
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
broom::[tidy](https://generics.r-lib.org/reference/tidy.html)()
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[select](https://dplyr.tidyverse.org/reference/select.html)(statistic,
p.value) } trial \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(
by = trt, include = [c](https://rdrr.io/r/base/c.html)(trt, age,
marker), missing = "no" ) \|\> add_stat(fns =
[everything](https://tidyselect.r-lib.org/reference/everything.html)() ~
my_ttest3) \|\>
[modify_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)(statistic
= "\*\*t-statistic\*\*", p.value = "\*\*p-value\*\*") \|\>
[modify_fmt_fun](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_fmt_fun.md)(statistic
=
[label_style_sigfig](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)(),
p.value =
[label_style_pvalue](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)(digits
= 2))

[TABLE]
