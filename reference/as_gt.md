# Convert gtsummary object to gt

Function converts a gtsummary object to a `"gt_tbl"` object, that is, a
table created with
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html). Function is used
in the background when the results are printed or knit. A user can use
this function if they wish to add customized formatting available via
the [gt package](https://gt.rstudio.com/index.html).

## Usage

``` r
as_gt(x, include = everything(), return_calls = FALSE, ...)
```

## Arguments

- x:

  (`gtsummary`)  
  An object of class `"gtsummary"`

- include:

  Commands to include in output. Input may be a vector of quoted or
  unquoted names. tidyselect and gtsummary select helper functions are
  also accepted. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- return_calls:

  Logical. Default is `FALSE`. If `TRUE`, the calls are returned as a
  list of expressions.

- ...:

  Arguments passed on to `gt::gt(...)`

## Value

A `gt_tbl` object

## Note

As of 2024-08-15, line breaks (e.g. `'\n'`) do not render properly for
PDF output. For now, these line breaks are stripped when rendering to
PDF with Quarto and R markdown.

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(age, grade, response)) |>
  as_gt()


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

Age

46 (37, 60)

48 (39, 56)

    Unknown

7

4

Grade

  

  

    I

35 (36%)

33 (32%)

    II

32 (33%)

36 (35%)

    III

31 (32%)

33 (32%)

Tumor Response

28 (29%)

33 (34%)

    Unknown

3

4

¹ Median (Q1, Q3); n (%)
