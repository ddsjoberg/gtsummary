# Add p-value

Calculate and add a p-value comparing the two variables in the cross
table. If missing levels are included in the tables, they are also
included in p-value calculation.

## Usage

``` r
# S3 method for class 'tbl_cross'
add_p(
  x,
  test = NULL,
  pvalue_fun = ifelse(source_note, label_style_pvalue(digits = 1, prepend_p = TRUE),
    label_style_pvalue(digits = 1)),
  source_note = FALSE,
  test.args = NULL,
  ...
)
```

## Arguments

- x:

  (`tbl_cross`)  
  Object with class `tbl_cross` created with the
  [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)
  function

- test:

  (`string`)  
  A string specifying statistical test to perform. Default is
  `"chisq.test"` when expected cell counts \>=5 and "`fisher.test`" when
  expected cell counts \<5.

- pvalue_fun:

  (`function`)  
  Function to round and format p-value. Default is
  `label_style_pvalue(digits = 1)`, except when `source_note = TRUE`
  when the default is `label_style_pvalue(digits = 1, prepend_p = TRUE)`

- source_note:

  (scalar `logical`)  
  Logical value indicating whether to show p-value in the {gt} table
  source notes rather than a column.

- test.args:

  (named `list`)  
  Named list containing additional arguments to pass to the test (if it
  accepts additional arguments). For example, add an argument for a
  chi-squared test with `test.args = list(correct = TRUE)`

- ...:

  These dots are for future extensions and must be empty.

## Author

Karissa Whiting, Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_cross(row = stage, col = trt) |>
  add_p()


  

```

Chemotherapy Treatment

Total

p-value¹

Drug A

Drug B

T Stage

  

  

  

0.9

    T1

28

25

53

  

    T2

25

29

54

  

    T3

22

21

43

  

    T4

23

27

50

  

Total

98

102

200

  

¹ Pearson’s Chi-squared test

\# Example 2 ---------------------------------- trial \|\>
[tbl_cross](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)(row
= stage, col = trt) \|\>
[add_p](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)(source_note
= TRUE)

[TABLE]
