# Stack tables

Assists in patching together more complex tables. `tbl_stack()` appends
two or more gtsummary tables.

## Usage

``` r
tbl_stack(
  tbls,
  group_header = NULL,
  quiet = FALSE,
  attr_order = seq_along(tbls),
  tbl_ids = NULL,
  tbl_id_lbls = NULL
)
```

## Arguments

- tbls:

  (`list`)  
  List of gtsummary objects

- group_header:

  (`character`)  
  Character vector with table headers where length matches the length of
  `tbls`

- quiet:

  (scalar `logical`)  
  Logical indicating whether to suppress additional messaging. Default
  is `FALSE`.

- attr_order:

  (`integer`)  
  Set the order table attributes are set. Tables are stacked in the
  order they are passed in the `tbls` argument: use `attr_order` to
  specify the order the table attributes take precedent. For example, to
  use the header from the second table specify `attr_order=2`. Default
  is to set precedent in the order tables are passed.

- tbl_ids:

  (`character`)  
  Optional character vector of IDs that will be assigned to the input
  tables. The ID is assigned by assigning a name to the `tbls` list,
  which is returned in `x$tbls`.

- tbl_id_lbls:

  (`vector`)  
  Optional vector of the same length `tbls`. When specified a new,
  hidden column is added to the returned `.$table_body` with these
  labels. *The most common use case of this argument is for the
  development of other functions.*

## Value

A `tbl_stack` object

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
# stacking two tbl_regression objects
t1 <-
  glm(response ~ trt, trial, family = binomial) %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(trt ~ "Treatment (unadjusted)")
  )

t2 <-
  glm(response ~ trt + grade + stage + marker, trial, family = binomial) %>%
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list(trt ~ "Treatment (adjusted)")
  )

tbl_stack(list(t1, t2))


  

Characteristic
```

**OR**

**95% CI**

**p-value**

Treatment (unadjusted)

  

  

  

    Drug A

—

—

  

    Drug B

1.21

0.66, 2.24

0.5

Treatment (adjusted)

  

  

  

    Drug A

—

—

  

    Drug B

1.48

0.78, 2.86

0.2

Abbreviations: CI = Confidence Interval, OR = Odds Ratio

\# Example 2 ---------------------------------- \# stacking two
tbl_merge objects
[library](https://rdrr.io/r/base/library.html)([survival](https://github.com/therneau/survival))
t3 \<-
[coxph](https://rdrr.io/pkg/survival/man/coxph.html)([Surv](https://rdrr.io/pkg/survival/man/Surv.html)(ttdeath,
death) ~ trt, trial)
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)(
exponentiate = TRUE, label =
[list](https://rdrr.io/r/base/list.html)(trt ~ "Treatment (unadjusted)")
) t4 \<-
[coxph](https://rdrr.io/pkg/survival/man/coxph.html)([Surv](https://rdrr.io/pkg/survival/man/Surv.html)(ttdeath,
death) ~ trt + grade + stage + marker, trial)
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)(
include = "trt", exponentiate = TRUE, label =
[list](https://rdrr.io/r/base/list.html)(trt ~ "Treatment (adjusted)") )
\# first merging, then stacking row1 \<-
[tbl_merge](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)([list](https://rdrr.io/r/base/list.html)(t1,
t3), tab_spanner = [c](https://rdrr.io/r/base/c.html)("Tumor Response",
"Death")) row2 \<-
[tbl_merge](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)([list](https://rdrr.io/r/base/list.html)(t2,
t4)) tbl_stack([list](https://rdrr.io/r/base/list.html)(row1, row2),
group_header = [c](https://rdrr.io/r/base/c.html)("Unadjusted Analysis",
"Adjusted Analysis"))

[TABLE]
