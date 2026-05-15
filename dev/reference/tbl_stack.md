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
