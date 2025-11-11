# Filter Hierarchical Tables

**\[experimental\]**  

This function is used to filter hierarchical table rows. Filters are not
applied to summary or overall rows.

## Usage

``` r
filter_hierarchical(x, ...)

# S3 method for class 'tbl_hierarchical'
filter_hierarchical(
  x,
  filter,
  var = NULL,
  keep_empty = FALSE,
  quiet = FALSE,
  ...
)

# S3 method for class 'tbl_hierarchical_count'
filter_hierarchical(
  x,
  filter,
  var = NULL,
  keep_empty = FALSE,
  quiet = FALSE,
  ...
)

# S3 method for class 'tbl_ard_hierarchical'
filter_hierarchical(
  x,
  filter,
  var = NULL,
  keep_empty = FALSE,
  quiet = FALSE,
  ...
)
```

## Arguments

- x:

  (`tbl_hierarchical`, `tbl_hierarchical_count`,
  `tbl_ard_hierarchical`)  
  A hierarchical gtsummary table of class `'tbl_hierarchical'`,
  `'tbl_hierarchical_count'`, or `'tbl_ard_hierarchical'`.

- ...:

  These dots are for future extensions and must be empty.

- filter:

  (`expression`)  
  An expression that is used to filter rows of the table. See the
  Details section below.

- var:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Hierarchy variable from `x` to perform filtering on. The variable must
  be present in `x$inputs$include`. If `NULL`, the last hierarchy
  variable from `x` (`dplyr::last(x$inputs$include)`) will be used.

- keep_empty:

  (scalar `logical`)  
  Logical argument indicating whether to retain summary rows
  corresponding to table hierarchy sections that have had all rows
  filtered out. Default is `FALSE`.

- quiet:

  (`logical`)  
  Logical indicating whether to suppress any messaging. Default is
  `FALSE`.

## Value

a gtsummary table of the same class as `x`.

## Details

The `filter` argument can be used to filter out rows of a table which do
not meet the criteria provided as an expression. Rows can be filtered on
the values of any of the possible statistics (`n`, `p`, and `N`)
provided they are included at least once in the table, as well as the
values of any `by` variables.

Additionally, filters can be applied on individual column values (if a
`by` variable was specified) via the `n_XX`, `N_XX`, and `p_XX`
statistics, where each `XX` represents the index of the column to select
the statistic from. For example, `filter = n_1 > 5` will check whether
`n` values in the first column of the table are greater than 5 in each
row.

Overall statistics for each row can be used in filters via the
`n_overall`, `N_overall`, and `p_overall` statistics. If used in
`filter`, overall statistics are derived within the filtering function.
`n_overall` can only be derived if `n` statistic is present in the table
for the filter variable, `N_overall` if the `N` statistic is present for
the filter variable, and `p_overall` if both the `n` and `N` statistics
are present for the filter variable.

By default, filters will be applied at the level of the innermost
hierarchy variable, i.e. the last variable supplied to `variables`. If
filters should instead be applied at the level of one of the outer
hierarchy variables, the `var` parameter can be used to specify a
different variable to filter on. When `var` is set to a different
(outer) variable and a level of the variable does not meet the filtering
criteria then the section corresponding to that variable level -
including summary rows - and all sub-sections within that section will
be removed.

If an overall column was added to the table (via `add_overall())`) this
column will not be used in any filters (i.e. `n_overall` will not
include the overall `n` in a given row).

Some examples of possible filters:

- `filter = n > 5`: keep rows where one of the treatment groups observed
  more than 5 AEs

- `filter = n == 2 & p < 0.05`: keep rows where one of the treatment
  groups observed exactly 2 AEs and one of the treatment groups observed
  a proportion less than 5%.

- `filter = n_overall >= 4`: keep rows where there were 4 or more AEs
  observed across the row

- `filter = mean(n) > 4 | n > 3`: keep rows where the mean number of AEs
  is 4 or more across the row or one of the treatment groups observed
  more than 3 AEs

- `filter = n_2 > 2`: keep rows where the `"Xanomeline High Dose"`
  treatment group observed more than 2 AEs

## See also

[`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_hierarchical.md)

## Examples

``` r
ADAE_subset <- cards::ADAE |>
  dplyr::filter(AEBODSYS %in% c("SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                                "EAR AND LABYRINTH DISORDERS")) |>
  dplyr::filter(.by = AEBODSYS, dplyr::row_number() < 20)

tbl <-
  tbl_hierarchical(
    data = ADAE_subset,
    variables = c(AEBODSYS, AEDECOD),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
    overall_row = TRUE
  )

# Example 1 ----------------------------------
# Keep rows where less than 2 AEs are observed across the row
filter_hierarchical(tbl, sum(n) < 2)


  

Body System or Organ Class

    Dictionary-Derived Term
```

**Placebo**  
N = 86¹

**Xanomeline High Dose**  
N = 84¹

**Xanomeline Low Dose**  
N = 84¹

Number of patients with event

3 (3.5%)

3 (3.6%)

5 (6.0%)

EAR AND LABYRINTH DISORDERS

1 (1.2%)

1 (1.2%)

2 (2.4%)

    CERUMEN IMPACTION

0 (0%)

0 (0%)

1 (1.2%)

    EAR PAIN

1 (1.2%)

0 (0%)

0 (0%)

    TINNITUS

0 (0%)

0 (0%)

1 (1.2%)

SKIN AND SUBCUTANEOUS TISSUE DISORDERS

2 (2.3%)

2 (2.4%)

3 (3.6%)

    ACTINIC KERATOSIS

0 (0%)

1 (1.2%)

0 (0%)

    PRURITUS GENERALISED

0 (0%)

0 (0%)

1 (1.2%)

    RASH

0 (0%)

1 (1.2%)

0 (0%)

¹ n (%)

\# Example 2 ---------------------------------- \# Keep rows where at
least one treatment group in the row has at least 2 AEs observed
filter_hierarchical(tbl, n \>= 2)

[TABLE]

\# Example 3 ---------------------------------- \# Keep rows where AEs
across the row have an overall prevalence of greater than 0.5%
filter_hierarchical(tbl, p_overall \> 0.005)

[TABLE]

\# Example 4 ---------------------------------- \# Keep rows where SOCs
across the row have an overall prevalence of greater than 20
filter_hierarchical(tbl, n_overall \> 20, var = AEBODSYS)

[TABLE]

\# Example 5 ---------------------------------- \# Keep AEs that have a
difference in prevalence of greater than 3% between reference group with
\# \`TRTA = "Xanomeline High Dose"\` and comparison group with \`TRTA =
"Xanomeline Low Dose"\` filter_hierarchical(tbl,
[abs](https://rdrr.io/r/base/MathFun.html)(p_2 - p_3) \> 0.03) \#\> When
applying filters on specific levels of \`by\` variable "TRTA" xx_1 =
\#\> "Placebo", xx_2 = "Xanomeline High Dose", and xx_3 = "Xanomeline
Low Dose".

[TABLE]
