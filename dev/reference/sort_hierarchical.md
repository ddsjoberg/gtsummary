# Sort Hierarchical Tables

**\[experimental\]**  

This function is used to sort hierarchical tables. Options for sorting
criteria are:

1.  Descending - within each section of the hierarchy table, event rate
    sums are calculated for each row and rows are sorted in descending
    order by sum (default).

2.  Alphanumeric - rows are ordered alphanumerically (i.e. A to Z) by
    label text. By default,
    [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md)
    sorts tables in alphanumeric order.

## Usage

``` r
sort_hierarchical(x, ...)

# S3 method for class 'tbl_hierarchical'
sort_hierarchical(x, sort = everything() ~ "descending", ...)

# S3 method for class 'tbl_hierarchical_count'
sort_hierarchical(x, sort = everything() ~ "descending", ...)

# S3 method for class 'tbl_ard_hierarchical'
sort_hierarchical(x, sort = everything() ~ "descending", ...)
```

## Arguments

- x:

  (`tbl_hierarchical`, `tbl_hierarchical_count`,
  `tbl_ard_hierarchical`)  
  a hierarchical gtsummary table of class `'tbl_hierarchical'`,
  `'tbl_hierarchical_count'`, or `'tbl_ard_hierarchical'`.

- ...:

  These dots are for future extensions and must be empty.

- sort:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md),
  `string`)  
  a named list, a list of formulas, a single formula where the list
  element is a named list of functions (or the RHS of a formula), or a
  string specifying the types of sorting to perform at each hierarchy
  level. If the sort method for any variable is not specified then the
  method will default to `"descending"`. If a single unnamed string is
  supplied it is applied to all hierarchy levels. For each variable, the
  value specified must be one of:

  - `"alphanumeric"` - at the specified hierarchy level, groups are
    ordered alphanumerically (i.e. A to Z) by `variable_level` text.

  - `"descending"` - at the specified hierarchy level, count sums are
    calculated for each row and rows are sorted in descending order by
    sum. If `sort` is `"descending"` for a given variable and `n` is
    included in `statistic` for the variable then `n` is used to
    calculate row sums, otherwise `p` is used. If neither `n` nor `p`
    are present in `x` for the variable, an error will occur.

  Defaults to `everything() ~ "descending"`.

## Value

a gtsummary table of the same class as `x`.

## Details

If you do not want to display rates for a hierarchy variable from table
`x` but you would like to sort by descending frequency for that
variable, the recommended method is to keep the variable in `include`
when calling
[`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md)
so that rates for this variable are available, sort the table as needed
using `sort_hierarchical()`, and then finally remove these rates from
the table using
[`modify_table_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_body.md).

For example, to remove rates from the `AESOC` rows in hierarchy table
`x`, you can call:

    x |>
      modify_table_body(
        \(df) mutate(df, dplyr::across(all_stat_cols(), ~ifelse(variable %in% "AESOC", NA, .)))
      )

## Note

When sorting a table that includes an overall column
[`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
must be called to add the overall column *before* `sort_hierarchical()`
is called.

## See also

[`filter_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/filter_hierarchical.md)

## Examples

``` r
theme_gtsummary_compact()
#> Setting theme "Compact"
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
  ) |>
  add_overall()

# Example 1 ----------------------------------------------
# Sort all variables by descending frequency (default)
sort_hierarchical(tbl)


  

Body System or Organ Class

    Dictionary-Derived Term
```

**Overall**  
N = 254¹

**Placebo**  
N = 86¹

**Xanomeline High Dose**  
N = 84¹

**Xanomeline Low Dose**  
N = 84¹

Number of patients with event

11 (4.3%)

3 (3.5%)

3 (3.6%)

5 (6.0%)

SKIN AND SUBCUTANEOUS TISSUE DISORDERS

7 (2.8%)

2 (2.3%)

2 (2.4%)

3 (3.6%)

    ERYTHEMA

4 (1.6%)

1 (1.2%)

0 (0%)

3 (3.6%)

    PRURITUS

3 (1.2%)

1 (1.2%)

0 (0%)

2 (2.4%)

    ACTINIC KERATOSIS

1 (0.4%)

0 (0%)

1 (1.2%)

0 (0%)

    PRURITUS GENERALISED

1 (0.4%)

0 (0%)

0 (0%)

1 (1.2%)

    RASH

1 (0.4%)

0 (0%)

1 (1.2%)

0 (0%)

EAR AND LABYRINTH DISORDERS

4 (1.6%)

1 (1.2%)

1 (1.2%)

2 (2.4%)

    VERTIGO

2 (0.8%)

0 (0%)

1 (1.2%)

1 (1.2%)

    CERUMEN IMPACTION

1 (0.4%)

0 (0%)

0 (0%)

1 (1.2%)

    EAR PAIN

1 (0.4%)

1 (1.2%)

0 (0%)

0 (0%)

    TINNITUS

1 (0.4%)

0 (0%)

0 (0%)

1 (1.2%)

¹ n (%)

\# Example 2 ---------------------------------------------- \# Sort all
variables alphanumerically sort_hierarchical(tbl, sort =
[everything](https://tidyselect.r-lib.org/reference/everything.html)() ~
"alphanumeric")

[TABLE]

\# Example 3 ---------------------------------------------- \# Sort
\`AEBODSYS\` alphanumerically, \`AEDECOD\` by descending frequency
sort_hierarchical(tbl, sort =
[list](https://rdrr.io/r/base/list.html)(AEBODSYS = "alphanumeric",
AEDECOD = "descending"))

[TABLE]

[reset_gtsummary_theme](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)()
