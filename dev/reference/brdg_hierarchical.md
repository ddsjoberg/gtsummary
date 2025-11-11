# Hierarchy table bridge

Bridge function for converting
[`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md)
(and similar) cards to basic gtsummary objects. All bridge functions
begin with prefix `brdg_*()`.

This file also contains helper functions for constructing the bridge,
referred to as the piers (supports for a bridge) and begin with
`pier_*()`.

- `brdg_hierarchical()`: The bridge function ingests an ARD data frame
  and returns a gtsummary table that includes `.$table_body` and a basic
  `.$table_styling`. The `.$table_styling$header` data frame includes
  the header statistics. Based on context, this function adds a column
  to the ARD data frame named `"gts_column"`. This column is used during
  the reshaping in the `pier_*()` functions defining column names.

- `pier_*()`: these functions accept a cards tibble and returns a tibble
  that is a piece of the `.$table_body`. Typically these will be stacked
  to construct the final table body data frame. The ARD object passed
  here will have two primary parts: the calculated summary statistics
  and the attributes ARD. The attributes ARD is used for labeling. The
  ARD data frame passed to this function must include a `"gts_column"`
  column, which is added in `brdg_hierarchical()`.

## Usage

``` r
brdg_hierarchical(
  cards,
  variables,
  by,
  include,
  statistic,
  overall_row,
  count,
  is_ordered,
  label
)

pier_summary_hierarchical(cards, variables, include, statistic)
```

## Arguments

- cards:

  (`card`)  
  an ARD object of class `"card"` created with
  `cards::ard_hierarchical_stack()`.

- variables:

  (`character`)  
  character list of hierarchy variables.

- by:

  (`string`)  
  string indicating the stratifying column.

- include:

  (`character`)  
  character list of hierarchy variables to include summary statistics
  for.

- statistic:

  (named `list`)  
  named list of summary statistic names.

- overall_row:

  (scalar `logical`)  
  whether an overall summary row should be included at the top of the
  table. The default is `FALSE`.

- count:

  (scalar `logical`)  
  whether
  [`tbl_hierarchical_count()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md)
  (`TRUE`) or
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md)
  (`FALSE`) is being applied.

- is_ordered:

  (scalar `logical`)  
  whether the last variable in `variables` is ordered.

- label:

  (named `list`)  
  named list of hierarchy variable labels.

## Value

a gtsummary object

## See also

Review [list, formula, and selector
syntax](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md)
used throughout gtsummary
