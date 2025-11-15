# Likert Summary

Create a table of ordered categorical variables in a wide format.

## Usage

``` r
tbl_likert(
  data,
  statistic = ~"{n} ({p}%)",
  label = NULL,
  digits = NULL,
  include = everything(),
  sort = c("ascending", "descending")
)
```

## Arguments

- data:

  (`data.frame`)  
  A data frame.

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to specify the summary statistics for each variable. The default
  is `everything() ~ "{n} ({p}%)"`.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- digits:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies how summary statistics are rounded. Values may be either
  integer(s) or function(s). If not specified, default formatting is
  assigned via
  [`assign_summary_digits()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/assign_summary_digits.md).

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- sort:

  (`string`)  
  indicates whether levels of variables should be placed in ascending
  order (the default) or descending.

## Value

a 'tbl_likert' gtsummary table

## Examples

``` r
levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
df_likert <- data.frame(
  recommend_friend = sample(levels, size = 20, replace = TRUE) |> factor(levels = levels),
  regret_purchase = sample(levels, size = 20, replace = TRUE) |> factor(levels = levels)
)

# Example 1 ----------------------------------
tbl_likert_ex1 <-
  df_likert |>
  tbl_likert(include = c(recommend_friend, regret_purchase)) |>
  add_n()
tbl_likert_ex1


  

Characteristic
```

**N**

**Strongly Disagree**

**Disagree**

**Agree**

**Strongly Agree**

recommend_friend

20

4 (20%)

3 (15%)

5 (25%)

8 (40%)

regret_purchase

20

4 (20%)

6 (30%)

5 (25%)

5 (25%)

\# Example 2 ---------------------------------- \# Add continuous
summary of the likert scores [list](https://rdrr.io/r/base/list.html)(
tbl_likert_ex1,
[tbl_wide_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_wide_summary.md)(
df_likert \|\>
dplyr::[mutate](https://dplyr.tidyverse.org/reference/mutate.html)(dplyr::[across](https://dplyr.tidyverse.org/reference/across.html)([everything](https://tidyselect.r-lib.org/reference/everything.html)(),
as.numeric)), statistic = [c](https://rdrr.io/r/base/c.html)("{mean}",
"{sd}"), type = ~"continuous", include =
[c](https://rdrr.io/r/base/c.html)(recommend_friend, regret_purchase) )
) \|\>
[tbl_merge](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)(tab_spanner
= FALSE)

| **Characteristic** | **N** | **Strongly Disagree** | **Disagree** | **Agree** | **Strongly Agree** | **Mean** | **SD** |
|:-------------------|:-----:|:---------------------:|:------------:|:---------:|:------------------:|:--------:|:------:|
| recommend_friend   |  20   |        4 (20%)        |   3 (15%)    |  5 (25%)  |      8 (40%)       |   2.85   |  1.18  |
| regret_purchase    |  20   |        4 (20%)        |   6 (30%)    |  5 (25%)  |      5 (25%)       |   2.55   |  1.10  |
