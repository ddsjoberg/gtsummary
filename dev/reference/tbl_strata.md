# Stratified gtsummary tables

Build a stratified gtsummary table. Any gtsummary table that accepts a
data frame as its first argument can be stratified.

- In `tbl_strata()`, the stratified or subset data frame is passed to
  the function in `.tbl_fun=`, e.g. `purrr::map(data, .tbl_fun)`.

- In `tbl_strata2()`, both the stratified data frame and the strata
  level are passed to `.tbl_fun=`, e.g.
  `purrr::map2(data, strata, .tbl_fun)`.

Tables are created *independently* within each stratum. When merging,
keep in mind that merging works best with **like tables**. See
[`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
for details.

## Usage

``` r
tbl_strata(
  data,
  strata,
  .tbl_fun,
  ...,
  .sep = ", ",
  .combine_with = c("tbl_merge", "tbl_stack"),
  .combine_args = NULL,
  .header = ifelse(.combine_with == "tbl_merge", "**{strata}**", "{strata}"),
  .quiet = NULL
)

tbl_strata2(
  data,
  strata,
  .tbl_fun,
  ...,
  .sep = ", ",
  .combine_with = c("tbl_merge", "tbl_stack"),
  .combine_args = NULL,
  .header = ifelse(.combine_with == "tbl_merge", "**{strata}**", "{strata}"),
  .quiet = TRUE
)
```

## Arguments

- data:

  (`data.frame`, `survey.design`)  
  a data frame or survey object

- strata:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  character vector or tidy-selector of columns in data to stratify
  results by. Only *observed* combinations are shown in results.

- .tbl_fun:

  (`function`) A function or formula. If a *function*, it is used as is.
  If a formula, e.g. `~ .x %>% tbl_summary() %>% add_p()`, it is
  converted to a function. The stratified data frame is passed to this
  function.

- ...:

  Additional arguments passed on to the `.tbl_fun` function.

- .sep:

  (`string`)  
  when more than one stratifying variable is passed, this string is used
  to separate the levels in the spanning header. Default is `", "`

- .combine_with:

  (`string`)  
  One of `c("tbl_merge", "tbl_stack")`. Names the function used to
  combine the stratified tables.

- .combine_args:

  (named `list`)  
  named list of arguments that are passed to function specified in
  `.combine_with`

- .header:

  (`string`)  
  String indicating the headers that will be placed. Default is
  `"**{strata}**"` when `.combine_with = "tbl_merge"` and `"{strata}"`
  when `.combine_with = "tbl_stack"`. Items placed in curly brackets
  will be evaluated according to
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  syntax.

      - `strata` stratum levels

      - `n` N within stratum

      - `N` Overall N

  The evaluated value of `.header` is also available within
  `tbl_strata2(.tbl_fun)`

- .quiet:

  **\[deprecated\]**

## Tips

- [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)

  - The number of digits continuous variables are rounded to is
    determined separately within each stratum of the data frame. Set the
    `digits=` argument to ensure continuous variables are rounded to the
    same number of decimal places.

  - If some levels of a categorical variable are unobserved within a
    stratum, convert the variable to a factor to ensure all levels
    appear in each stratum's summary table.

  - The summary type for variables (e.g. continuous vs categorical vs
    dichotomous) are determined separately within stratum. Use the
    `tbl_summary(type)` argument to assign a summary type consistent
    across all tables being combined.

  - By default, a "missing" row appears when there are missing values
    only. Use the `tbl_summary(missing)` argument to ensure there is
    always/never a missing row for the combining of the tables.

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  select(age, grade, stage, trt) |>
  mutate(grade = paste("Grade", grade)) |>
  tbl_strata(
    strata = grade,
    .tbl_fun =
      ~ .x |>
        tbl_summary(by = trt, missing = "no") |>
        add_n(),
    .header = "**{strata}**, N = {n}"
  )


  


Characteristic
```

**Grade I**, N = 68

**Grade II**, N = 68

**Grade III**, N = 64

**N**

**Drug A**  
N = 35¹

**Drug B**  
N = 33¹

**N**

**Drug A**  
N = 32¹

**Drug B**  
N = 36¹

**N**

**Drug A**  
N = 31¹

**Drug B**  
N = 33¹

Age

66

46 (36, 60)

48 (42, 55)

62

45 (31, 55)

51 (42, 58)

61

52 (42, 61)

45 (36, 52)

T Stage

68

  

  

68

  

  

64

  

  

    T1

  

8 (23%)

9 (27%)

  

14 (44%)

9 (25%)

  

6 (19%)

7 (21%)

    T2

  

8 (23%)

10 (30%)

  

8 (25%)

9 (25%)

  

9 (29%)

10 (30%)

    T3

  

11 (31%)

7 (21%)

  

5 (16%)

6 (17%)

  

6 (19%)

8 (24%)

    T4

  

8 (23%)

7 (21%)

  

5 (16%)

12 (33%)

  

10 (32%)

8 (24%)

¹ Median (Q1, Q3); n (%)

\# Example 2 ---------------------------------- trial \|\>
[select](https://dplyr.tidyverse.org/reference/select.html)(grade,
response) \|\>
[mutate](https://dplyr.tidyverse.org/reference/mutate.html)(grade =
[paste](https://rdrr.io/r/base/paste.html)("Grade", grade)) \|\>
tbl_strata2( strata = grade, .tbl_fun = ~ .x
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(
label = [list](https://rdrr.io/r/base/list.html)(response = .y), missing
= "no", statistic = response ~ "{p}%" ) \|\>
[add_ci](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.md)(pattern
= "{stat} ({ci})") \|\>
[modify_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)(stat_0
= "\*\*Rate (95% CI)\*\*") \|\>
[remove_footnote_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)(stat_0),
.combine_with = "tbl_stack", .combine_args =
[list](https://rdrr.io/r/base/list.html)(group_header = NULL) ) \|\>
[modify_caption](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md)("\*\*Response
Rate by Grade\*\*")

| **Characteristic**                     | **Rate (95% CI)** |
|:---------------------------------------|:-----------------:|
| Grade I                                |  31% (21%, 44%)   |
| Grade II                               |  30% (20%, 43%)   |
| Grade III                              |  33% (22%, 46%)   |
| Abbreviation: CI = Confidence Interval |                   |

**Response Rate by Grade**
