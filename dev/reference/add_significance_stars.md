# Add significance stars

Add significance stars to estimates with small p-values

## Usage

``` r
add_significance_stars(
  x,
  pattern = ifelse(inherits(x, c("tbl_regression", "tbl_uvregression")),
    "{estimate}{stars}", "{p.value}{stars}"),
  thresholds = c(0.001, 0.01, 0.05),
  hide_ci = TRUE,
  hide_p = inherits(x, c("tbl_regression", "tbl_uvregression")),
  hide_se = FALSE
)
```

## Arguments

- x:

  (`gtsummary`)  
  A `'gtsummary'` object with a `'p.value'` column

- pattern:

  (`string`)  
  glue-syntax string indicating what to display in formatted column.
  Default is `"{estimate}{stars}"` for regression summaries and
  `"{p.value}{stars}"` otherwise. A footnote is placed on the first
  column listed in the pattern. Other common patterns are
  `"{estimate}{stars} ({conf.low}, {conf.high})"` and
  `"{estimate} ({conf.low} to {conf.high}){stars}"`

- thresholds:

  (`numeric`)  
  Thresholds for significance stars. Default is `c(0.001, 0.01, 0.05)`

- hide_ci:

  (scalar `logical`)  
  logical whether to hide confidence interval. Default is `TRUE`

- hide_p:

  (scalar `logical`)  
  logical whether to hide p-value. Default is `TRUE` for regression
  summaries, and `FALSE` otherwise.

- hide_se:

  (scalar `logical`)  
  logical whether to hide standard error. Default is `FALSE`

## Value

a 'gtsummary' table

## Examples

``` r
tbl <-
  lm(time ~ ph.ecog + sex, survival::lung) |>
  tbl_regression(label = list(ph.ecog = "ECOG Score", sex = "Sex"))

# Example 1 ----------------------------------
tbl |>
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)


  

Characteristic
```

**Beta**¹

**SE**

**95% CI**

**p-value**

ECOG Score

-58\*\*

19.1

-96, -21

0.003

Sex

52

27.9

-2.5, 107

0.061

¹ \*p\<0.05; \*\*p\<0.01; \*\*\*p\<0.001

Abbreviations: CI = Confidence Interval, SE = Standard Error

\# Example 2 ---------------------------------- tbl \|\>
add_significance_stars( pattern = "{estimate} ({conf.low},
{conf.high}){stars}", hide_ci = TRUE, hide_se = TRUE ) \|\>
[modify_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)(estimate
= "\*\*Beta (95% CI)\*\*") \|\>
[modify_abbreviation](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_abbreviation.md)("CI
= Confidence Interval")

| **Characteristic**                       | **Beta (95% CI)**¹ |
|:-----------------------------------------|:------------------:|
| ECOG Score                               | -58 (-96, -21)\*\* |
| Sex                                      |   52 (-2.5, 107)   |
| ¹ \*p\<0.05; \*\*p\<0.01; \*\*\*p\<0.001 |                    |
| Abbreviation: CI = Confidence Interval   |                    |

\# Example 3 ---------------------------------- \# Use ' \n' to put a
line break between beta and SE tbl \|\> add_significance_stars( hide_se
= TRUE, pattern = "{estimate}{stars} \n({std.error})" ) \|\>
[modify_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)(estimate
= "\*\*Beta \n(SE)\*\*") \|\>
[modify_abbreviation](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_abbreviation.md)("SE
= Standard Error") \|\>
[as_gt](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)()
\|\>
gt::[fmt_markdown](https://gt.rstudio.com/reference/fmt_markdown.html)(columns
=
[everything](https://tidyselect.r-lib.org/reference/everything.html)())
\|\> gt::[tab_style](https://gt.rstudio.com/reference/tab_style.html)(
style = "vertical-align:top", locations =
gt::[cells_body](https://gt.rstudio.com/reference/cells_body.html)(columns
= label) )

[TABLE]

\# Example 4 ----------------------------------
[lm](https://rdrr.io/r/stats/lm.html)(marker ~ stage + grade, data =
trial) \|\>
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)()
\|\>
[add_global_p](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)()
\|\> add_significance_stars( hide_p = FALSE, pattern =
"{p.value}{stars}" )

[TABLE]
