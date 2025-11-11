# Survival table

Function takes a `survfit` object as an argument, and provides a
formatted summary table of the results.

No more than one stratifying variable is allowed in each model. If
you're experiencing unexpected errors using `tbl_survfit()`, please
review
[?tbl_survfit_errors](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit_errors.md)
for a possible explanation.

## Usage

``` r
tbl_survfit(x, ...)

# S3 method for class 'survfit'
tbl_survfit(x, ...)

# S3 method for class 'data.frame'
tbl_survfit(x, y, include = everything(), conf.level = 0.95, ...)

# S3 method for class 'list'
tbl_survfit(
  x,
  times = NULL,
  probs = NULL,
  statistic = "{estimate} ({conf.low}, {conf.high})",
  label = NULL,
  label_header = ifelse(!is.null(times), "**Time {time}**",
    "**{style_sigfig(prob, scale=100)}% Percentile**"),
  estimate_fun = ifelse(!is.null(times), label_style_percent(suffix = "%"),
    label_style_sigfig()),
  missing = "--",
  type = NULL,
  reverse = FALSE,
  quiet = TRUE,
  ...
)
```

## Arguments

- x:

  (`survfit`, `list`, `data.frame`)  
  a survfit object, list of survfit objects, or a data frame. If a data
  frame is passed, a list of survfit objects is constructed using each
  variable as a stratifying variable.

- ...:

  For `tbl_survfit.data.frame()` and `tbl_survfit.survfit()` the
  arguments are passed to `tbl_survfit.list()`. They are not used when
  `tbl_survfit.list()` is called directly.

- y:

  outcome call, e.g. `y = Surv(ttdeath, death)`

- include:

  Variable to include as stratifying variables.

- conf.level:

  (scalar `numeric`)  
  \] Confidence level for confidence intervals. Default is `0.95`

- times:

  (`numeric`)  
  a vector of times for which to return survival probabilities.

- probs:

  (`numeric`)  
  a vector of probabilities with values in (0,1) specifying the survival
  quantiles to return.

- statistic:

  (`string`)  
  string defining the statistics to present in the table. Default is
  `"{estimate} ({conf.low}, {conf.high})"`

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  List of formulas specifying variables labels, e.g.
  `list(age = "Age, yrs", stage = "Path T Stage")`, or a string for a
  single variable table.

- label_header:

  (`string`)  
  string specifying column labels above statistics. Default is
  `"{prob} Percentile"` for survival percentiles, and `"Time {time}"`
  for n-year survival estimates

- estimate_fun:

  (`function`)  
  function to format the Kaplan-Meier estimates. Default is
  [`label_style_percent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)
  for survival probabilities and
  [`label_style_sigfig()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)
  for survival times

- missing:

  (`string`)  
  text to fill when estimate is not estimable. Default is `"--"`

- type:

  (`string` or `NULL`)  
  type of statistic to report. Available for Kaplan-Meier time estimates
  only, otherwise `type` is ignored. Default is `NULL`. Must be one of
  the following:

  |              |                |
  |--------------|----------------|
  | type         | transformation |
  | `"survival"` | `x`            |
  | `"risk"`     | `1 - x`        |
  | `"cumhaz"`   | `-log(x)`      |

- reverse:

  **\[deprecated\]**

- quiet:

  **\[deprecated\]**

## Formula Specification

When passing a
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
object to `tbl_survfit()`, the
[`survfit()`](https://rdrr.io/pkg/survival/man/survfit.html) call must
use an evaluated formula and not a stored formula. Including a proper
formula in the call allows the function to accurately identify all
variables included in the estimation. See below for examples:

    library(gtsummary)
    library(survival)

    # include formula in `survfit()` call
    survfit(Surv(time, status) ~ sex, lung) |> tbl_survfit(times = 500)

    # you can also pass a data frame to `tbl_survfit()` as well.
    lung |>
      tbl_survfit(y = Surv(time, status), include = "sex", times = 500)

You **cannot**, however, pass a stored formula, e.g.
`survfit(my_formula, lung)`, but you can use stored formulas with
`rlang::inject(survfit(!!my_formula, lung))`.

## Author

Daniel D. Sjoberg

## Examples

``` r
library(survival)

# Example 1 ----------------------------------
# Pass single survfit() object
tbl_survfit(
  survfit(Surv(ttdeath, death) ~ trt, trial),
  times = c(12, 24),
  label_header = "**{time} Month**"
)


  

Characteristic
```

**12 Month**

**24 Month**

Chemotherapy Treatment

  

  

    Drug A

91% (85%, 97%)

47% (38%, 58%)

    Drug B

86% (80%, 93%)

41% (33%, 52%)

\# Example 2 ---------------------------------- \# Pass a data frame
tbl_survfit( trial, y = "Surv(ttdeath, death)", include =
[c](https://rdrr.io/r/base/c.html)(trt, grade), probs = 0.5,
label_header = "\*\*Median Survival\*\*" )

[TABLE]

\# Example 3 ---------------------------------- \# Pass a list of
survfit() objects
[list](https://rdrr.io/r/base/list.html)([survfit](https://rdrr.io/pkg/survival/man/survfit.html)([Surv](https://rdrr.io/pkg/survival/man/Surv.html)(ttdeath,
death) ~ 1, trial),
[survfit](https://rdrr.io/pkg/survival/man/survfit.html)([Surv](https://rdrr.io/pkg/survival/man/Surv.html)(ttdeath,
death) ~ trt, trial)) \|\> tbl_survfit(times =
[c](https://rdrr.io/r/base/c.html)(12, 24))

[TABLE]

\# Example 4 Competing Events Example --------- \# adding a competing
event for death (cancer vs other causes)
[set.seed](https://rdrr.io/r/base/Random.html)(1123)
[library](https://rdrr.io/r/base/library.html)([dplyr](https://dplyr.tidyverse.org),
warn.conflicts = FALSE, quietly = TRUE) trial2 \<- trial \|\>
dplyr::[mutate](https://dplyr.tidyverse.org/reference/mutate.html)(
death_cr =
dplyr::[case_when](https://dplyr.tidyverse.org/reference/case_when.html)(
death == 0 ~ "censor",
[runif](https://rdrr.io/r/stats/Uniform.html)([n](https://dplyr.tidyverse.org/reference/context.html)())
\< 0.5 ~ "death from cancer", TRUE ~ "death other causes" ) \|\>
[factor](https://rdrr.io/r/base/factor.html)() )
[survfit](https://rdrr.io/pkg/survival/man/survfit.html)([Surv](https://rdrr.io/pkg/survival/man/Surv.html)(ttdeath,
death_cr) ~ grade, data = trial2) \|\> tbl_survfit(times =
[c](https://rdrr.io/r/base/c.html)(12, 24), label = "Tumor Grade") \#\>
Multi-state model detected. Showing probabilities into state 'death from
\#\> cancer'.

[TABLE]
