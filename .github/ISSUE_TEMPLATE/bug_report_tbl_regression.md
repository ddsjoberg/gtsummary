---
name: "Bug report: tbl_regression()"
about: Report a bug in tbl_regression(), tbl_uvregression(), or other model-based tables
title: 'tbl_regression() Bug Report: <Describe bug>'
labels: ''
assignees: ''

---

Do not use this form to ask a question, or ask for assistance. Instead, try asking an AI assistant, or ask on <https://stackoverflow.com/> using the gtsummary tag.
Questions about a function's use will be closed without a response.

Most issues reported for `tbl_regression()` are not gtsummary bugs. `tbl_regression()` relies on the {broom.helpers} package to tidy the model, so before filing please confirm:

- [ ] The model type is listed among the [supported models](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#supported-models). If it is not, support requests can be filed at <https://github.com/larmarange/broom.helpers/issues>.
- [ ] `broom.helpers::tidy_plus_plus(<your model>)` runs without error and returns the results you expect. If it does not, please report the bug at <https://github.com/larmarange/broom.helpers/issues>.

If both of the above check out, please briefly describe your problem and what output you expect.

INCLUDE a *minimal* reproducible example (AKA a reprex) with both data and code that anyone can run. If you've never heard of a [reprex](https://reprex.tidyverse.org/) before, start by reading <https://www.tidyverse.org/help/#reprex>. It'll take minutes to master creating a reprex.
ISSUES WITHOUT A REPRODUCIBLE EXAMPLE WILL LIKELY BE CLOSED WITHOUT A RESPONSE.

---

Brief description of the problem.

```r
# insert reprex here, including the model, the `tbl_regression()` call,
# and the output of `broom.helpers::tidy_plus_plus(<your model>)`
```
