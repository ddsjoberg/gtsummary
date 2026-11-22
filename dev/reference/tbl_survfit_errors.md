# Common Sources of Error with `tbl_survfit()`

When functions
[`add_n()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.md)
and
[`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
are run after
[`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md),
the original call to
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
is extracted and the `formula=` and `data=` arguments are used to
calculate the N or p-value.

When the values of the `formula=` and `data=` are unavailable, the
functions cannot execute. Below are some tips to modify your code to
ensure all functions run without issue.

1.  Let
    [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
    construct the
    [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
    for you by passing a data frame to
    [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md).
    The survfit model will be constructed in a manner ensuring the
    formula and data are available. This only works if you have a
    stratified model.

    Instead of the following line

        survfit(Surv(ttdeath, death) ~ trt, trial) %>%
          tbl_survfit(times = c(12, 24))

    Use this code

        trial %>%
          select(ttdeath, death, trt) %>%
          tbl_survfit(y = Surv(ttdeath, death), times = c(12, 24))

2.  Construct an expression of the
    [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
    before evaluating it. Ensure the formula and data are available in
    the call by using the tidyverse bang-bang operator, `!!`.

    Use this code

        formula_arg <- Surv(ttdeath, death) ~ 1
        data_arg <- trial
        rlang::expr(survfit(!!formula_arg, !!data_arg)) %>%
          eval() %>%
          tbl_survfit(times = c(12, 24))
