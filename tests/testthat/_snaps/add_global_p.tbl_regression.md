# add_global_p.tbl_regression returns an error for unsupported anova_fun input

    Code
      res <- add_global_p(tbl, anova_fun = not_anova)
    Condition
      Error in `add_global_p()`:
      ! There was an error running `anova_fun`. See message below.
      x non-numeric argument to binary operator

