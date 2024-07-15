# add_p.tbl_survfit() works

    Code
      add_p(tbl)

# add_p.tbl_survfit(pvalue_fun) works

    Code
      as.data.frame(add_p(tbl_survfit(trial, include = trt, y = "Surv(ttdeath, death)",
        times = 12), pvalue_fun = s_ns))
    Output
            **Characteristic**    **Time 12** **p-value**
      1 Chemotherapy Treatment           <NA>        N.S.
      2                 Drug A 91% (85%, 97%)        <NA>
      3                 Drug B 86% (80%, 93%)        <NA>

