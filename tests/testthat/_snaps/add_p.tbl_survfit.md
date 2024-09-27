# add_p.tbl_survfit() works

    Code
      as.data.frame(add_p(tbl))
    Output
            **Characteristic**    **Time 12** **p-value**
      1 Chemotherapy Treatment           <NA>         0.2
      2                 Drug A 91% (85%, 97%)        <NA>
      3                 Drug B 86% (80%, 93%)        <NA>

# add_p.tbl_survfit(pvalue_fun) works

    Code
      as.data.frame(add_p(tbl, pvalue_fun = s_ns))
    Output
            **Characteristic**    **Time 12** **p-value**
      1 Chemotherapy Treatment           <NA>        N.S.
      2                 Drug A 91% (85%, 97%)        <NA>
      3                 Drug B 86% (80%, 93%)        <NA>

# add_p.tbl_survfit() works with tbl_survfit(type)

    Code
      as.data.frame(add_p(tbl))
    Output
            **Characteristic**      **Time 12** **p-value**
      1 Chemotherapy Treatment             <NA>         0.2
      2                 Drug A 9.2% (3.3%, 15%)        <NA>
      3                 Drug B  14% (6.8%, 20%)        <NA>

