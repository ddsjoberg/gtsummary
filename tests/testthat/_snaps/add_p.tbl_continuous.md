# add_p.tbl_continuous(pvalue_fun) works

    Code
      as.data.frame(add_p(tbl_continuous(trial, variable = age, by = trt, include = c(
        grade, stage)), pvalue_fun = s_ns))
    Output
        **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102 **p-value**
      1              Grade                 <NA>                  <NA>        N.S.
      2                  I          46 (36, 60)           48 (42, 55)        <NA>
      3                 II          45 (31, 55)           51 (42, 58)        <NA>
      4                III          52 (42, 61)           45 (36, 52)        <NA>
      5            T Stage                 <NA>                  <NA>        N.S.
      6                 T1          43 (31, 53)           47 (43, 57)        <NA>
      7                 T2          48 (41, 63)           49 (42, 53)        <NA>
      8                 T3          48 (38, 61)           53 (40, 59)        <NA>
      9                 T4          46 (36, 60)           45 (37, 54)        <NA>

