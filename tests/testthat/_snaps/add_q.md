# no errors/warnings with standard use after tbl_summary() and add_p()

    Code
      add_q(table1) %>% as.data.frame()
    Message
      add_q: Adjusting p-values with
      `stats::p.adjust(x$table_body$p.value, method = "fdr")`
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1                     Age        46 (37, 59)         48 (39, 56)         0.7
      2                 Unknown                  7                   4        <NA>
      3    Marker Level (ng/mL)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)       0.085
      4                 Unknown                  6                   4        <NA>
      5                 T Stage               <NA>                <NA>         0.9
      6                      T1           28 (29%)            25 (25%)        <NA>
      7                      T2           25 (26%)            29 (28%)        <NA>
      8                      T3           22 (22%)            21 (21%)        <NA>
      9                      T4           23 (23%)            27 (26%)        <NA>
      10                  Grade               <NA>                <NA>         0.9
      11                      I           35 (36%)            33 (32%)        <NA>
      12                     II           32 (33%)            36 (35%)        <NA>
      13                    III           31 (32%)            33 (32%)        <NA>
      14         Tumor Response           28 (29%)            33 (34%)         0.5
      15                Unknown                  3                   4        <NA>
      16           Patient Died           52 (53%)            60 (59%)         0.4
      17 Months to Death/Censor  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)        0.14
         **q-value**
      1          0.9
      2         <NA>
      3          0.5
      4         <NA>
      5          0.9
      6         <NA>
      7         <NA>
      8         <NA>
      9         <NA>
      10         0.9
      11        <NA>
      12        <NA>
      13        <NA>
      14         0.9
      15        <NA>
      16         0.9
      17         0.5

# no errors/warnings with standard use after tbl_uvregression() and add_global_p()

    Code
      add_q(uni_reg) %>% as.data.frame()
    Message
      add_q: Adjusting p-values with
      `stats::p.adjust(x$table_body$p.value, method = "fdr")`
    Output
             **Characteristic** **N** **Beta**  **95% CI** **p-value** **q-value**
      1  Chemotherapy Treatment   189     <NA>        <NA>         0.8        >0.9
      2                  Drug A  <NA>     <NA>        <NA>        <NA>        <NA>
      3                  Drug B  <NA>     0.44   -3.7, 4.6        <NA>        <NA>
      4    Marker Level (ng/mL)   179    -0.05   -2.5, 2.4        >0.9        >0.9
      5                 T Stage   189     <NA>        <NA>         0.5         0.9
      6                      T1  <NA>     <NA>        <NA>        <NA>        <NA>
      7                      T2  <NA>      1.3   -4.2, 6.9        <NA>        <NA>
      8                      T3  <NA>      2.6   -3.3, 8.6        <NA>        <NA>
      9                      T4  <NA>     -2.0   -7.8, 3.8        <NA>        <NA>
      10                  Grade   189     <NA>        <NA>         0.7        >0.9
      11                      I  <NA>     <NA>        <NA>        <NA>        <NA>
      12                     II  <NA>      1.4   -3.6, 6.4        <NA>        <NA>
      13                    III  <NA>      2.0   -3.1, 7.0        <NA>        <NA>
      14         Tumor Response   183      3.8  -0.66, 8.3       0.094         0.7
      15           Patient Died   189      2.2   -2.0, 6.3         0.3         0.9
      16 Months to Death/Censor   189    -0.14 -0.54, 0.26         0.5         0.9

