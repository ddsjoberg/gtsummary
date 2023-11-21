# tab_style: bold and italicize

    Code
      tbl %>% bold_labels() %>% bold_levels() %>% italicize_labels() %>%
        italicize_levels() %>% bold_p() %>% bold_p(q = TRUE, t = 0.2) %>%
        as.data.frame()
    Output
                   **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                     ___Age___        46 (37, 59)         48 (39, 56)
      2                 ___Unknown___                  7                   4
      3    ___Marker Level (ng/mL)___  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)
      4                 ___Unknown___                  6                   4
      5                 ___T Stage___               <NA>                <NA>
      6                      ___T1___           28 (29%)            25 (25%)
      7                      ___T2___           25 (26%)            29 (28%)
      8                      ___T3___           22 (22%)            21 (21%)
      9                      ___T4___           23 (23%)            27 (26%)
      10                  ___Grade___               <NA>                <NA>
      11                      ___I___           35 (36%)            33 (32%)
      12                     ___II___           32 (33%)            36 (35%)
      13                    ___III___           31 (32%)            33 (32%)
      14         ___Tumor Response___           28 (29%)            33 (34%)
      15                ___Unknown___                  3                   4
      16           ___Patient Died___           52 (53%)            60 (59%)
      17 ___Months to Death/Censor___  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)
         **p-value** **q-value**
      1          0.7         0.9
      2         <NA>        <NA>
      3        0.085         0.5
      4         <NA>        <NA>
      5          0.9         0.9
      6         <NA>        <NA>
      7         <NA>        <NA>
      8         <NA>        <NA>
      9         <NA>        <NA>
      10         0.9         0.9
      11        <NA>        <NA>
      12        <NA>        <NA>
      13        <NA>        <NA>
      14         0.5         0.9
      15        <NA>        <NA>
      16         0.4         0.9
      17        0.14         0.5

---

    Code
      tbl_cross_ex %>% bold_labels() %>% bold_levels() %>% italicize_labels() %>%
        italicize_levels() %>% as.data.frame()
    Output
                                     ***0*** ***1*** ***Unknown*** ***Total***
      1 ___Chemotherapy Treatment___    <NA>    <NA>          <NA>        <NA>
      2                 ___Drug A___      67      28             3          98
      3                 ___Drug B___      65      33             4         102
      4                  ___Total___     132      61             7         200

---

    Code
      tbl_uv_ex1 %>% bold_labels() %>% bold_levels() %>% italicize_labels() %>%
        italicize_levels() %>% as.data.frame()
    Output
        **Characteristic** **N** **OR** **95% CI** **p-value**
      1          ___Age___   183   1.02 1.00, 1.04        0.10
      2        ___Grade___   193   <NA>       <NA>        <NA>
      3            ___I___  <NA>   <NA>       <NA>        <NA>
      4           ___II___  <NA>   0.95 0.45, 2.00         0.9
      5          ___III___  <NA>   1.10 0.52, 2.29         0.8

