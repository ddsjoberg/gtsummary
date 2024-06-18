# no errors/warnings with standard use after tbl_uvregression

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **Beta**   **95% CI** **p-value**
      1                Age   179     0.00  -0.01, 0.01        >0.9
      2              Grade   190     <NA>         <NA>       0.025
      3                  I  <NA>     <NA>         <NA>        <NA>
      4                 II  <NA>    -0.39 -0.68, -0.09        <NA>
      5                III  <NA>    -0.07  -0.37, 0.23        <NA>

