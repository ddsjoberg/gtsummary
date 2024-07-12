# add_global_p.tbl_uvregression(x)

    Code
      as.data.frame(res)
    Output
        **Characteristic** **N** **Beta**   **95% CI** **p-value**
      1                Age   179     0.00  -0.01, 0.01        >0.9
      2              Grade   190     <NA>         <NA>       0.025
      3                  I  <NA>     <NA>         <NA>        <NA>
      4                 II  <NA>    -0.39 -0.68, -0.09        <NA>
      5                III  <NA>    -0.07  -0.37, 0.23        <NA>

# modify tidy_fun to not show p-values

    Code
      res6 %>% as.data.frame()
    Output
        **Characteristic** **N** **Beta**  **95% CI** **p-value**
      1                Age   183     0.00  0.00, 0.01       0.092
      2              Grade   193     <NA>        <NA>        >0.9
      3                  I  <NA>     <NA>        <NA>        <NA>
      4                 II  <NA>    -0.01 -0.17, 0.15        <NA>
      5                III  <NA>     0.02 -0.14, 0.18        <NA>

