# no errors/warnings with standard use

    Code
      t4 %>% as.data.frame()
    Output
            **Characteristic** **N** **OR** **95% CI** **p-value** **OR** **95% CI**
      1 Chemotherapy Treatment   193   <NA>       <NA>        <NA>   <NA>       <NA>
      2                 Drug A  <NA>   <NA>       <NA>        <NA>   <NA>       <NA>
      3                 Drug B  <NA>   1.21 0.66, 2.24         0.5   1.13 0.60, 2.13
      4                  Grade   193   <NA>       <NA>        <NA>   <NA>       <NA>
      5                      I  <NA>   <NA>       <NA>        <NA>   <NA>       <NA>
      6                     II  <NA>   0.95 0.45, 2.00         0.9   0.85 0.39, 1.85
      7                    III  <NA>   1.10 0.52, 2.29         0.8   1.01 0.47, 2.15
      8                    Age   183   1.02 1.00, 1.04        0.10   1.02 1.00, 1.04
        **p-value** **HR** **95% CI** **p-value** **HR** **95% CI** **p-value**
      1        <NA>   <NA>       <NA>        <NA>   <NA>       <NA>        <NA>
      2        <NA>   <NA>       <NA>        <NA>   <NA>       <NA>        <NA>
      3         0.7   1.30 0.88, 1.92         0.2   1.25 0.86, 1.81         0.2
      4        <NA>   <NA>       <NA>        <NA>   <NA>       <NA>        <NA>
      5        <NA>   <NA>       <NA>        <NA>   <NA>       <NA>        <NA>
      6         0.7   1.21 0.73, 1.99         0.5   1.28 0.80, 2.05         0.3
      7        >0.9   1.79 1.12, 2.86       0.014   1.69 1.07, 2.66       0.024
      8        0.10   1.01 0.99, 1.02         0.3   1.01 0.99, 1.02         0.3

# tbl_merge() column ordering

    Code
      tbl_merge(list(t1, t2, t3)) %>% as.data.frame()
    Output
        **Characteristic** **Beta**  **95% CI** **p-value** **Beta**  **95% CI**
      1                Age     0.00 -0.01, 0.01        >0.9     <NA>        <NA>
      2     Tumor Response     <NA>        <NA>        <NA>     0.23 -0.04, 0.50
        **p-value** **Beta**  **95% CI** **p-value**
      1        <NA>     0.00 -0.01, 0.01        >0.9
      2        0.10     <NA>        <NA>        <NA>

# tbl_merge() no spanning header

    Code
      tbl_no_spanning %>% as.data.frame()
    Output
        **Characteristic** **Beta** **95% CI** **p-value** **Beta** **95% CI**
      1        factor(cyl)     <NA>       <NA>        <NA>     <NA>       <NA>
      2                  4     <NA>       <NA>        <NA>     <NA>       <NA>
      3                  6     -6.2 -9.3, -3.0      <0.001     -6.2 -9.3, -3.0
      4                  8      -10  -13, -7.1      <0.001      -10  -13, -7.1
      5                 am      2.6 -0.10, 5.2       0.058      2.6 -0.10, 5.2
        **p-value**
      1        <NA>
      2        <NA>
      3      <0.001
      4      <0.001
      5       0.058

# tbl_merge() one table

    Code
      tbl_only_one %>% as.data.frame()
    Output
        **Characteristic** **Beta** **95% CI** **p-value**
      1        factor(cyl)     <NA>       <NA>        <NA>
      2                  4     <NA>       <NA>        <NA>
      3                  6     -6.2 -9.3, -3.0      <0.001
      4                  8      -10  -13, -7.1      <0.001
      5                 am      2.6 -0.10, 5.2       0.058

# tbl_merge with complicated tbl_stack + cols_merge

    Code
      tbl_merge(tbls = list(t3, t4)) %>% modify_spanning_header(everything() ~ NA) %>%
        as.data.frame()
    Output
        **Characteristic** **N** **Summary Statistics** **HR** **(95% CI)**
      1                Age   189                   <NA> 1.01 (0.99 to 1.02)
      2       Median (IQR)  <NA>           47 (38 – 57)                <NA>
      3     Tumor Response   193                   <NA> 0.50 (0.31 to 0.78)
      4       Median (IQR)  <NA>              0 (0 – 1)                <NA>
        **p-value**
      1        0.33
      2        <NA>
      3       0.003
      4        <NA>

