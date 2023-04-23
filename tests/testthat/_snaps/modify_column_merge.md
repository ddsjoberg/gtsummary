# modify_column_merge() works

    Code
      tbl %>% as.data.frame()
    Output
        **Characteristic**             **Beta** **p-value**
      1                Age   0.00 (-0.01, 0.01)        >0.9
      2              Grade                 <NA>        <NA>
      3                  I                 <NA>        <NA>
      4                 II -0.38 (-0.69, -0.07)       0.015
      5                III  -0.12 (-0.43, 0.19)         0.5

