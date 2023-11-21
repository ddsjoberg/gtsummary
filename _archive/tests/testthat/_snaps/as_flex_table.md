# tbl_summary

    Code
      tbl
    Output
      a flextable object.
      col_keys: `label`, `stat_0` 
      header has 1 row(s) 
      body has 20 row(s) 
      original dataset sample: 
                         label      stat_0
      1 Chemotherapy Treatment        <NA>
      2                 Drug A    98 (49%)
      3                 Drug B   102 (51%)
      4                    Age 47 (38, 57)
      5                Unknown          11

---

    Code
      tbl
    Output
      a flextable object.
      col_keys: `label`, `stat_0` 
      header has 1 row(s) 
      body has 5 row(s) 
      original dataset sample: 
                         label      stat_0
      1 Chemotherapy Treatment        <NA>
      2                 Drug A    98 (49%)
      3                 Drug B   102 (51%)
      4                    Age 47 (38, 57)
      5                Unknown          11

# tbl_cross

    Code
      tbl
    Output
      a flextable object.
      col_keys: `label`, `stat_1`, `stat_2`, `stat_3`, `stat_0` 
      header has 2 row(s) 
      body has 4 row(s) 
      original dataset sample: 
                         label stat_1 stat_2 stat_3 stat_0
      1 Chemotherapy Treatment   <NA>   <NA>   <NA>   <NA>
      2                 Drug A     67     28      3     98
      3                 Drug B     65     33      4    102
      4                  Total    132     61      7    200

# tbl_regression

    Code
      tbl
    Output
      a flextable object.
      col_keys: `label`, `estimate`, `ci`, `p.value` 
      header has 1 row(s) 
      body has 1 row(s) 
      original dataset sample: 
        label estimate          ci p.value
      1   Age     0.00 -0.01, 0.01    >0.9

# tbl_uvregression

    Code
      tbl
    Output
      a flextable object.
      col_keys: `label`, `stat_n`, `estimate`, `ci`, `p.value` 
      header has 1 row(s) 
      body has 16 row(s) 
      original dataset sample: 
                         label stat_n estimate        ci p.value
      1 Chemotherapy Treatment    189     <NA>      <NA>    <NA>
      2                 Drug A   <NA>     <NA>      <NA>    <NA>
      3                 Drug B   <NA>     0.44 -3.7, 4.6     0.8
      4   Marker Level (ng/mL)    179    -0.05 -2.5, 2.4    >0.9
      5                T Stage    189     <NA>      <NA>    <NA>

# tbl_survfit

    Code
      tbl
    Output
      a flextable object.
      col_keys: `label`, `stat_1`, `stat_2` 
      header has 1 row(s) 
      body has 3 row(s) 
      original dataset sample: 
                         label         stat_1         stat_2
      1 Chemotherapy Treatment           <NA>           <NA>
      2                 Drug A 91% (85%, 97%) 47% (38%, 58%)
      3                 Drug B 86% (80%, 93%) 41% (33%, 52%)

# tbl_merge/tbl_stack

    Code
      tbl
    Output
      a flextable object.
      col_keys: `label`, `estimate_1`, `ci_1`, `p.value_1`, `estimate_2`, `ci_2`, `p.value_2` 
      header has 2 row(s) 
      body has 8 row(s) 
      original dataset sample: 
                         label estimate_1       ci_1 p.value_1 estimate_2       ci_2
      1 Chemotherapy Treatment       <NA>       <NA>      <NA>       <NA>       <NA>
      2                 Drug A       <NA>       <NA>      <NA>       <NA>       <NA>
      3                 Drug B       1.13 0.60, 2.13       0.7       1.30 0.88, 1.92
      4                  Grade       <NA>       <NA>      <NA>       <NA>       <NA>
      5                      I       <NA>       <NA>      <NA>       <NA>       <NA>
        p.value_2
      1      <NA>
      2      <NA>
      3       0.2
      4      <NA>
      5      <NA>

---

    Code
      tbl
    Output
      a flextable object.
      col_keys: `label`, `estimate_1`, `ci_1`, `p.value_1`, `estimate_2`, `ci_2`, `p.value_2` 
      header has 2 row(s) 
      body has 8 row(s) 
      original dataset sample: 
                         label estimate_1       ci_1 p.value_1 estimate_2       ci_2
      1 Chemotherapy Treatment       <NA>       <NA>      <NA>       <NA>       <NA>
      2                 Drug A       <NA>       <NA>      <NA>       <NA>       <NA>
      3                 Drug B       1.13 0.60, 2.13       0.7       1.30 0.88, 1.92
      4                  Grade       <NA>       <NA>      <NA>       <NA>       <NA>
      5                      I       <NA>       <NA>      <NA>       <NA>       <NA>
        p.value_2
      1      <NA>
      2      <NA>
      3       0.2
      4      <NA>
      5      <NA>

# source notes

    Code
      mtcars %>% tbl_cross(row = vs, col = am, margin = "col") %>% add_p(source_note = TRUE) %>%
        as_flex_table()
    Output
      a flextable object.
      col_keys: `label`, `stat_1`, `stat_2`, `stat_0` 
      header has 2 row(s) 
      body has 3 row(s) 
      original dataset sample: 
        label stat_1 stat_2 stat_0
      1    vs   <NA>   <NA>   <NA>
      2     0     12      6     18
      3     1      7      7     14

