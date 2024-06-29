# as_tibble works with standard use

    Code
      as.data.frame(res)
    Output
            **Characteristic** **N = 200**
      1 Chemotherapy Treatment        <NA>
      2                 Drug A    98 (49%)
      3                 Drug B   102 (51%)
      4                    Age 47 (38, 57)
      5                Unknown          11
      6           Patient Died   112 (56%)

---

    Code
      as.data.frame(res)
    Output
        **Characteristic** **OR** **95% CI** **p-value**
      1                Age   1.02 1.00, 1.04        0.10
      2              Grade   <NA>       <NA>        <NA>
      3                  I   <NA>       <NA>        <NA>
      4                 II   0.85 0.39, 1.85         0.7
      5                III   1.01 0.47, 2.16        >0.9

---

    Code
      as.data.frame(res)
    Output
        **Characteristic** **N** **OR** **95% CI** **p-value**
      1                Age   183   1.02 1.00, 1.04        0.10
      2              Grade   193   <NA>       <NA>        <NA>
      3                  I  <NA>   <NA>       <NA>        <NA>
      4                 II  <NA>   0.95 0.45, 2.00         0.9
      5                III  <NA>   1.10 0.52, 2.29         0.8

