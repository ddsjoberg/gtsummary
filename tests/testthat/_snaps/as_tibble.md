# as_tibble works with standard use

    Code
      as_tibble(t1) %>% as.data.frame()
    Output
                 **Characteristic**       **N = 200**
      1  __Chemotherapy Treatment__              <NA>
      2                    _Drug A_          98 (49%)
      3                    _Drug B_         102 (51%)
      4                     __Age__       47 (38, 57)
      5                   _Unknown_                11
      6    __Marker Level (ng/mL)__ 0.64 (0.22, 1.39)
      7                   _Unknown_                10
      8                 __T Stage__              <NA>
      9                        _T1_          53 (27%)
      10                       _T2_          54 (27%)
      11                       _T3_          43 (22%)
      12                       _T4_          50 (25%)
      13                  __Grade__              <NA>
      14                        _I_          68 (34%)
      15                       _II_          68 (34%)
      16                      _III_          64 (32%)
      17         __Tumor Response__          61 (32%)
      18                  _Unknown_                 7
      19           __Patient Died__         112 (56%)
      20 __Months to Death/Censor__ 22.4 (16.0, 24.0)

---

    Code
      as_tibble(t3) %>% as.data.frame()
    Output
        **Characteristic** **N** **OR** **95% CI** **p-value**
      1              _Age_   183   1.02 1.00, 1.04        0.10
      2            _Grade_   193   <NA>       <NA>        <NA>
      3              __I__  <NA>   <NA>       <NA>        <NA>
      4             __II__  <NA>   0.95 0.45, 2.00         0.9
      5            __III__  <NA>   1.10 0.52, 2.29         0.8

---

    Code
      as_tibble(t3) %>% as.data.frame()
    Output
        **Characteristic** **N** **OR** **95% CI** **p-value**
      1              _Age_   183   1.02 1.00, 1.04        0.10
      2            _Grade_   193   <NA>       <NA>        <NA>
      3              __I__  <NA>   <NA>       <NA>        <NA>
      4             __II__  <NA>   0.95 0.45, 2.00         0.9
      5            __III__  <NA>   1.10 0.52, 2.29         0.8

# as_tibble(fmt_missing=) works

    Code
      tbl %>% as.data.frame()
    Output
              label estimate_1      ci_1 p.value_1 estimate_2       ci_2 p.value_2
      1 factor(cyl)       <NA>      <NA>      <NA>       <NA>       <NA>      <NA>
      2           4          —         —      <NA>       <NA>       <NA>      <NA>
      3           6       -6.9 -10, -3.7    <0.001          —          —      <NA>
      4           8        -12 -14, -8.9    <0.001       -4.6 -6.8, -2.4    <0.001

