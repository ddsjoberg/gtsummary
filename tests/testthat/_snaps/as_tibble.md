# as_tibble works with standard use

    Code
      as.data.frame(res)
    Output
             **Characteristic**       **N = 200**
      1  Chemotherapy Treatment              <NA>
      2                  Drug A          98 (49%)
      3                  Drug B         102 (51%)
      4                     Age       47 (38, 57)
      5                 Unknown                11
      6    Marker Level (ng/mL) 0.64 (0.22, 1.41)
      7                 Unknown                10
      8                 T Stage              <NA>
      9                      T1          53 (27%)
      10                     T2          54 (27%)
      11                     T3          43 (22%)
      12                     T4          50 (25%)
      13                  Grade              <NA>
      14                      I          68 (34%)
      15                     II          68 (34%)
      16                    III          64 (32%)
      17         Tumor Response          61 (32%)
      18                Unknown                 7
      19           Patient Died         112 (56%)
      20 Months to Death/Censor 22.4 (15.9, 24.0)

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

# as_tibble works with bold/italics

    Code
      as.data.frame(res)
    Output
                 **Characteristic**       **N = 200**
      1  __Chemotherapy Treatment__              <NA>
      2                    _Drug A_          98 (49%)
      3                    _Drug B_         102 (51%)
      4                     __Age__       47 (38, 57)
      5                   _Unknown_                11
      6    __Marker Level (ng/mL)__ 0.64 (0.22, 1.41)
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
      20 __Months to Death/Censor__ 22.4 (15.9, 24.0)

---

    Code
      as.data.frame(res)
    Output
        **Characteristic** **OR** **95% CI** **p-value**
      1            __Age__   1.02 1.00, 1.04        0.10
      2          __Grade__   <NA>       <NA>        <NA>
      3                _I_   <NA>       <NA>        <NA>
      4               _II_   0.85 0.39, 1.85         0.7
      5              _III_   1.01 0.47, 2.16        >0.9

---

    Code
      as.data.frame(res)
    Output
        **Characteristic** **N** **OR** **95% CI** **p-value**
      1              _Age_   183   1.02 1.00, 1.04        0.10
      2            _Grade_   193   <NA>       <NA>        <NA>
      3              __I__  <NA>   <NA>       <NA>        <NA>
      4             __II__  <NA>   0.95 0.45, 2.00         0.9
      5            __III__  <NA>   1.10 0.52, 2.29         0.8

# as_tibble works with formatting functions

    Code
      as.data.frame(res)
    Output
        **Characteristic** **OR** **95% CI** **p-value**
      1            __Age__ 1,0191 1.00, 1.04    __0.10__
      2          __Grade__   <NA>       <NA>        <NA>
      3                _I_   <NA>       <NA>        <NA>
      4               _II_ 0,8535 0.39, 1.85       0.688
      5              _III_ 1,0136 0.47, 2.16       0.972

---

    Code
      as.data.frame(res)
    Output
        **Characteristic**    **N** **OR**  **95% CI** **p-value**
      1              _Age_   183.00   1.02 0.997, 1.04        0.10
      2            _Grade_ 193.0000   <NA>        <NA>        <NA>
      3              __I__     <NA>   <NA>        <NA>        <NA>
      4             __II__     <NA>   0.95 0.446, 2.00         0.9
      5            __III__     <NA>   1.10 0.524, 2.29         0.8

# as_tibble works with column merging

    Code
      as.data.frame(res)
    Output
        **Characteristic** **Beta** **95% CI** **p-value** **Beta** **95% CI**
      1        factor(cyl)     <NA>       <NA>        <NA>     <NA>       <NA>
      2                  4     <NA>       <NA>        <NA>     <NA>       <NA>
      3                  6     -6.9  -10, -3.7      <0.001     <NA>       <NA>
      4                  8      -12  -14, -8.9      <0.001     -4.6 -6.8, -2.4
        **p-value**
      1        <NA>
      2        <NA>
      3        <NA>
      4      <0.001

# as_tibble(fmt_missing=) works

    Code
      as.data.frame(res)
    Output
              label estimate_1 conf.low_1 p.value_1 estimate_2 conf.low_2 p.value_2
      1 factor(cyl)       <NA>       <NA>      <NA>       <NA>       <NA>      <NA>
      2           4          —          —      <NA>       <NA>       <NA>      <NA>
      3           6       -6.9  -10, -3.7    <0.001          —          —      <NA>
      4           8        -12  -14, -8.9    <0.001       -4.6 -6.8, -2.4    <0.001

# as_tibble works with column grouping

    Code
      as.data.frame(res)
    Output
        **Group** **Characteristic** **N = 200**
      1        T1                Age 47 (38, 57)
      2        T2                Age 47 (38, 57)

