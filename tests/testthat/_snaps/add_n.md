# no errors/warnings with standard use

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N**       **N = 200**
      1  Chemotherapy Treatment   200              <NA>
      2                  Drug A  <NA>          98 (49%)
      3                  Drug B  <NA>         102 (51%)
      4                     Age   189       47 (38, 57)
      5                 Unknown  <NA>                11
      6    Marker Level (ng/mL)   190 0.64 (0.22, 1.39)
      7                 Unknown  <NA>                10
      8                 T Stage   200              <NA>
      9                      T1  <NA>          53 (27%)
      10                     T2  <NA>          54 (27%)
      11                     T3  <NA>          43 (22%)
      12                     T4  <NA>          50 (25%)
      13                  Grade   200              <NA>
      14                      I  <NA>          68 (34%)
      15                     II  <NA>          68 (34%)
      16                    III  <NA>          64 (32%)
      17         Tumor Response   193          61 (32%)
      18                Unknown  <NA>                 7
      19           Patient Died   200         112 (56%)
      20 Months to Death/Censor   200 22.4 (16.0, 24.0)

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age   189        46 (37, 59)         48 (39, 56)
      2                 Unknown  <NA>                  7                   4
      3    Marker Level (ng/mL)   190  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)
      4                 Unknown  <NA>                  6                   4
      5                 T Stage   200               <NA>                <NA>
      6                      T1  <NA>           28 (29%)            25 (25%)
      7                      T2  <NA>           25 (26%)            29 (28%)
      8                      T3  <NA>           22 (22%)            21 (21%)
      9                      T4  <NA>           23 (23%)            27 (26%)
      10                  Grade   200               <NA>                <NA>
      11                      I  <NA>           35 (36%)            33 (32%)
      12                     II  <NA>           32 (33%)            36 (35%)
      13                    III  <NA>           31 (32%)            33 (32%)
      14         Tumor Response   193           28 (29%)            33 (34%)
      15                Unknown  <NA>                  3                   4
      16           Patient Died   200           52 (53%)            60 (59%)
      17 Months to Death/Censor   200  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic**       **N = 200** **N**
      1  Chemotherapy Treatment              <NA>   200
      2                  Drug A          98 (49%)  <NA>
      3                  Drug B         102 (51%)  <NA>
      4                     Age       47 (38, 57)   189
      5                 Unknown                11  <NA>
      6    Marker Level (ng/mL) 0.64 (0.22, 1.39)   190
      7                 Unknown                10  <NA>
      8                 T Stage              <NA>   200
      9                      T1          53 (27%)  <NA>
      10                     T2          54 (27%)  <NA>
      11                     T3          43 (22%)  <NA>
      12                     T4          50 (25%)  <NA>
      13                  Grade              <NA>   200
      14                      I          68 (34%)  <NA>
      15                     II          68 (34%)  <NA>
      16                    III          64 (32%)  <NA>
      17         Tumor Response          61 (32%)   193
      18                Unknown                 7  <NA>
      19           Patient Died         112 (56%)   200
      20 Months to Death/Censor 22.4 (16.0, 24.0)   200

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **N**
      1                     Age        46 (37, 59)         48 (39, 56)   189
      2                 Unknown                  7                   4  <NA>
      3    Marker Level (ng/mL)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)   190
      4                 Unknown                  6                   4  <NA>
      5                 T Stage               <NA>                <NA>   200
      6                      T1           28 (29%)            25 (25%)  <NA>
      7                      T2           25 (26%)            29 (28%)  <NA>
      8                      T3           22 (22%)            21 (21%)  <NA>
      9                      T4           23 (23%)            27 (26%)  <NA>
      10                  Grade               <NA>                <NA>   200
      11                      I           35 (36%)            33 (32%)  <NA>
      12                     II           32 (33%)            36 (35%)  <NA>
      13                    III           31 (32%)            33 (32%)  <NA>
      14         Tumor Response           28 (29%)            33 (34%)   193
      15                Unknown                  3                   4  <NA>
      16           Patient Died           52 (53%)            60 (59%)   200
      17 Months to Death/Censor  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)   200

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic**         **N**       **N = 200**
      1  Chemotherapy Treatment   20020001000              <NA>
      2                  Drug A          <NA>          98 (49%)
      3                  Drug B          <NA>         102 (51%)
      4                     Age 20018911955.5       47 (38, 57)
      5                 Unknown          <NA>                11
      6    Marker Level (ng/mL) 20019010955.0 0.64 (0.22, 1.39)
      7                 Unknown          <NA>                10
      8                 T Stage   20020001000              <NA>
      9                      T1          <NA>          53 (27%)
      10                     T2          <NA>          54 (27%)
      11                     T3          <NA>          43 (22%)
      12                     T4          <NA>          50 (25%)
      13                  Grade   20020001000              <NA>
      14                      I          <NA>          68 (34%)
      15                     II          <NA>          68 (34%)
      16                    III          <NA>          64 (32%)
      17         Tumor Response  2001937973.5          61 (32%)
      18                Unknown          <NA>                 7
      19           Patient Died   20020001000         112 (56%)
      20 Months to Death/Censor   20020001000 22.4 (16.0, 24.0)

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic**         **N**       **N = 200**
      1  Chemotherapy Treatment   20020001000              <NA>
      2                  Drug A          <NA>          98 (49%)
      3                  Drug B          <NA>         102 (51%)
      4                     Age 20018911955.5       47 (38, 57)
      5                 Unknown          <NA>                11
      6    Marker Level (ng/mL) 20019010955.0 0.64 (0.22, 1.39)
      7                 Unknown          <NA>                10
      8                 T Stage   20020001000              <NA>
      9                      T1          <NA>          53 (27%)
      10                     T2          <NA>          54 (27%)
      11                     T3          <NA>          43 (22%)
      12                     T4          <NA>          50 (25%)
      13                  Grade   20020001000              <NA>
      14                      I          <NA>          68 (34%)
      15                     II          <NA>          68 (34%)
      16                    III          <NA>          64 (32%)
      17         Tumor Response  2001937973.5          61 (32%)
      18                Unknown          <NA>                 7
      19           Patient Died   20020001000         112 (56%)
      20 Months to Death/Censor   20020001000 22.4 (16.0, 24.0)

# no errors/warnings with standard use with continuous2

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N**       **N = 200**
      1  Chemotherapy Treatment   200              <NA>
      2                  Drug A  <NA>          98 (49%)
      3                  Drug B  <NA>         102 (51%)
      4                     Age   189              <NA>
      5            Median (IQR)  <NA>       47 (38, 57)
      6                 Unknown  <NA>                11
      7    Marker Level (ng/mL)   190              <NA>
      8            Median (IQR)  <NA> 0.64 (0.22, 1.39)
      9                 Unknown  <NA>                10
      10                T Stage   200              <NA>
      11                     T1  <NA>          53 (27%)
      12                     T2  <NA>          54 (27%)
      13                     T3  <NA>          43 (22%)
      14                     T4  <NA>          50 (25%)
      15                  Grade   200              <NA>
      16                      I  <NA>          68 (34%)
      17                     II  <NA>          68 (34%)
      18                    III  <NA>          64 (32%)
      19         Tumor Response   193          61 (32%)
      20                Unknown  <NA>                 7
      21           Patient Died   200         112 (56%)
      22 Months to Death/Censor   200              <NA>
      23           Median (IQR)  <NA> 22.4 (16.0, 24.0)

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age   189               <NA>                <NA>
      2            Median (IQR)  <NA>        46 (37, 59)         48 (39, 56)
      3                 Unknown  <NA>                  7                   4
      4    Marker Level (ng/mL)   190               <NA>                <NA>
      5            Median (IQR)  <NA>  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)
      6                 Unknown  <NA>                  6                   4
      7                 T Stage   200               <NA>                <NA>
      8                      T1  <NA>           28 (29%)            25 (25%)
      9                      T2  <NA>           25 (26%)            29 (28%)
      10                     T3  <NA>           22 (22%)            21 (21%)
      11                     T4  <NA>           23 (23%)            27 (26%)
      12                  Grade   200               <NA>                <NA>
      13                      I  <NA>           35 (36%)            33 (32%)
      14                     II  <NA>           32 (33%)            36 (35%)
      15                    III  <NA>           31 (32%)            33 (32%)
      16         Tumor Response   193           28 (29%)            33 (34%)
      17                Unknown  <NA>                  3                   4
      18           Patient Died   200           52 (53%)            60 (59%)
      19 Months to Death/Censor   200               <NA>                <NA>
      20           Median (IQR)  <NA>  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic**       **N = 200** **N**
      1  Chemotherapy Treatment              <NA>   200
      2                  Drug A          98 (49%)  <NA>
      3                  Drug B         102 (51%)  <NA>
      4                     Age              <NA>   189
      5            Median (IQR)       47 (38, 57)  <NA>
      6                 Unknown                11  <NA>
      7    Marker Level (ng/mL)              <NA>   190
      8            Median (IQR) 0.64 (0.22, 1.39)  <NA>
      9                 Unknown                10  <NA>
      10                T Stage              <NA>   200
      11                     T1          53 (27%)  <NA>
      12                     T2          54 (27%)  <NA>
      13                     T3          43 (22%)  <NA>
      14                     T4          50 (25%)  <NA>
      15                  Grade              <NA>   200
      16                      I          68 (34%)  <NA>
      17                     II          68 (34%)  <NA>
      18                    III          64 (32%)  <NA>
      19         Tumor Response          61 (32%)   193
      20                Unknown                 7  <NA>
      21           Patient Died         112 (56%)   200
      22 Months to Death/Censor              <NA>   200
      23           Median (IQR) 22.4 (16.0, 24.0)  <NA>

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **N**
      1                     Age               <NA>                <NA>   189
      2            Median (IQR)        46 (37, 59)         48 (39, 56)  <NA>
      3                 Unknown                  7                   4  <NA>
      4    Marker Level (ng/mL)               <NA>                <NA>   190
      5            Median (IQR)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)  <NA>
      6                 Unknown                  6                   4  <NA>
      7                 T Stage               <NA>                <NA>   200
      8                      T1           28 (29%)            25 (25%)  <NA>
      9                      T2           25 (26%)            29 (28%)  <NA>
      10                     T3           22 (22%)            21 (21%)  <NA>
      11                     T4           23 (23%)            27 (26%)  <NA>
      12                  Grade               <NA>                <NA>   200
      13                      I           35 (36%)            33 (32%)  <NA>
      14                     II           32 (33%)            36 (35%)  <NA>
      15                    III           31 (32%)            33 (32%)  <NA>
      16         Tumor Response           28 (29%)            33 (34%)   193
      17                Unknown                  3                   4  <NA>
      18           Patient Died           52 (53%)            60 (59%)   200
      19 Months to Death/Censor               <NA>                <NA>   200
      20           Median (IQR)  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)  <NA>

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic**         **N**       **N = 200**
      1  Chemotherapy Treatment   20020001000              <NA>
      2                  Drug A          <NA>          98 (49%)
      3                  Drug B          <NA>         102 (51%)
      4                     Age 20018911955.5              <NA>
      5            Median (IQR)          <NA>       47 (38, 57)
      6                 Unknown          <NA>                11
      7    Marker Level (ng/mL) 20019010955.0              <NA>
      8            Median (IQR)          <NA> 0.64 (0.22, 1.39)
      9                 Unknown          <NA>                10
      10                T Stage   20020001000              <NA>
      11                     T1          <NA>          53 (27%)
      12                     T2          <NA>          54 (27%)
      13                     T3          <NA>          43 (22%)
      14                     T4          <NA>          50 (25%)
      15                  Grade   20020001000              <NA>
      16                      I          <NA>          68 (34%)
      17                     II          <NA>          68 (34%)
      18                    III          <NA>          64 (32%)
      19         Tumor Response  2001937973.5          61 (32%)
      20                Unknown          <NA>                 7
      21           Patient Died   20020001000         112 (56%)
      22 Months to Death/Censor   20020001000              <NA>
      23           Median (IQR)          <NA> 22.4 (16.0, 24.0)

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic**         **N** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age 20018911955.5               <NA>                <NA>
      2            Median (IQR)          <NA>        46 (37, 59)         48 (39, 56)
      3                 Unknown          <NA>                  7                   4
      4    Marker Level (ng/mL) 20019010955.0               <NA>                <NA>
      5            Median (IQR)          <NA>  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)
      6                 Unknown          <NA>                  6                   4
      7                 T Stage   20020001000               <NA>                <NA>
      8                      T1          <NA>           28 (29%)            25 (25%)
      9                      T2          <NA>           25 (26%)            29 (28%)
      10                     T3          <NA>           22 (22%)            21 (21%)
      11                     T4          <NA>           23 (23%)            27 (26%)
      12                  Grade   20020001000               <NA>                <NA>
      13                      I          <NA>           35 (36%)            33 (32%)
      14                     II          <NA>           32 (33%)            36 (35%)
      15                    III          <NA>           31 (32%)            33 (32%)
      16         Tumor Response  2001937973.5           28 (29%)            33 (34%)
      17                Unknown          <NA>                  3                   4
      18           Patient Died   20020001000           52 (53%)            60 (59%)
      19 Months to Death/Censor   20020001000               <NA>                <NA>
      20           Median (IQR)          <NA>  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)

# no errors/warnings with standard use for tbl_svysummary

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age   189        46 (37, 59)         48 (39, 56)
      2                 Unknown  <NA>                  7                   4
      3    Marker Level (ng/mL)   190  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)
      4                 Unknown  <NA>                  6                   4
      5                 T Stage   200               <NA>                <NA>
      6                      T1  <NA>           28 (29%)            25 (25%)
      7                      T2  <NA>           25 (26%)            29 (28%)
      8                      T3  <NA>           22 (22%)            21 (21%)
      9                      T4  <NA>           23 (23%)            27 (26%)
      10                  Grade   200               <NA>                <NA>
      11                      I  <NA>           35 (36%)            33 (32%)
      12                     II  <NA>           32 (33%)            36 (35%)
      13                    III  <NA>           31 (32%)            33 (32%)
      14         Tumor Response   193           28 (29%)            33 (34%)
      15                Unknown  <NA>                  3                   4
      16           Patient Died   200           52 (53%)            60 (59%)
      17 Months to Death/Censor   200  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **N**
      1                     Age        46 (37, 59)         48 (39, 56)   189
      2                 Unknown                  7                   4  <NA>
      3    Marker Level (ng/mL)  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)   190
      4                 Unknown                  6                   4  <NA>
      5                 T Stage               <NA>                <NA>   200
      6                      T1           28 (29%)            25 (25%)  <NA>
      7                      T2           25 (26%)            29 (28%)  <NA>
      8                      T3           22 (22%)            21 (21%)  <NA>
      9                      T4           23 (23%)            27 (26%)  <NA>
      10                  Grade               <NA>                <NA>   200
      11                      I           35 (36%)            33 (32%)  <NA>
      12                     II           32 (33%)            36 (35%)  <NA>
      13                    III           31 (32%)            33 (32%)  <NA>
      14         Tumor Response           28 (29%)            33 (34%)   193
      15                Unknown                  3                   4  <NA>
      16           Patient Died           52 (53%)            60 (59%)   200
      17 Months to Death/Censor  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)   200

---

    Code
      res %>% as.data.frame()
    Output
         **Characteristic**           **N** **No**, N = 1,490 **Yes**, N = 711
      1               Class 2,2012,20101000              <NA>             <NA>
      2                 1st            <NA>        122 (8.2%)        203 (29%)
      3                 2nd            <NA>         167 (11%)        118 (17%)
      4                 3rd            <NA>         528 (35%)        178 (25%)
      5                Crew            <NA>         673 (45%)        212 (30%)
      6                 Sex 2,2012,20101000              <NA>             <NA>
      7                Male            <NA>       1,364 (92%)        367 (52%)
      8              Female            <NA>        126 (8.5%)        344 (48%)
      9                 Age 2,2012,20101000              <NA>             <NA>
      10              Child            <NA>         52 (3.5%)        57 (8.0%)
      11              Adult            <NA>       1,438 (97%)        654 (92%)
      12               Freq 2,2012,20101000    342 (140, 513)     79 (64, 144)

# no errors/warnings with standard use for tbl_svysummary with continuous2

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age   189               <NA>                <NA>
      2            Median (IQR)  <NA>        46 (37, 59)         48 (39, 56)
      3                 Unknown  <NA>                  7                   4
      4    Marker Level (ng/mL)   190               <NA>                <NA>
      5            Median (IQR)  <NA>  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)
      6                 Unknown  <NA>                  6                   4
      7                 T Stage   200               <NA>                <NA>
      8                      T1  <NA>           28 (29%)            25 (25%)
      9                      T2  <NA>           25 (26%)            29 (28%)
      10                     T3  <NA>           22 (22%)            21 (21%)
      11                     T4  <NA>           23 (23%)            27 (26%)
      12                  Grade   200               <NA>                <NA>
      13                      I  <NA>           35 (36%)            33 (32%)
      14                     II  <NA>           32 (33%)            36 (35%)
      15                    III  <NA>           31 (32%)            33 (32%)
      16         Tumor Response   193           28 (29%)            33 (34%)
      17                Unknown  <NA>                  3                   4
      18           Patient Died   200           52 (53%)            60 (59%)
      19 Months to Death/Censor   200               <NA>                <NA>
      20           Median (IQR)  <NA>  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **N**
      1                     Age               <NA>                <NA>   189
      2            Median (IQR)        46 (37, 59)         48 (39, 56)  <NA>
      3                 Unknown                  7                   4  <NA>
      4    Marker Level (ng/mL)               <NA>                <NA>   190
      5            Median (IQR)  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)  <NA>
      6                 Unknown                  6                   4  <NA>
      7                 T Stage               <NA>                <NA>   200
      8                      T1           28 (29%)            25 (25%)  <NA>
      9                      T2           25 (26%)            29 (28%)  <NA>
      10                     T3           22 (22%)            21 (21%)  <NA>
      11                     T4           23 (23%)            27 (26%)  <NA>
      12                  Grade               <NA>                <NA>   200
      13                      I           35 (36%)            33 (32%)  <NA>
      14                     II           32 (33%)            36 (35%)  <NA>
      15                    III           31 (32%)            33 (32%)  <NA>
      16         Tumor Response           28 (29%)            33 (34%)   193
      17                Unknown                  3                   4  <NA>
      18           Patient Died           52 (53%)            60 (59%)   200
      19 Months to Death/Censor               <NA>                <NA>   200
      20           Median (IQR)  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)  <NA>

---

    Code
      res %>% as.data.frame()
    Output
         **Characteristic**           **N** **No**, N = 1,490 **Yes**, N = 711
      1               Class 2,2012,20101000              <NA>             <NA>
      2                 1st            <NA>        122 (8.2%)        203 (29%)
      3                 2nd            <NA>         167 (11%)        118 (17%)
      4                 3rd            <NA>         528 (35%)        178 (25%)
      5                Crew            <NA>         673 (45%)        212 (30%)
      6                 Sex 2,2012,20101000              <NA>             <NA>
      7                Male            <NA>       1,364 (92%)        367 (52%)
      8              Female            <NA>        126 (8.5%)        344 (48%)
      9                 Age 2,2012,20101000              <NA>             <NA>
      10              Child            <NA>         52 (3.5%)        57 (8.0%)
      11              Adult            <NA>       1,438 (97%)        654 (92%)
      12               Freq 2,2012,20101000              <NA>             <NA>
      13       Median (IQR)            <NA>    342 (140, 513)     79 (64, 144)

# add_n.tbl_survfit

    Code
      res %>% as.data.frame()
    Output
            **Characteristic** **N**    **Time 12**    **Time 24**
      1                Overall   200 89% (84%, 93%) 44% (38%, 51%)
      2 Chemotherapy Treatment   200           <NA>           <NA>
      3                 Drug A  <NA> 91% (85%, 97%) 47% (38%, 58%)
      4                 Drug B  <NA> 86% (80%, 93%) 41% (33%, 52%)

# add_n.tbl_regression

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **log(OR)**  **95% CI** **p-value**
      1              Grade   183        <NA>        <NA>        <NA>
      2                  I  <NA>        <NA>        <NA>        <NA>
      3                 II  <NA>       -0.16 -0.94, 0.61         0.7
      4                III  <NA>        0.01 -0.74, 0.77        >0.9
      5                Age   183        0.02  0.00, 0.04        0.10

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **log(OR)**  **95% CI** **p-value**
      1              Grade  <NA>        <NA>        <NA>        <NA>
      2                  I    65        <NA>        <NA>        <NA>
      3                 II    58       -0.16 -0.94, 0.61         0.7
      4                III    60        0.01 -0.74, 0.77        >0.9
      5                Age   183        0.02  0.00, 0.04        0.10

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **log(OR)**  **95% CI** **p-value**
      1              Grade   183        <NA>        <NA>        <NA>
      2                  I    65        <NA>        <NA>        <NA>
      3                 II    58       -0.16 -0.94, 0.61         0.7
      4                III    60        0.01 -0.74, 0.77        >0.9
      5                Age   183        0.02  0.00, 0.04        0.10

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **log(OR)**  **95% CI** **p-value**
      1                Age   183        0.02  0.00, 0.04        0.10
      2              Grade   193        <NA>        <NA>        <NA>
      3                  I  <NA>        <NA>        <NA>        <NA>
      4                 II  <NA>       -0.06 -0.81, 0.69         0.9
      5                III  <NA>        0.09 -0.65, 0.83         0.8

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **log(OR)**  **95% CI** **p-value**
      1                Age   183        0.02  0.00, 0.04        0.10
      2              Grade  <NA>        <NA>        <NA>        <NA>
      3                  I    67        <NA>        <NA>        <NA>
      4                 II    63       -0.06 -0.81, 0.69         0.9
      5                III    63        0.09 -0.65, 0.83         0.8

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **log(OR)**  **95% CI** **p-value**
      1                Age   183        0.02  0.00, 0.04        0.10
      2              Grade   193        <NA>        <NA>        <NA>
      3                  I    67        <NA>        <NA>        <NA>
      4                 II    63       -0.06 -0.81, 0.69         0.9
      5                III    63        0.09 -0.65, 0.83         0.8

