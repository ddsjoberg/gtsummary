# Stacking tbl_regression objects

    Code
      tbl_stack(list(t1, t2), group_header = c("Group 1", "Group 2")) %>%
        as.data.frame()
    Output
        **Group**     **Characteristic** **OR** **95% CI** **p-value**
      1   Group 1 Treatment (unadjusted)   <NA>       <NA>        <NA>
      2      <NA>                 Drug A   <NA>       <NA>        <NA>
      3      <NA>                 Drug B   1.21 0.66, 2.24         0.5
      4   Group 2   Treatment (adjusted)   <NA>       <NA>        <NA>
      5      <NA>                 Drug A   <NA>       <NA>        <NA>
      6      <NA>                 Drug B   1.48 0.78, 2.86         0.2

# Stacking tbl_merge objects

    Code
      tbl_stack(list(row1, row2)) %>% as.data.frame()
    Message
      i Column headers among stacked tables differ. Headers from the first table are
      used. Use `quiet = TRUE` to suppress this message.
    Output
            **Characteristic** **OR** **95% CI** **p-value** **HR** **95% CI**
      1 Treatment (unadjusted)   <NA>       <NA>        <NA>   <NA>       <NA>
      2                 Drug A   <NA>       <NA>        <NA>   <NA>       <NA>
      3                 Drug B   1.21 0.66, 2.24         0.5   1.25 0.86, 1.81
      4   Treatment (adjusted)   <NA>       <NA>        <NA>   <NA>       <NA>
      5                 Drug A   <NA>       <NA>        <NA>   <NA>       <NA>
      6                 Drug B   1.48 0.78, 2.86         0.2   1.30 0.88, 1.92
        **p-value**
      1        <NA>
      2        <NA>
      3         0.2
      4        <NA>
      5        <NA>
      6         0.2

# Stacking tbl_summary objects

    Code
      zz %>% as.data.frame()
    Output
             **Characteristic**    **0**, N = 132     **1**, N = 61 **p-value**
      1  Chemotherapy Treatment              <NA>              <NA>         0.5
      2                  Drug A          67 (51%)          28 (46%)        <NA>
      3                  Drug B          65 (49%)          33 (54%)        <NA>
      4                     Age       46 (36, 55)       49 (43, 59)       0.091
      5                 Unknown                 7                 3        <NA>
      6    Marker Level (ng/mL) 0.59 (0.21, 1.24) 0.98 (0.31, 1.53)        0.10
      7                 Unknown                 6                 4        <NA>
      8                 T Stage              <NA>              <NA>         0.6
      9                      T1          34 (26%)          18 (30%)        <NA>
      10                     T2          39 (30%)          13 (21%)        <NA>
      11                     T3          25 (19%)          15 (25%)        <NA>
      12                     T4          34 (26%)          15 (25%)        <NA>
      13                  Grade              <NA>              <NA>        >0.9
      14                      I          46 (35%)          21 (34%)        <NA>
      15                     II          44 (33%)          19 (31%)        <NA>
      16                    III          42 (32%)          21 (34%)        <NA>
      17           Patient Died          83 (63%)          24 (39%)       0.002
      18 Months to Death/Censor 20.6 (15.0, 24.0) 24.0 (18.4, 24.0)       0.001
      19                    Age       46 (37, 59)       48 (39, 56)         0.7
      20                Unknown                 7                 4        <NA>
      21   Marker Level (ng/mL) 0.84 (0.24, 1.57) 0.52 (0.19, 1.20)       0.085
      22                Unknown                 6                 4        <NA>
      23                T Stage              <NA>              <NA>         0.9
      24                     T1          28 (29%)          25 (25%)        <NA>
      25                     T2          25 (26%)          29 (28%)        <NA>
      26                     T3          22 (22%)          21 (21%)        <NA>
      27                     T4          23 (23%)          27 (26%)        <NA>
      28                  Grade              <NA>              <NA>         0.9
      29                      I          35 (36%)          33 (32%)        <NA>
      30                     II          32 (33%)          36 (35%)        <NA>
      31                    III          31 (32%)          33 (32%)        <NA>
      32         Tumor Response          28 (29%)          33 (34%)         0.5
      33                Unknown                 3                 4        <NA>
      34           Patient Died          52 (53%)          60 (59%)         0.4
      35 Months to Death/Censor 23.5 (17.4, 24.0) 21.2 (14.6, 24.0)        0.14
         **q-value**
      1          0.7
      2         <NA>
      3         <NA>
      4          0.2
      5         <NA>
      6          0.2
      7         <NA>
      8          0.7
      9         <NA>
      10        <NA>
      11        <NA>
      12        <NA>
      13        >0.9
      14        <NA>
      15        <NA>
      16        <NA>
      17       0.008
      18       0.008
      19         0.9
      20        <NA>
      21         0.5
      22        <NA>
      23         0.9
      24        <NA>
      25        <NA>
      26        <NA>
      27        <NA>
      28         0.9
      29        <NA>
      30        <NA>
      31        <NA>
      32         0.9
      33        <NA>
      34         0.9
      35         0.5

---

    Code
      tbl_stack(lst_summary, group_header = c("Group 1", "Group 2")) %>%
        as.data.frame()
    Message
      i Column headers among stacked tables differ. Headers from the first table are
      used. Use `quiet = TRUE` to suppress this message.
    Output
         **Group**     **Characteristic**    **0**, N = 132     **1**, N = 61
      1    Group 1 Chemotherapy Treatment              <NA>              <NA>
      2       <NA>                 Drug A          67 (51%)          28 (46%)
      3       <NA>                 Drug B          65 (49%)          33 (54%)
      4       <NA>                    Age       46 (36, 55)       49 (43, 59)
      5       <NA>                Unknown                 7                 3
      6       <NA>   Marker Level (ng/mL) 0.59 (0.21, 1.24) 0.98 (0.31, 1.53)
      7       <NA>                Unknown                 6                 4
      8       <NA>                T Stage              <NA>              <NA>
      9       <NA>                     T1          34 (26%)          18 (30%)
      10      <NA>                     T2          39 (30%)          13 (21%)
      11      <NA>                     T3          25 (19%)          15 (25%)
      12      <NA>                     T4          34 (26%)          15 (25%)
      13      <NA>                  Grade              <NA>              <NA>
      14      <NA>                      I          46 (35%)          21 (34%)
      15      <NA>                     II          44 (33%)          19 (31%)
      16      <NA>                    III          42 (32%)          21 (34%)
      17      <NA>           Patient Died          83 (63%)          24 (39%)
      18      <NA> Months to Death/Censor 20.6 (15.0, 24.0) 24.0 (18.4, 24.0)
      19   Group 2                    Age       46 (37, 59)       48 (39, 56)
      20      <NA>                Unknown                 7                 4
      21      <NA>   Marker Level (ng/mL) 0.84 (0.24, 1.57) 0.52 (0.19, 1.20)
      22      <NA>                Unknown                 6                 4
      23      <NA>                T Stage              <NA>              <NA>
      24      <NA>                     T1          28 (29%)          25 (25%)
      25      <NA>                     T2          25 (26%)          29 (28%)
      26      <NA>                     T3          22 (22%)          21 (21%)
      27      <NA>                     T4          23 (23%)          27 (26%)
      28      <NA>                  Grade              <NA>              <NA>
      29      <NA>                      I          35 (36%)          33 (32%)
      30      <NA>                     II          32 (33%)          36 (35%)
      31      <NA>                    III          31 (32%)          33 (32%)
      32      <NA>         Tumor Response          28 (29%)          33 (34%)
      33      <NA>                Unknown                 3                 4
      34      <NA>           Patient Died          52 (53%)          60 (59%)
      35      <NA> Months to Death/Censor 23.5 (17.4, 24.0) 21.2 (14.6, 24.0)
         **p-value** **q-value**
      1          0.5         0.7
      2         <NA>        <NA>
      3         <NA>        <NA>
      4        0.091         0.2
      5         <NA>        <NA>
      6         0.10         0.2
      7         <NA>        <NA>
      8          0.6         0.7
      9         <NA>        <NA>
      10        <NA>        <NA>
      11        <NA>        <NA>
      12        <NA>        <NA>
      13        >0.9        >0.9
      14        <NA>        <NA>
      15        <NA>        <NA>
      16        <NA>        <NA>
      17       0.002       0.008
      18       0.001       0.008
      19         0.7         0.9
      20        <NA>        <NA>
      21       0.085         0.5
      22        <NA>        <NA>
      23         0.9         0.9
      24        <NA>        <NA>
      25        <NA>        <NA>
      26        <NA>        <NA>
      27        <NA>        <NA>
      28         0.9         0.9
      29        <NA>        <NA>
      30        <NA>        <NA>
      31        <NA>        <NA>
      32         0.5         0.9
      33        <NA>        <NA>
      34         0.4         0.9
      35        0.14         0.5

