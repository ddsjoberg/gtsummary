# add_p.tbl_survfit works

    Code
      survfit_list %>% purrr::map(~ tbl_survfit(.x, times = c(12, 24)) %>% add_p() %>%
        as.data.frame())
    Output
      [[1]]
            **Characteristic**    **Time 12**    **Time 24** **p-value**
      1 Chemotherapy Treatment           <NA>           <NA>         0.2
      2                 Drug A 91% (85%, 97%) 47% (38%, 58%)        <NA>
      3                 Drug B 86% (80%, 93%) 41% (33%, 52%)        <NA>
      
      [[2]]
        **Characteristic**    **Time 12**    **Time 24** **p-value**
      1          trial$trt           <NA>           <NA>         0.2
      2             Drug A 91% (85%, 97%) 47% (38%, 58%)        <NA>
      3             Drug B 86% (80%, 93%) 41% (33%, 52%)        <NA>
      

---

    Code
      survfit_list %>% tbl_survfit(prob = c(seq(0.1, 0.9, by = 0.1))) %>% add_p() %>%
        as.data.frame()
    Output
            **Characteristic** **10% Percentile** **20% Percentile**
      1 Chemotherapy Treatment               <NA>               <NA>
      2                 Drug A        12 (10, 16)        16 (14, 18)
      3                 Drug B       10 (9.9, 13)        13 (11, 16)
      4              trial$trt               <NA>               <NA>
      5                 Drug A        12 (10, 16)        16 (14, 18)
      6                 Drug B       10 (9.9, 13)        13 (11, 16)
        **30% Percentile** **40% Percentile** **50% Percentile** **60% Percentile**
      1               <NA>               <NA>               <NA>               <NA>
      2        18 (16, 21)        21 (18, 24)         24 (21, —)          — (24, —)
      3        16 (13, 18)        18 (16, 21)         21 (18, —)          — (22, —)
      4               <NA>               <NA>               <NA>               <NA>
      5        18 (16, 21)        21 (18, 24)         24 (21, —)          — (24, —)
      6        16 (13, 18)        18 (16, 21)         21 (18, —)          — (22, —)
        **70% Percentile** **80% Percentile** **90% Percentile** **p-value**
      1               <NA>               <NA>               <NA>         0.2
      2           — (—, —)           — (—, —)           — (—, —)        <NA>
      3           — (—, —)           — (—, —)           — (—, —)        <NA>
      4               <NA>               <NA>               <NA>         0.2
      5           — (—, —)           — (—, —)           — (—, —)        <NA>
      6           — (—, —)           — (—, —)           — (—, —)        <NA>

---

    Code
      survfit_list[[1]] %>% tbl_survfit(prob = c(seq(0.1, 0.9, by = 0.1))) %>% add_p() %>%
        as.data.frame()
    Output
            **Characteristic** **10% Percentile** **20% Percentile**
      1 Chemotherapy Treatment               <NA>               <NA>
      2                 Drug A        12 (10, 16)        16 (14, 18)
      3                 Drug B       10 (9.9, 13)        13 (11, 16)
        **30% Percentile** **40% Percentile** **50% Percentile** **60% Percentile**
      1               <NA>               <NA>               <NA>               <NA>
      2        18 (16, 21)        21 (18, 24)         24 (21, —)          — (24, —)
      3        16 (13, 18)        18 (16, 21)         21 (18, —)          — (22, —)
        **70% Percentile** **80% Percentile** **90% Percentile** **p-value**
      1               <NA>               <NA>               <NA>         0.2
      2           — (—, —)           — (—, —)           — (—, —)        <NA>
      3           — (—, —)           — (—, —)           — (—, —)        <NA>

---

    Code
      trial %>% select(trt, grade, ttdeath, death) %>% tbl_survfit(times = c(12, 24),
      y = survival::Surv(ttdeath, death)) %>% add_p() %>% as.data.frame()
    Output
            **Characteristic**     **Time 12**    **Time 24** **p-value**
      1 Chemotherapy Treatment            <NA>           <NA>         0.2
      2                 Drug A  91% (85%, 97%) 47% (38%, 58%)        <NA>
      3                 Drug B  86% (80%, 93%) 41% (33%, 52%)        <NA>
      4                  Grade            <NA>           <NA>       0.072
      5                      I 97% (93%, 100%) 51% (41%, 65%)        <NA>
      6                     II  82% (74%, 92%) 47% (37%, 61%)        <NA>
      7                    III  86% (78%, 95%) 33% (23%, 47%)        <NA>

# add_p.tbl_survfit survdiff family checks

    Code
      tbl_survfit %>% as.data.frame()
    Output
             **Characteristic**     **Time 12**    **Time 24**
      1  Chemotherapy Treatment            <NA>           <NA>
      2                  Drug A  91% (85%, 97%) 47% (38%, 58%)
      3                  Drug B  86% (80%, 93%) 41% (33%, 52%)
      4          Tumor Response            <NA>           <NA>
      5                       0  86% (81%, 92%) 37% (30%, 46%)
      6                       1 95% (90%, 100%) 61% (50%, 74%)
      7                   Grade            <NA>           <NA>
      8                       I 97% (93%, 100%) 51% (41%, 65%)
      9                      II  82% (74%, 92%) 47% (37%, 61%)
      10                    III  86% (78%, 95%) 33% (23%, 47%)
      11                T Stage            <NA>           <NA>
      12                     T1 94% (88%, 100%) 55% (43%, 70%)
      13                     T2  89% (81%, 98%) 50% (38%, 65%)
      14                     T3 91% (82%, 100%) 49% (36%, 66%)
      15                     T4  80% (70%, 92%) 22% (13%, 37%)

# add_p.tbl_survfit coxph family checks

    Code
      tbl_survfit %>% as.data.frame()
    Output
             **Characteristic**     **Time 12**    **Time 24**
      1  Chemotherapy Treatment            <NA>           <NA>
      2                  Drug A  91% (85%, 97%) 47% (38%, 58%)
      3                  Drug B  86% (80%, 93%) 41% (33%, 52%)
      4          Tumor Response            <NA>           <NA>
      5                       0  86% (81%, 92%) 37% (30%, 46%)
      6                       1 95% (90%, 100%) 61% (50%, 74%)
      7                   Grade            <NA>           <NA>
      8                       I 97% (93%, 100%) 51% (41%, 65%)
      9                      II  82% (74%, 92%) 47% (37%, 61%)
      10                    III  86% (78%, 95%) 33% (23%, 47%)

