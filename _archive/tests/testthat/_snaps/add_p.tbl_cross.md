# add_p.tbl_cross

    Code
      trial %>% tbl_cross(response, death) %>% add_p() %>% as.data.frame()
    Output
                          0    1 Total p-value
      1 Tumor Response <NA> <NA>  <NA>   0.005
      2              0   49   83   132    <NA>
      3              1   37   24    61    <NA>
      4        Unknown    2    5     7    <NA>
      5          Total   88  112   200    <NA>

---

    Code
      trial[c("trt", "grade")] %>% tbl_cross() %>% add_p() %>% as.data.frame()
    Output
                                  I   II  III Total p-value
      1 Chemotherapy Treatment <NA> <NA> <NA>  <NA>     0.9
      2                 Drug A   35   32   31    98    <NA>
      3                 Drug B   33   36   33   102    <NA>
      4                  Total   68   68   64   200    <NA>

---

    Code
      trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(source_note = TRUE) %>%
        as.data.frame()
    Output
                                  I   II  III Total
      1 Chemotherapy Treatment <NA> <NA> <NA>  <NA>
      2                 Drug A   35   32   31    98
      3                 Drug B   33   36   33   102
      4                  Total   68   68   64   200

---

    Code
      tbl %>% as.data.frame()
    Output
                 1    2    3    4    6    8 Total p-value
      1  gear <NA> <NA> <NA> <NA> <NA> <NA>  <NA> 8.6e-02
      2     3    3    4    3    5    0    0    15    <NA>
      3     4    4    4    0    4    0    0    12    <NA>
      4     5    0    2    0    1    1    1     5    <NA>
      5 Total    7   10    3   10    1    1    32    <NA>

---

    Code
      tbl_pt %>% as.data.frame()
    Output
                          0    1 Unknown Total Valor p
      1 Tumor Response <NA> <NA>    <NA>  <NA>    <NA>
      2              0   86   39       7   132    <NA>
      3              1   39   22       0    61    <NA>
      4        Unknown    7    0       0     7    <NA>
      5          Total  132   61       7   200    <NA>

---

    Code
      tbl_pt %>% as.data.frame()
    Output
                          0    1 Unknown Total
      1 Tumor Response <NA> <NA>    <NA>  <NA>
      2              0   86   39       7   132
      3              1   39   22       0    61
      4        Unknown    7    0       0     7
      5          Total  132   61       7   200

