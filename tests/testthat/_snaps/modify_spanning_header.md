# modify_spanning_header works

    Code
      tbl1 %>% modify_spanning_header(starts_with("stat_") ~
        "**Randomization Assignment**") %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age        46 (37, 59)         48 (39, 56)
      2            Unknown                  7                   4
      3              Grade               <NA>                <NA>
      4                  I           35 (36%)            33 (32%)
      5                 II           32 (33%)            36 (35%)
      6                III           31 (32%)            33 (32%)

---

    Code
      tbl1 %>% modify_spanning_header(label = "Variables", starts_with("stat_") ~
        "**Randomization Assignment**") %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age        46 (37, 59)         48 (39, 56)
      2            Unknown                  7                   4
      3              Grade               <NA>                <NA>
      4                  I           35 (36%)            33 (32%)
      5                 II           32 (33%)            36 (35%)
      6                III           31 (32%)            33 (32%)

