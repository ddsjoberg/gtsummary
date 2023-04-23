# no errors/warnings with all output types

    Code
      tbl %>% as.data.frame()
    Output
        **Characteristic** **N = 200**
      1                Age 47 (38, 57)
      2            Unknown          11

---

    Code
      tbl2 %>% as.data.frame()
    Output
        **Characteristic** **N = 200**
      1                Age 47 (38, 57)
      2            Unknown          11

---

    Code
      tbl_with_caption %>% as.data.frame()
    Output
         label2 **N = 200**
      1     Age 47 (38, 57)
      2 Unknown          11

