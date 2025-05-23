# tbl_merge works with more complex merge

    Code
      as.data.frame(modify_spanning_header(tbl_merge(tbls = list(t1, t2)), everything() ~
        NA))
    Output
        **Characteristic** **N** **Summary Statistics** **HR** **(95% CI)**
      1                Age   189                   <NA> 1.01 (0.99 to 1.02)
      2   Median (Q1 – Q3)  <NA>           47 (38 – 57)                <NA>
      3     Tumor Response   193                   <NA> 0.50 (0.31 to 0.78)
      4   Median (Q1 – Q3)  <NA>              0 (0 – 1)                <NA>
        **p-value**
      1        0.33
      2        <NA>
      3       0.003
      4        <NA>

# tbl_merge throws expected errors

    Code
      tbl_merge(t1)
    Condition
      Error in `tbl_merge()`:
      ! Expecting argument `tbls` to be class <list>, e.g. `tbl_merge(tbls = list(tbl1, tbl2))`.

---

    Code
      tbl_merge(list(mtcars))
    Condition
      Error in `tbl_merge()`:
      ! All objects in `tbls` list must be class <gtsummary>.

---

    Code
      tbl_merge(tbls = list(t0, t1), tab_spanner = 1)
    Condition
      Error in `tbl_merge()`:
      ! The `tab_spanner` argument must be `NULL`, FALSE, or class <character>.

---

    Code
      tbl_merge(tbls = list(t0, t1), tab_spanner = c("Table"))
    Condition
      Error in `tbl_merge()`:
      ! The lengths of arguments `tbls` and `tab_spanner` must be the same.

---

    Code
      tbl_merge(list(tbl, tbl))
    Condition
      Error in `tbl_merge()`:
      ! The tables in the `tbls` argument do not share any columns specified in `merge_vars` argument and merge cannot be performed.

# tbl_merge(merge_vars)

    Code
      as.data.frame(tbl)
    Output
          **Characteristic** **Beta** **95% CI** **p-value** **Beta** **95% CI**
      1 Marker Level (ng/mL)    -0.05  -2.5, 2.4        >0.9    -0.05  -2.5, 2.4
        **p-value**
      1        >0.9

