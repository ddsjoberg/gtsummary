# tbl_custom_summary() basics

    Code
      tbl1
    Output
        variable    var_type row_type            var_label                label
      1    grade categorical    label                Grade                Grade
      2    grade categorical    level                Grade                    I
      3    grade categorical    level                Grade                   II
      4    grade categorical    level                Grade                  III
      5 response dichotomous    label       Tumor Response       Tumor Response
      6 response dichotomous  missing       Tumor Response              Unknown
      7   marker  continuous    label Marker Level (ng/mL) Marker Level (ng/mL)
      8   marker  continuous  missing Marker Level (ng/mL)              Unknown
        stat_1 stat_2 stat_0
      1   <NA>   <NA>   <NA>
      2   45.9   46.4   46.2
      3   44.6   50.3   47.5
      4   51.0   45.7   48.1
      5   50.1   49.5   49.8
      6      3      4      7
      7   46.5   47.5   47.0
      8      6      4     10

---

    Code
      tbl
    Output
                           label    n                stat_0                stat_1
      1                __Grade__  200                  <NA>                  <NA>
      2                      _I_ <NA> 1.1 (high, diff: 0.2) 1.2 (high, diff: 0.2)
      3                     _II_ <NA> 0.7 (low, diff: -0.2) 0.9 (low, diff: -0.1)
      4                    _III_ <NA> 1.0 (high, diff: 0.1) 1.0 (high, diff: 0.1)
      5              __T Stage__  200                  <NA>                  <NA>
      6                     _T1_ <NA> 0.7 (low, diff: -0.2) 0.7 (low, diff: -0.2)
      7                     _T2_ <NA> 1.1 (high, diff: 0.2) 1.2 (high, diff: 0.3)
      8                     _T3_ <NA> 1.0 (high, diff: 0.1) 1.1 (high, diff: 0.1)
      9                     _T4_ <NA> 0.9 (low, diff: -0.1) 1.1 (high, diff: 0.2)
      10 __All grades & stages__  200  0.9 (low, diff: 0.0) 1.0 (high, diff: 0.1)
                        stat_2
      1                   <NA>
      2  1.0 (high, diff: 0.1)
      3  0.5 (low, diff: -0.4)
      4  1.0 (high, diff: 0.1)
      5                   <NA>
      6  0.7 (low, diff: -0.3)
      7  1.0 (high, diff: 0.1)
      8  0.9 (high, diff: 0.0)
      9  0.7 (low, diff: -0.2)
      10 0.8 (low, diff: -0.1)

---

    Code
      tbl
    Output
                         label            stat_1            stat_2
      1   Marker Level (ng/mL) 1.02 [0.83; 1.20] 0.82 [0.65; 0.99]
      2                Unknown                 6                 4
      3 Months to Death/Censor 20.2 [19.2; 21.2] 19.0 [18.0; 20.1]

# tbl_custom_summary() manage factor levels with no observation

    Code
      tbl
    Output
          label  stat_1  stat_2
      1 Overall 46.0000 48.0000
      2   grade    <NA>    <NA>
      3       I      46      48
      4      II      45      51
      5     III      52      45
      6      IV      NA      NA

# tbl_custom_summary() helpers work as expected

    Code
      tbl
    Output
          label                     stat_1                      stat_2
      1 T Stage                       <NA>                        <NA>
      2      T1 0.012 [0.00; 0.02] (7/583) 0.021 [0.01; 0.04] (11/522)
      3      T2 0.011 [0.00; 0.02] (6/528)  0.012 [0.01; 0.03] (7/560)
      4      T3 0.019 [0.01; 0.04] (8/426)  0.016 [0.01; 0.03] (7/425)
      5      T4 0.016 [0.01; 0.03] (7/445)  0.018 [0.01; 0.04] (8/434)

---

    Code
      tbl
    Output
        label                    stat_1                  stat_2
      1   Age                      <NA>                    <NA>
      2 Child     45.3% (29/64) [33-58]   62.2% (28/45) [47-76]
      3 Adult 20.3% (338/1,667) [18-22] 74.4% (316/425) [70-78]
      4 Class                      <NA>                    <NA>
      5   1st    34.4% (62/180) [28-42] 97.2% (141/145) [93-99]
      6   2nd   14.0% (25/179) [9.4-20]  87.7% (93/106) [80-93]
      7   3rd    17.3% (88/510) [14-21]  45.9% (90/196) [39-53]
      8  Crew   22.3% (192/862) [20-25]   87.0% (20/23) [65-97]

# character/date summaries do not cause error

    Code
      as.data.frame(tbl)
    Output
        **Characteristic**  **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1            T Stage                  <NA>                  <NA>
      2                 T1  1 (low) [2016-06-15]  1 (low) [2016-06-15]
      3                 T2 1 (high) [2016-06-15] 1 (high) [2016-06-15]
      4                 T3 1 (high) [2016-06-15] 1 (high) [2016-06-15]
      5                 T4 1 (high) [2016-06-15]  1 (low) [2016-06-15]

# full_data contains all observations including missing values

    Code
      res
    Output
        **Characteristic** **N = 200**
      1                Age     189/200
      2            Unknown          11

