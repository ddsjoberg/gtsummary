# tbl_summary

    Code
      tbl
    Output
      
      
      |**Characteristic**     |    **N = 200**    |
      |:----------------------|:-----------------:|
      |Chemotherapy Treatment |                   |
      |Drug A                 |     98 (49%)      |
      |Drug B                 |     102 (51%)     |
      |Age                    |    47 (38, 57)    |
      |Unknown                |        11         |
      |Marker Level (ng/mL)   | 0.64 (0.22, 1.39) |
      |Unknown                |        10         |
      |T Stage                |                   |
      |T1                     |     53 (27%)      |
      |T2                     |     54 (27%)      |
      |T3                     |     43 (22%)      |
      |T4                     |     50 (25%)      |
      |Grade                  |                   |
      |I                      |     68 (34%)      |
      |II                     |     68 (34%)      |
      |III                    |     64 (32%)      |
      |Tumor Response         |     61 (32%)      |
      |Unknown                |         7         |
      |Patient Died           |     112 (56%)     |
      |Months to Death/Censor | 22.4 (16.0, 24.0) |

# tbl_cross

    Code
      tbl
    Output
      
      
      |      | Drug A | Drug B | Total |
      |:-----|:------:|:------:|:-----:|
      |Grade |        |        |       |
      |I     |   35   |   33   |  68   |
      |II    |   32   |   36   |  68   |
      |III   |   31   |   33   |  64   |
      |Total |   98   |  102   |  200  |

# tbl_regression

    Code
      tbl
    Output
      
      
      |**Characteristic** | **Beta** | **95% CI**  | **p-value** |
      |:------------------|:--------:|:-----------:|:-----------:|
      |Age                |   0.00   | -0.01, 0.01 |    >0.9     |

---

    Code
      with_gtsummary_theme(x = theme_gtsummary_journal("qjecon"), lm(age ~ marker +
        response, data = trial) %>% tbl_regression() %>% as_kable(format = "pipe"))
    Output
      
      
      |**Characteristic**   | **Beta**  **(SE)** |
      |:--------------------|:------------------:|
      |Marker Level (ng/mL) |    0.03  (1.29)    |
      |Tumor Response       |    3.9  (2.40)     |

# tbl_uvregression

    Code
      tbl
    Output
      
      
      |**Characteristic**     | **N** | **Beta** | **95% CI**  | **p-value** |
      |:----------------------|:-----:|:--------:|:-----------:|:-----------:|
      |Chemotherapy Treatment |  189  |          |             |             |
      |Drug A                 |       |    —     |      —      |             |
      |Drug B                 |       |   0.44   |  -3.7, 4.6  |     0.8     |
      |Marker Level (ng/mL)   |  179  |  -0.05   |  -2.5, 2.4  |    >0.9     |
      |T Stage                |  189  |          |             |             |
      |T1                     |       |    —     |      —      |             |
      |T2                     |       |   1.3    |  -4.2, 6.9  |     0.6     |
      |T3                     |       |   2.6    |  -3.3, 8.6  |     0.4     |
      |T4                     |       |   -2.0   |  -7.8, 3.8  |     0.5     |
      |Grade                  |  189  |          |             |             |
      |I                      |       |    —     |      —      |             |
      |II                     |       |   1.4    |  -3.6, 6.4  |     0.6     |
      |III                    |       |   2.0    |  -3.1, 7.0  |     0.4     |
      |Tumor Response         |  183  |   3.8    | -0.66, 8.3  |    0.094    |
      |Patient Died           |  189  |   2.2    |  -2.0, 6.3  |     0.3     |
      |Months to Death/Censor |  189  |  -0.14   | -0.54, 0.26 |     0.5     |

# tbl_survfit

    Code
      tbl
    Output
      
      
      |**Characteristic**     |   12 Months    |   24 Months    |
      |:----------------------|:--------------:|:--------------:|
      |Chemotherapy Treatment |                |                |
      |Drug A                 | 91% (85%, 97%) | 47% (38%, 58%) |
      |Drug B                 | 86% (80%, 93%) | 41% (33%, 52%) |

# tbl_merge/tbl_stack

    Code
      tbl
    Output
      
      
      |**Characteristic**     | **OR** | **95% CI** | **p-value** | **HR** | **95% CI** | **p-value** |
      |:----------------------|:------:|:----------:|:-----------:|:------:|:----------:|:-----------:|
      |Chemotherapy Treatment |        |            |             |        |            |             |
      |Drug A                 |   —    |     —      |             |   —    |     —      |             |
      |Drug B                 |  1.13  | 0.60, 2.13 |     0.7     |  1.30  | 0.88, 1.92 |     0.2     |
      |Grade                  |        |            |             |        |            |             |
      |I                      |   —    |     —      |             |   —    |     —      |             |
      |II                     |  0.85  | 0.39, 1.85 |     0.7     |  1.21  | 0.73, 1.99 |     0.5     |
      |III                    |  1.01  | 0.47, 2.15 |    >0.9     |  1.79  | 1.12, 2.86 |    0.014    |
      |Age                    |  1.02  | 1.00, 1.04 |    0.10     |  1.01  | 0.99, 1.02 |     0.3     |

---

    Code
      tbl
    Output
      
      
      |**Group**          |**Characteristic**     | **OR** | **95% CI** | **p-value** |
      |:------------------|:----------------------|:------:|:----------:|:-----------:|
      |**Tumor Response** |Chemotherapy Treatment |        |            |             |
      |                   |Drug A                 |   —    |     —      |             |
      |                   |Drug B                 |  1.13  | 0.60, 2.13 |     0.7     |
      |                   |Grade                  |        |            |             |
      |                   |I                      |   —    |     —      |             |
      |                   |II                     |  0.85  | 0.39, 1.85 |     0.7     |
      |                   |III                    |  1.01  | 0.47, 2.15 |    >0.9     |
      |                   |Age                    |  1.02  | 1.00, 1.04 |    0.10     |
      |**Time to Death**  |Chemotherapy Treatment |        |            |             |
      |                   |Drug A                 |   —    |     —      |             |
      |                   |Drug B                 |  1.30  | 0.88, 1.92 |     0.2     |
      |                   |Grade                  |        |            |             |
      |                   |I                      |   —    |     —      |             |
      |                   |II                     |  1.21  | 0.73, 1.99 |     0.5     |
      |                   |III                    |  1.79  | 1.12, 2.86 |    0.014    |
      |                   |Age                    |  1.01  | 0.99, 1.02 |     0.3     |

