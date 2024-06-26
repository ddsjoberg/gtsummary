# as_kable works with standard use

    Code
      kbl_summary
    Output
      
      
      |**Characteristic**     | **N = 200** |
      |:----------------------|:-----------:|
      |Chemotherapy Treatment |             |
      |Drug A                 |  98 (49%)   |
      |Drug B                 |  102 (51%)  |
      |Age                    | 47 (38, 57) |
      |Unknown                |     11      |
      |Patient Died           |  112 (56%)  |

# as_kable works with tbl_cross

    Code
      kbl_cross
    Output
      
      
      |      | Drug A | Drug B | Total |
      |:-----|:------:|:------:|:-----:|
      |Grade |        |        |       |
      |I     |   35   |   33   |  68   |
      |II    |   32   |   36   |  68   |
      |III   |   31   |   33   |  64   |
      |Total |   98   |  102   |  200  |

# as_kable works with tbl_uvregression

    Code
      kbl_uvregression
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

# as_kable works with tbl_survfit

    Code
      kbl_survfit
    Output
      
      
      |**Characteristic**     |   12 Months    |   24 Months    |
      |:----------------------|:--------------:|:--------------:|
      |Chemotherapy Treatment |                |                |
      |Drug A                 | 91% (85%, 97%) | 47% (38%, 58%) |
      |Drug B                 | 86% (80%, 93%) | 41% (33%, 52%) |

# as_kable works with tbl_merge

    Code
      kbl_merge
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

# as_kable works with tbl_stack

    Code
      kbl_stack
    Output
      
      
      |**Group** |**Characteristic** | **Statistic** |
      |:---------|:------------------|:-------------:|
      |Drug A    |Age                |  46 (37, 60)  |
      |          |Unknown            |       7       |
      |          |Tumor Response     |   28 (29%)    |
      |          |Unknown            |       3       |
      |          |Patient Died       |   52 (53%)    |
      |Drug B    |Age                |  48 (39, 56)  |
      |          |Unknown            |       4       |
      |          |Tumor Response     |   33 (34%)    |
      |          |Unknown            |       4       |
      |          |Patient Died       |   60 (59%)    |

