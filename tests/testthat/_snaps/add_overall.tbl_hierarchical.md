# add_overall.tbl_hierarchical() works

    Code
      as.data.frame(add_overall(tbl))
    Output
         **Primary System Organ Class**  \n    **Reported Term for the Adverse Event** **Overall**  \nN = 254 **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                                              Any Adverse Event            108 (42.5%)            26 (30.2%)                         42 (50.0%)                        40 (47.6%)
      2                                                              CARDIAC DISORDERS               5 (2.0%)              2 (2.3%)                           3 (3.6%)                          0 (0.0%)
      3                                           ATRIOVENTRICULAR BLOCK SECOND DEGREE               5 (2.0%)              2 (2.3%)                           3 (3.6%)                          0 (0.0%)
      4                                                     GASTROINTESTINAL DISORDERS              18 (7.1%)             9 (10.5%)                           4 (4.8%)                          5 (6.0%)
      5                                                                      DIARRHOEA              18 (7.1%)             9 (10.5%)                           4 (4.8%)                          5 (6.0%)
      6                           GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS             57 (22.4%)              8 (9.3%)                         25 (29.8%)                        24 (28.6%)
      7                                                      APPLICATION SITE ERYTHEMA             30 (11.8%)              3 (3.5%)                         15 (17.9%)                        12 (14.3%)
      8                                                      APPLICATION SITE PRURITUS             50 (19.7%)              6 (7.0%)                         22 (26.2%)                        22 (26.2%)
      9                                         SKIN AND SUBCUTANEOUS TISSUE DISORDERS             38 (15.0%)             9 (10.5%)                         14 (16.7%)                        15 (17.9%)
      10                                                                      ERYTHEMA             38 (15.0%)             9 (10.5%)                         14 (16.7%)                        15 (17.9%)

# add_overall.tbl_hierarchical_count() works

    Code
      as.data.frame(add_overall(tbl))
    Output
         **Primary System Organ Class**  \n    **Reported Term for the Adverse Event**  \n        **Severity/Intensity** **Overall** **Placebo** **Xanomeline High Dose** **Xanomeline Low Dose**
      1                                                                                              Total Number of AEs         210          38                       88                      84
      2                                                                                                CARDIAC DISORDERS           6           2                        4                       0
      3                                                                             ATRIOVENTRICULAR BLOCK SECOND DEGREE           6           2                        4                       0
      4                                                                                                             MILD           3           1                        2                       0
      5                                                                                                         MODERATE           2           0                        2                       0
      6                                                                                                           SEVERE           1           1                        0                       0
      7                                                                                       GASTROINTESTINAL DISORDERS          21          10                        4                       7
      8                                                                                                        DIARRHOEA          21          10                        4                       7
      9                                                                                                             MILD          19          10                        2                       7
      10                                                                                                        MODERATE           2           0                        2                       0
      11                                                            GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS         124          13                       58                      53
      12                                                                                       APPLICATION SITE ERYTHEMA          46           3                       23                      20
      13                                                                                                            MILD          28           3                       16                       9
      14                                                                                                        MODERATE          15           0                        7                       8
      15                                                                                                          SEVERE           3           0                        0                       3
      16                                                                                       APPLICATION SITE PRURITUS          78          10                       35                      33
      17                                                                                                            MILD          50           8                       21                      21
      18                                                                                                        MODERATE          27           2                       14                      11
      19                                                                                                          SEVERE           1           0                        0                       1
      20                                                                          SKIN AND SUBCUTANEOUS TISSUE DISORDERS          59          13                       22                      24
      21                                                                                                        ERYTHEMA          59          13                       22                      24
      22                                                                                                            MILD          36           8                       16                      12
      23                                                                                                        MODERATE          23           5                        6                      12

