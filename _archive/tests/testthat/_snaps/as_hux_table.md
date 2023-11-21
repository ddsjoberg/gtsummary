# tbl_summary

    Code
      tbl
    Output
                         Characteristic                N = 200       
                       ──────────────────────────────────────────────
                         Chemotherapy Treatment                      
                         Drug A                       98 (49%)       
                         Drug B                       102 (51%)      
                         Age                         47 (38, 57)     
                         Unknown                         11          
                         Marker Level (ng/mL)     0.64 (0.22, 1.39)  
                         Unknown                         10          
                         T Stage                                     
                         T1                           53 (27%)       
                         T2                           54 (27%)       
                         T3                           43 (22%)       
                         T4                           50 (25%)       
                         Grade                                       
                         I                            68 (34%)       
                         II                           68 (34%)       
                         III                          64 (32%)       
                         Tumor Response               61 (32%)       
                         Unknown                          7          
                         Patient Died                 112 (56%)      
                         Months to Death/Censor   22.4 (16.0, 24.0)  
                       ──────────────────────────────────────────────
                         n (%); Median (IQR)                         
      
      Column names: label, stat_0

---

    Code
      tbl
    Output
                            Characteristic             N = 200    
                          ────────────────────────────────────────
                            Chemotherapy Treatment                
                            Drug A                    98 (49%)    
                            Drug B                    102 (51%)   
                            Age                      47 (38, 57)  
                            Unknown                      11       
                          ────────────────────────────────────────
                            n (%); Median (IQR)                   
                            test footnote                         
      
      Column names: label, stat_0

---

    Code
      tbl
    Output
                                Characteristic    N = 5,000   
                              ────────────────────────────────
                                age              47 (38, 57)  
                                Unknown              275      
                                trt                           
                                Drug A           2,450 (49%)  
                                Drug B           2,550 (51%)  
                              ────────────────────────────────
                                Median (IQR); n (%)           
      
      Column names: label, stat_0

# tbl_cross

    Code
      tbl
    Output
                                            Grade                  
                                           I    II    III   Total  
                         ──────────────────────────────────────────
                           Chemotherapy                            
                           Treatment                               
                           Drug A         35    32    31     98    
                           Drug B         33    36    33     102   
                           Total          68    68    64     200   
      
      Column names: label, stat_1, stat_2, stat_3, stat_0

# tbl_regression

    Code
      tbl
    Output
                       Characteristic   Beta     95% CI      p-value  
                     ─────────────────────────────────────────────────
                       Age              0.00   -0.01, 0.01    >0.9    
                     ─────────────────────────────────────────────────
                       CI = Confidence Interval                       
      
      Column names: label, estimate, ci, p.value

# tbl_uvregression

    Code
      tbl
    Output
                     Characterist    N    Beta      95% CI      p-value  
                     ic                                                  
                   ──────────────────────────────────────────────────────
                     Chemotherapy   189                                  
                     Treatment                                           
                     Drug A                 —          —                 
                     Drug B               0.44     -3.7, 4.6      0.8    
                     Marker Level   179   -0.05    -2.5, 2.4     >0.9    
                     (ng/mL)                                             
                     T Stage        189                                  
                     T1                     —          —                 
                     T2                    1.3     -4.2, 6.9      0.6    
                     T3                    2.6     -3.3, 8.6      0.4    
                     T4                   -2.0     -7.8, 3.8      0.5    
                     Grade          189                                  
                     I                      —          —                 
                     II                    1.4     -3.6, 6.4      0.6    
                     III                   2.0     -3.1, 7.0      0.4    
                     Tumor          183    3.8    -0.66, 8.3     0.094   
                     Response                                            
                     Patient Died   189    2.2     -2.0, 6.3      0.3    
                     Months to      189   -0.14   -0.54, 0.26     0.5    
                     Death/Censor                                        
                   ──────────────────────────────────────────────────────
                     CI = Confidence Interval                            
      
      Column names: label, stat_n, estimate, ci, p.value

# tbl_survfit

    Code
      tbl
    Output
                  Characteristic             12 Months        24 Months     
                ────────────────────────────────────────────────────────────
                  Chemotherapy Treatment                                    
                  Drug A                   91% (85%, 97%)   47% (38%, 58%)  
                  Drug B                   86% (80%, 93%)   41% (33%, 52%)  
      
      Column names: label, stat_1, stat_2

# tbl_merge/tbl_stack

    Code
      tbl
    Output
                           Tumor Response               Time to Death           
              Characte    OR     95% CI    p-value    HR     95% CI    p-value  
              ristic                                                            
            ────────────────────────────────────────────────────────────────────
              Chemothe                                                          
              rapy                                                              
              Treatmen                                                          
              t                                                                 
              Drug A      —        —                  —        —                
              Drug B     1.13    0.60,       0.7     1.30    0.88,       0.2    
                                  2.13                        1.92              
              Grade                                                             
              I           —        —                  —        —                
              II         0.85    0.39,       0.7     1.21    0.73,       0.5    
                                  1.85                        1.99              
              III        1.01    0.47,      >0.9     1.79    1.12,      0.014   
                                  2.15                        2.86              
              Age        1.02    1.00,      0.10     1.01    0.99,       0.3    
                                  1.04                        1.02              
            ────────────────────────────────────────────────────────────────────
              OR = Odds Ratio, CI = Confidence Interval, HR                     
              = Hazard Ratio                                                    
      
      Column names: label, estimate_1, ci_1, p.value_1, estimate_2, ci_2, p.value_2

---

    Code
      tbl
    Output
                 Group          Characterist    OR      95% CI     p-value  
                                ic                                          
               ─────────────────────────────────────────────────────────────
                 **Tumor        Chemotherapy                                
                 Response**     Treatment                                   
                                Drug A          —         —                 
                                Drug B         1.13   0.60, 2.13     0.7    
                                Grade                                       
                                I               —         —                 
                                II             0.85   0.39, 1.85     0.7    
                                III            1.01   0.47, 2.15    >0.9    
                                Age            1.02   1.00, 1.04    0.10    
                 **Time to      Chemotherapy                                
                 Death**        Treatment                                   
                                Drug A          —         —                 
                                Drug B         1.30   0.88, 1.92     0.2    
                                Grade                                       
                                I               —         —                 
                                II             1.21   0.73, 1.99     0.5    
                                III            1.79   1.12, 2.86    0.014   
                                Age            1.01   0.99, 1.02     0.3    
               ─────────────────────────────────────────────────────────────
                 OR = Odds Ratio, CI = Confidence Interval                  
      
      Column names: groupname_col, label, estimate, ci, p.value

