# as_hux_table works with tbl_merge

    Code
      ht_merge
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
      
      Column names: label, estimate_1, conf.low_1, p.value_1, estimate_2, conf.low_2,
      p.value_2

# as_hux_table works with tbl_stack

    Code
      ht_stack
    Output
                           Group    Characteristic    Statistic   
                         ─────────────────────────────────────────
                           Drug A   Age              46 (37, 60)  
                                    Unknown               7       
                                    Tumor Response    28 (29%)    
                                    Unknown               3       
                                    Patient Died      52 (53%)    
                           Drug B   Age              48 (39, 56)  
                                    Unknown               4       
                                    Tumor Response    33 (34%)    
                                    Unknown               4       
                                    Patient Died      60 (59%)    
                         ─────────────────────────────────────────
                           Median (Q1, Q3); n (%)                 
      
      Column names: groupname_col, label, stat_0

