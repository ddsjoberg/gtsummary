# as_kable_extra(format) works as expected

    Code
      kbl_html
    Output
      [[1]]
       [1] "<table style=\"NAborder-bottom: 0;\">\n <thead>\n "                                                                                                
       [2] "<th style=\"text-align:left;\"> Characteristic </th>\n   <th style=\"text-align:center;\"> N = 200 </th>\n "                                       
       [3] "</thead>\n<tbody>\n "                                                                                                                              
       [4] "<td style=\"text-align:left;\"> Chemotherapy Treatment </td>\n   <td style=\"text-align:center;\">  </td>\n "                                      
       [5] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> Drug A </td>\n   <td style=\"text-align:center;\"> 98 (49%) </td>\n " 
       [6] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> Drug B </td>\n   <td style=\"text-align:center;\"> 102 (51%) </td>\n "
       [7] "<tr>\n   <td style=\"text-align:left;\"> Age </td>\n   <td style=\"text-align:center;\"> 47 (38, 57) </td>\n "                                     
       [8] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> Unknown </td>\n   <td style=\"text-align:center;\"> 11 </td>\n "      
       [9] "<tr>\n   <td style=\"text-align:left;\"> Patient Died </td>\n   <td style=\"text-align:center;\"> 112 (56%) </td>\n "                              
      [10] "</tbody>\n<tfoot><tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup>1</sup> n (%); Median (Q1, Q3)</td></tr></tfoot>\n</table>"                
      

---

    Code
      kbl_latex
    Output
      [[1]]
       [1] ""                                                                                      
       [2] "\\begin{tabular}{l|c}"                                                                 
       [3] "\\hline"                                                                               
       [4] "\\textbf{Characteristic} & \\textbf{N = 200}\\\\"                                      
       [5] "\\hline"                                                                               
       [6] "Chemotherapy Treatment & \\\\"                                                         
       [7] "\\hline"                                                                               
       [8] "\\hspace{1em}Drug A & 98 (49\\%)\\\\"                                                  
       [9] "\\hline"                                                                               
      [10] "\\hspace{1em}Drug B & 102 (51\\%)\\\\"                                                 
      [11] "\\hline"                                                                               
      [12] "Age & 47 (38, 57)\\\\"                                                                 
      [13] "\\hline"                                                                               
      [14] "\\hspace{1em}Unknown & 11\\\\"                                                         
      [15] "\\hline"                                                                               
      [16] "Patient Died & 112 (56\\%)\\\\"                                                        
      [17] "\\hline"                                                                               
      [18] "\\multicolumn{2}{l}{\\rule{0pt}{1em}\\textsuperscript{1} n (\\%); Median (Q1, Q3)}\\\\"
      [19] "\\end{tabular}"                                                                        
      

---

    Code
      kbl_simple
    Output
      
      
      Characteristic              N = 200   
      -----------------------  -------------
      Chemotherapy Treatment                
      Drug A                     98 (49%)   
      Drug B                     102 (51%)  
      Age                       47 (38, 57) 
      Unknown                       11      
      Patient Died               112 (56%)  

# as_kable_extra works with tbl_merge

    Code
      kbl_merge
    Output
       [1] "<table style=\"NAborder-bottom: 0;\">\n <thead>\n<tr>\n<th style=\"empty-cells: hide;border-bottom:hidden;\" colspan=\"1\"></th>\n<th style=\"border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; \" colspan=\"3\"><div style=\"border-bottom: 1px solid #ddd; padding-bottom: 5px; \">Tumor Response</div></th>\n<th style=\"border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; \" colspan=\"3\"><div style=\"border-bottom: 1px solid #ddd; padding-bottom: 5px; \">Time to Death</div></th>\n</tr>\n "
       [2] "<th style=\"text-align:left;\"> Characteristic </th>\n   <th style=\"text-align:center;\"> OR </th>\n   <th style=\"text-align:center;\"> 95% CI </th>\n   <th style=\"text-align:center;\"> p-value </th>\n   <th style=\"text-align:center;\"> HR </th>\n   <th style=\"text-align:center;\"> 95% CI </th>\n   <th style=\"text-align:center;\"> p-value </th>\n "                                                                                                                                                                                                                          
       [3] "</thead>\n<tbody>\n "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
       [4] "<td style=\"text-align:left;\"> Chemotherapy Treatment </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n "                                                                                                                                                                                                                                                
       [5] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> Drug A </td>\n   <td style=\"text-align:center;\"> — </td>\n   <td style=\"text-align:center;\"> — </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\"> — </td>\n   <td style=\"text-align:center;\"> — </td>\n   <td style=\"text-align:center;\">  </td>\n "                                                                                                                                                                                                               
       [6] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> Drug B </td>\n   <td style=\"text-align:center;\"> 1.13 </td>\n   <td style=\"text-align:center;\"> 0.60, 2.13 </td>\n   <td style=\"text-align:center;\"> 0.7 </td>\n   <td style=\"text-align:center;\"> 1.30 </td>\n   <td style=\"text-align:center;\"> 0.88, 1.92 </td>\n   <td style=\"text-align:center;\"> 0.2 </td>\n "                                                                                                                                                                                 
       [7] "<tr>\n   <td style=\"text-align:left;\"> Grade </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\">  </td>\n "                                                                                                                                                                                                                                                        
       [8] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> I </td>\n   <td style=\"text-align:center;\"> — </td>\n   <td style=\"text-align:center;\"> — </td>\n   <td style=\"text-align:center;\">  </td>\n   <td style=\"text-align:center;\"> — </td>\n   <td style=\"text-align:center;\"> — </td>\n   <td style=\"text-align:center;\">  </td>\n "                                                                                                                                                                                                                    
       [9] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> II </td>\n   <td style=\"text-align:center;\"> 0.85 </td>\n   <td style=\"text-align:center;\"> 0.39, 1.85 </td>\n   <td style=\"text-align:center;\"> 0.7 </td>\n   <td style=\"text-align:center;\"> 1.21 </td>\n   <td style=\"text-align:center;\"> 0.73, 1.99 </td>\n   <td style=\"text-align:center;\"> 0.5 </td>\n "                                                                                                                                                                                     
      [10] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> III </td>\n   <td style=\"text-align:center;\"> 1.01 </td>\n   <td style=\"text-align:center;\"> 0.47, 2.15 </td>\n   <td style=\"text-align:center;\"> &gt;0.9 </td>\n   <td style=\"text-align:center;\"> 1.79 </td>\n   <td style=\"text-align:center;\"> 1.12, 2.86 </td>\n   <td style=\"text-align:center;\"> 0.014 </td>\n "                                                                                                                                                                              
      [11] "<tr>\n   <td style=\"text-align:left;\"> Age </td>\n   <td style=\"text-align:center;\"> 1.02 </td>\n   <td style=\"text-align:center;\"> 1.00, 1.04 </td>\n   <td style=\"text-align:center;\"> 0.10 </td>\n   <td style=\"text-align:center;\"> 1.01 </td>\n   <td style=\"text-align:center;\"> 0.99, 1.02 </td>\n   <td style=\"text-align:center;\"> 0.3 </td>\n "                                                                                                                                                                                                                       
      [12] "</tbody>\n<tfoot><tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup></sup> Abbreviations: CI = Confidence Interval, HR = Hazard Ratio, OR = Odds Ratio</td></tr></tfoot>\n</table>"                                                                                                                                                                                                                                                                                                                                                                                                       

# as_kable_extra works with tbl_stack

    Code
      kbl_stack
    Output
       [1] "<table style=\"NAborder-bottom: 0;\">\n <thead>\n "                                                                                                                                    
       [2] "<th style=\"text-align:left;\"> Group </th>\n   <th style=\"text-align:left;\"> Characteristic </th>\n   <th style=\"text-align:center;\"> Statistic </th>\n "                         
       [3] "</thead>\n<tbody>\n "                                                                                                                                                                  
       [4] "<td style=\"text-align:left;\"> Drug A </td>\n   <td style=\"text-align:left;\"> Age </td>\n   <td style=\"text-align:center;\"> 46 (37, 60) </td>\n "                                 
       [5] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\">  </td>\n   <td style=\"text-align:left;\"> Unknown </td>\n   <td style=\"text-align:center;\"> 7 </td>\n "
       [6] "<tr>\n   <td style=\"text-align:left;\">  </td>\n   <td style=\"text-align:left;\"> Tumor Response </td>\n   <td style=\"text-align:center;\"> 28 (29%) </td>\n "                      
       [7] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\">  </td>\n   <td style=\"text-align:left;\"> Unknown </td>\n   <td style=\"text-align:center;\"> 3 </td>\n "
       [8] "<tr>\n   <td style=\"text-align:left;\">  </td>\n   <td style=\"text-align:left;\"> Patient Died </td>\n   <td style=\"text-align:center;\"> 52 (53%) </td>\n "                        
       [9] "<tr>\n   <td style=\"text-align:left;\"> Drug B </td>\n   <td style=\"text-align:left;\"> Age </td>\n   <td style=\"text-align:center;\"> 48 (39, 56) </td>\n "                        
      [10] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\">  </td>\n   <td style=\"text-align:left;\"> Unknown </td>\n   <td style=\"text-align:center;\"> 4 </td>\n "
      [11] "<tr>\n   <td style=\"text-align:left;\">  </td>\n   <td style=\"text-align:left;\"> Tumor Response </td>\n   <td style=\"text-align:center;\"> 33 (34%) </td>\n "                      
      [12] "<tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\">  </td>\n   <td style=\"text-align:left;\"> Unknown </td>\n   <td style=\"text-align:center;\"> 4 </td>\n "
      [13] "<tr>\n   <td style=\"text-align:left;\">  </td>\n   <td style=\"text-align:left;\"> Patient Died </td>\n   <td style=\"text-align:center;\"> 60 (59%) </td>\n "                        
      [14] "</tbody>\n<tfoot><tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup>1</sup> Median (Q1, Q3); n (%)</td></tr></tfoot>\n</table>"                                                    

# as_kable_extra passes table footnotes & abbreviations correctly

    "<\/tbody>\n<tfoot>\n<tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup>1<\/sup> n (%); Median (Q1, Q3)<\/td><\/tr>\n<tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup>2<\/sup> test footnote<\/td><\/tr>\n<\/tfoot>\n<\/table>"

---

    "<\/tbody>\n<tfoot><tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup><\/sup> Abbreviation: N = number of observations<\/td><\/tr><\/tfoot>\n<tfoot>\n<tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup>1<\/sup> n (%); Median (Q1, Q3)<\/td><\/tr>\n<tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup>2<\/sup> test footnote<\/td><\/tr>\n<\/tfoot>\n<\/table>"

---

    "<\/tbody>\n<tfoot>\n<tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup>1<\/sup> another new footnote<\/td><\/tr>\n<tr><td style=\"padding: 0; \" colspan=\"100%\">\n<sup>2<\/sup> replace old footnote<\/td><\/tr>\n<\/tfoot>\n<\/table>"

# as_kable_extra passes appended glance statistics correctly

    "<tr>\n   <td style=\"text-align:left;\"> R² <\/td>\n   <td style=\"text-align:center;\"> 0.000 <\/td>\n   <td style=\"text-align:center;\">  <\/td>\n   <td style=\"text-align:center;\">  <\/td>\n "

---

    "<tr>\n   <td style=\"text-align:left;\"> BIC <\/td>\n   <td style=\"text-align:center;\"> 471 <\/td>\n   <td style=\"text-align:center;\">  <\/td>\n   <td style=\"text-align:center;\">  <\/td>\n "

# as_kable_extra passes captions correctly

    "<table style=\"NAborder-bottom: 0;\">\n<caption>My table caption<\/caption>\n <thead>\n "

