# tbl_custom_summary() basics

    Code
      tbl1
    Output
      # A tibble: 8 x 8
        variable var_type    var_label            row_type label  stat_1 stat_2 stat_0
        <chr>    <chr>       <chr>                <chr>    <chr>  <chr>  <chr>  <chr> 
      1 grade    categorical Grade                label    Grade  <NA>   <NA>   <NA>  
      2 grade    categorical Grade                level    I      45.9   46.4   46.2  
      3 grade    categorical Grade                level    II     44.6   50.3   47.5  
      4 grade    categorical Grade                level    III    51.0   45.7   48.1  
      5 response dichotomous Tumor Response       label    Tumor~ 50.1   49.5   49.8  
      6 response dichotomous Tumor Response       missing  Unkno~ 3      4      7     
      7 marker   continuous  Marker Level (ng/mL) label    Marke~ 46.5   47.5   47.0  
      8 marker   continuous  Marker Level (ng/mL) missing  Unkno~ 6      4      10    

---

    Code
      tbl
    Output
      # A tibble: 10 x 5
         label                   n     stat_0                stat_1             stat_2
         <chr>                   <chr> <chr>                 <chr>              <chr> 
       1 __Grade__               200   <NA>                  <NA>               <NA>  
       2 _I_                     <NA>  1.1 (high, diff: 0.2) 1.2 (high, diff: ~ 1.0 (~
       3 _II_                    <NA>  0.7 (low, diff: -0.2) 0.9 (low, diff: -~ 0.5 (~
       4 _III_                   <NA>  1.0 (high, diff: 0.1) 1.0 (high, diff: ~ 1.0 (~
       5 __T Stage__             200   <NA>                  <NA>               <NA>  
       6 _T1_                    <NA>  0.7 (low, diff: -0.2) 0.7 (low, diff: -~ 0.7 (~
       7 _T2_                    <NA>  1.1 (high, diff: 0.2) 1.2 (high, diff: ~ 1.0 (~
       8 _T3_                    <NA>  1.0 (high, diff: 0.1) 1.1 (high, diff: ~ 0.9 (~
       9 _T4_                    <NA>  0.9 (low, diff: -0.1) 1.1 (high, diff: ~ 0.7 (~
      10 __All grades & stages__ 200   0.9 (low, diff: 0.0)  1.0 (high, diff: ~ 0.8 (~

---

    Code
      tbl
    Output
      # A tibble: 3 x 3
        label                  stat_1            stat_2           
        <chr>                  <chr>             <chr>            
      1 Marker Level (ng/mL)   1.02 [0.83; 1.20] 0.82 [0.65; 0.99]
      2 Unknown                6                 4                
      3 Months to Death/Censor 20.2 [19.2; 21.2] 19.0 [18.0; 20.1]

# tbl_custom_summary() manage factor levels with no observation

    Code
      tbl
    Output
      # A tibble: 6 x 3
        label   stat_1 stat_2
        <chr>   <chr>  <chr> 
      1 Overall 46     48    
      2 Grade   <NA>   <NA>  
      3 I       46     48    
      4 II      44     50    
      5 III     52     45    
      6 IV      NA     NA    

# tbl_custom_summary() helpers work as expected

    Code
      tbl
    Output
      # A tibble: 5 x 3
        label   stat_1                     stat_2                     
        <chr>   <chr>                      <chr>                      
      1 T Stage <NA>                       <NA>                       
      2 T1      0.012 [0.00; 0.02] (7/583) 0.021 [0.01; 0.04] (11/522)
      3 T2      0.011 [0.00; 0.02] (6/528) 0.012 [0.01; 0.03] (7/560) 
      4 T3      0.019 [0.01; 0.04] (8/426) 0.016 [0.01; 0.03] (7/425) 
      5 T4      0.016 [0.01; 0.03] (7/445) 0.018 [0.01; 0.04] (8/434) 

---

    Code
      tbl
    Output
      # A tibble: 4 x 3
        label stat_1     stat_2    
        <chr> <chr>      <chr>     
      1 Grade <NA>       <NA>      
      2 I     46 [36-60] 48 [42-55]
      3 II    44 [31-54] 50 [43-57]
      4 III   52 [42-60] 45 [36-52]

---

    Code
      tbl
    Output
      # A tibble: 8 x 3
        label stat_1                    stat_2                 
        <chr> <chr>                     <chr>                  
      1 Age   <NA>                      <NA>                   
      2 Child 45.3% (29/64) [33-58]     62.2% (28/45) [47-76]  
      3 Adult 20.3% (338/1,667) [18-22] 74.4% (316/425) [70-78]
      4 Class <NA>                      <NA>                   
      5 1st   34.4% (62/180) [28-42]    97.2% (141/145) [93-99]
      6 2nd   14.0% (25/179) [9.4-20]   87.7% (93/106) [80-93] 
      7 3rd   17.3% (88/510) [14-21]    45.9% (90/196) [39-53] 
      8 Crew  22.3% (192/862) [20-25]   87.0% (20/23) [65-97]  

# character summaries do not cause error

    Code
      trial %>% tbl_custom_summary(include = c("stage"), by = "trt", stat_fns = ~
      diff_to_great_mean, statistic = ~"{mean} ({level}) [{date}]") %>%
        render_as_html()
    Output
      <div  style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        
        <table class="gt_table" style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif; display: table; border-collapse: collapse; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;" bgcolor="#FFFFFF">
        
        <thead class="gt_col_headings" style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;">
          <tr>
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;" style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left"><strong>Characteristic</strong></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug A&lt;/strong&gt;, N = 98&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;" style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;" bgcolor="#FFFFFF" valign="bottom" align="center"><strong>Drug A</strong>, N = 98<sup class="gt_footnote_marks" style="font-style: italic; font-weight: normal; font-size: 75%; vertical-align: 0.4em;">1</sup></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug B&lt;/strong&gt;, N = 102&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;" style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;" bgcolor="#FFFFFF" valign="bottom" align="center"><strong>Drug B</strong>, N = 102<sup class="gt_footnote_marks" style="font-style: italic; font-weight: normal; font-size: 75%; vertical-align: 0.4em;">1</sup></th>
          </tr>
        </thead>
        <tbody class="gt_table_body" style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;">
          <tr><td headers="label" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">T Stage</td>
      <td headers="stat_1" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center"></td>
      <td headers="stat_2" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center"></td></tr>
          <tr><td headers="label" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">    T1</td>
      <td headers="stat_1" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center">0.74 (low) [2023-01-01]</td>
      <td headers="stat_2" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center">0.66 (low) [2023-01-01]</td></tr>
          <tr><td headers="label" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">    T2</td>
      <td headers="stat_1" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center">1.2 (high) [2023-01-01]</td>
      <td headers="stat_2" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center">1.0 (high) [2023-01-01]</td></tr>
          <tr><td headers="label" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">    T3</td>
      <td headers="stat_1" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center">1.1 (high) [2023-01-01]</td>
      <td headers="stat_2" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center">0.94 (high) [2023-01-01]</td></tr>
          <tr><td headers="label" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">    T4</td>
      <td headers="stat_1" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center">1.1 (high) [2023-01-01]</td>
      <td headers="stat_2" class="gt_row gt_center" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;" valign="middle" align="center">0.67 (low) [2023-01-01]</td></tr>
        </tbody>
        
        <tfoot class="gt_footnotes" style="color: #333333; background-color: #FFFFFF; border-bottom-style: none; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3;" bgcolor="#FFFFFF">
          <tr>
            <td class="gt_footnote" colspan="3" style="margin: 0px; font-size: 90%; padding-left: 4px; padding-right: 4px; padding-left: 5px; padding-right: 5px;"><sup class="gt_footnote_marks" style="font-style: italic; font-weight: normal; font-size: 75%; vertical-align: 0.4em;">1</sup> Mean (level) [date]</td>
          </tr>
        </tfoot>
      </table>
      </div>

# full_data contains all observations including missing values

    Code
      res
    Output
      # A tibble: 2 x 2
        `**Characteristic**` `**N = 200**`
        <chr>                <chr>        
      1 Age                  189/200      
      2 Unknown              11           

