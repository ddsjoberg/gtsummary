# show_header_names() works

    Code
      show_header_names(tbl_summary(trial, include = age, by = trt, missing = "no"))
    Output
      
    Message
      i As a usage guide, the code below re-creates the current column headers.
      modify_header(
        label = '**Characteristic**',
        stat_1 = '**Drug A**  
      N = 98',
        stat_2 = '**Drug B**  
      N = 102'
      )
    Output
      
      
      Column Name   Column Header       
      ------------  --------------------
      label         **Characteristic**  
      stat_1        **Drug A**  
      N = 98  
      stat_2        **Drug B**  
      N = 102 

