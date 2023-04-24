# setting themes

    Code
      tbl %>% as.data.frame()
    Output
            **Caractéristique** **Drug A**, N = 98 **Drug B**, N = 102 **p-valeur**
      1                     Age               <NA>                <NA>         0,72
      2            Médiane (EI)       46 (37 – 59)        48 (39 – 56)         <NA>
      3                Manquant                  7                   4         <NA>
      4    Marker Level (ng/mL)               <NA>                <NA>        0,085
      5            Médiane (EI) 0,84 (0,24 – 1,57)  0,52 (0,19 – 1,20)         <NA>
      6                Manquant                  6                   4         <NA>
      7          T Stage, n (%)               <NA>                <NA>         0,87
      8                      T1            28 (29)             25 (25)         <NA>
      9                      T2            25 (26)             29 (28)         <NA>
      10                     T3            22 (22)             21 (21)         <NA>
      11                     T4            23 (23)             27 (26)         <NA>
      12           Grade, n (%)               <NA>                <NA>         0,87
      13                      I            35 (36)             33 (32)         <NA>
      14                     II            32 (33)             36 (35)         <NA>
      15                    III            31 (32)             33 (32)         <NA>
      16  Tumor Response, n (%)            28 (29)             33 (34)         0,53
      17               Manquant                  3                   4         <NA>
      18    Patient Died, n (%)            52 (53)             60 (59)         0,41
      19 Months to Death/Censor               <NA>                <NA>         0,14
      20           Médiane (EI) 23,5 (17,4 – 24,0)  21,2 (14,6 – 24,0)         <NA>

---

    Code
      lm(age ~ marker + grade, trial) %>% tbl_regression() %>% as.data.frame()
    Output
         **Caractéristique** **Beta** **(95% CI)** **p-valeur**
      1 Marker Level (ng/mL)   -0,04 (-2,6 to 2,5)         0,98
      2                Grade                  <NA>         <NA>
      3                    I                  <NA>         <NA>
      4                   II    0,64 (-4,7 to 6,0)         0,81
      5                  III     2,4 (-2,8 to 7,6)         0,37

---

    Code
      tbl_theme %>% as.data.frame()
    Output
            **Characteristic**     **N = 200**
      1 Chemotherapy Treatment            <NA>
      2                 Drug A  98 / 200 (49%)
      3                 Drug B 102 / 200 (51%)
      4                    Age            <NA>
      5           Median (IQR)    47 (38 - 57)
      6              Mean (SD)         47 (14)
      7                Unknown              11

---

    Code
      tbl1
    Output
      # A tibble: 2 x 5
        label                 stat_1       stat_2       estimate             p.value
        <chr>                 <chr>        <chr>        <chr>                <chr>  
      1 Age, Median (IQR)     46 (37 – 59) 48 (39 – 56) -0.44 (-4.6 to 3.7)  0.83   
      2 Tumor Response, n (%) 28 (29)      33 (34)      -4.2% (-18% to 9.9%) 0.64   

---

    Code
      tbl_qjecon1 %>% as.data.frame()
    Output
        **Characteristic** **Beta**  \n**(SE)**
      1              Grade                 <NA>
      2                  I                 <NA>
      3                 II        1.4  \n(2.54)
      4                III        2.0  \n(2.55)

---

    Code
      tbl_qjecon2 %>% as.data.frame()
    Output
         **Characteristic** **N** **Beta**  \n**(SE)**
      1                 mpg    32    -8.8***  \n(1.31)
      2                 cyl    32      32***  \n(3.88)
      3                disp    32   0.44***  \n(0.062)
      4                drat    32      -58**  \n(20.9)
      5                  wt    32      46***  \n(9.63)
      6                qsec    32     -27***  \n(4.95)
      7                  vs    32     -98***  \n(17.2)
      8                  am    32        -33  \n(24.3)
      9                gear    32        -12  \n(16.8)
      10               carb    32      32***  \n(5.13)

---

    Code
      tbl
    Output
      # A tibble: 5 x 2
        label        stat_0     
        <chr>        <chr>      
      1 Age          <NA>       
      2 Median (IQR) 47 (38, 57)
      3 Mean (SD)    47 (14)    
      4 Range        6, 83      
      5 Unknown      11         

---

    Code
      with_gtsummary_theme(theme_gtsummary_journal("lancet"), lm(mpg ~ factor(cyl) +
        hp + am, mtcars) %>% tbl_regression() %>% as_tibble())
    Output
      # A tibble: 6 x 4
        `**Characteristic**` `**Beta**` `**95% CI**`   `**p-value**`
        <chr>                <chr>      <chr>          <chr>        
      1 factor(cyl)          <NA>       <NA>           <NA>         
      2 4                    <NA>       <NA>           <NA>         
      3 6                    -3·9       -7·1 to -0·77  0·017        
      4 8                    -3·5       -8·7 to 1·6    0·17         
      5 hp                   -0·04      -0·07 to -0·01 0·0053       
      6 am                   4·2        1·6 to 6·7     0·0027       

