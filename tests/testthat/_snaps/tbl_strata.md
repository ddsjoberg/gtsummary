# no errors with standard use

    Code
      tbl %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 35 **Drug B**, N = 33 **p-value**
      1                Age        46 (36, 60)        48 (42, 55)         0.6
      2            Unknown                  2                  0        <NA>
      3            T Stage               <NA>               <NA>         0.8
      4                 T1            8 (23%)            9 (27%)        <NA>
      5                 T2            8 (23%)           10 (30%)        <NA>
      6                 T3           11 (31%)            7 (21%)        <NA>
      7                 T4            8 (23%)            7 (21%)        <NA>
        **Drug A**, N = 32 **Drug B**, N = 36 **p-value** **Drug A**, N = 31
      1        45 (31, 55)        51 (43, 57)        0.10        52 (42, 60)
      2                  2                  4        <NA>                  3
      3               <NA>               <NA>         0.3               <NA>
      4           14 (44%)            9 (25%)        <NA>            6 (19%)
      5            8 (25%)            9 (25%)        <NA>            9 (29%)
      6            5 (16%)            6 (17%)        <NA>            6 (19%)
      7            5 (16%)           12 (33%)        <NA>           10 (32%)
        **Drug B**, N = 33 **p-value**
      1        45 (36, 52)        0.10
      2                  0        <NA>
      3               <NA>        >0.9
      4            7 (21%)        <NA>
      5           10 (30%)        <NA>
      6            8 (24%)        <NA>
      7            8 (24%)        <NA>

---

    Code
      trial %>% select(age, grade, stage, trt) %>% mutate(grade = paste("Grade",
        grade), stage = paste("Stage", stage)) %>% tbl_strata(strata = c(grade, stage),
      .tbl_fun = ~ .x %>% tbl_summary(by = trt) %>% add_p(test = all_continuous() ~
        "t.test")) %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 8 **Drug B**, N = 9 **p-value**
      1                Age       40 (34, 49)       48 (43, 66)         0.5
      2            Unknown                 2                 0        <NA>
        **Drug A**, N = 8 **Drug B**, N = 10 **p-value** **Drug A**, N = 11
      1       47 (41, 49)        48 (44, 54)        >0.9        48 (37, 61)
      2              <NA>               <NA>        <NA>               <NA>
        **Drug B**, N = 7 **p-value** **Drug A**, N = 8 **Drug B**, N = 7 **p-value**
      1       53 (42, 60)         0.9       40 (30, 60)       43 (39, 48)        >0.9
      2              <NA>        <NA>              <NA>              <NA>        <NA>
        **Drug A**, N = 14 **Drug B**, N = 9 **p-value** **Drug A**, N = 8
      1        43 (31, 47)       48 (44, 57)       0.071       51 (36, 59)
      2               <NA>              <NA>        <NA>              <NA>
        **Drug B**, N = 9 **p-value** **Drug A**, N = 5 **Drug B**, N = 6 **p-value**
      1       49 (38, 49)         0.5       47 (38, 54)       57 (54, 59)         0.3
      2              <NA>        <NA>                 1                 0        <NA>
        **Drug A**, N = 5 **Drug B**, N = 12 **p-value** **Drug A**, N = 6
      1       46 (29, 62)        53 (38, 60)         0.6       55 (42, 63)
      2                 1                  4        <NA>              <NA>
        **Drug B**, N = 7 **p-value** **Drug A**, N = 9 **Drug B**, N = 10
      1       43 (39, 48)         0.8       53 (43, 66)        50 (37, 53)
      2              <NA>        <NA>                 1                  0
        **p-value** **Drug A**, N = 6 **Drug B**, N = 8 **p-value**
      1         0.3       48 (39, 58)       44 (38, 54)         0.7
      2        <NA>                 1                 0        <NA>
        **Drug A**, N = 10 **Drug B**, N = 8 **p-value**
      1        51 (45, 54)       42 (34, 51)         0.3
      2                  1                 0        <NA>

---

    Code
      tbl %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 35 **Drug B**, N = 33 **Drug A**, N = 32
      1                Age        46 (36, 60)        48 (42, 55)        45 (31, 55)
      2            T Stage               <NA>               <NA>               <NA>
      3                 T1            8 (23%)            9 (27%)           14 (44%)
      4                 T2            8 (23%)           10 (30%)            8 (25%)
      5                 T3           11 (31%)            7 (21%)            5 (16%)
      6                 T4            8 (23%)            7 (21%)            5 (16%)
        **Drug B**, N = 36 **Drug A**, N = 31 **Drug B**, N = 33
      1        51 (43, 57)        52 (42, 60)        45 (36, 52)
      2               <NA>               <NA>               <NA>
      3            9 (25%)            6 (19%)            7 (21%)
      4            9 (25%)            9 (29%)           10 (30%)
      5            6 (17%)            6 (19%)            8 (24%)
      6           12 (33%)           10 (32%)            8 (24%)

---

    Code
      tbl %>% as.data.frame()
    Output
         **Group** **Characteristic** **Drug A**, N = 35 **Drug B**, N = 33
      1    Grade I                Age        46 (36, 60)        48 (42, 55)
      2       <NA>            Unknown                  2                  0
      3       <NA>            T Stage               <NA>               <NA>
      4       <NA>                 T1            8 (23%)            9 (27%)
      5       <NA>                 T2            8 (23%)           10 (30%)
      6       <NA>                 T3           11 (31%)            7 (21%)
      7       <NA>                 T4            8 (23%)            7 (21%)
      8   Grade II                Age        45 (31, 55)        51 (43, 57)
      9       <NA>            Unknown                  2                  4
      10      <NA>            T Stage               <NA>               <NA>
      11      <NA>                 T1           14 (44%)            9 (25%)
      12      <NA>                 T2            8 (25%)            9 (25%)
      13      <NA>                 T3            5 (16%)            6 (17%)
      14      <NA>                 T4            5 (16%)           12 (33%)
      15 Grade III                Age        52 (42, 60)        45 (36, 52)
      16      <NA>            Unknown                  3                  0
      17      <NA>            T Stage               <NA>               <NA>
      18      <NA>                 T1            6 (19%)            7 (21%)
      19      <NA>                 T2            9 (29%)           10 (30%)
      20      <NA>                 T3            6 (19%)            8 (24%)
      21      <NA>                 T4           10 (32%)            8 (24%)
         **p-value**
      1          0.6
      2         <NA>
      3          0.8
      4         <NA>
      5         <NA>
      6         <NA>
      7         <NA>
      8         0.10
      9         <NA>
      10         0.3
      11        <NA>
      12        <NA>
      13        <NA>
      14        <NA>
      15        0.10
      16        <NA>
      17        >0.9
      18        <NA>
      19        <NA>
      20        <NA>
      21        <NA>

---

    Code
      trial %>% select(age, grade, stage, trt) %>% mutate(grade = paste("Grade",
        grade)) %>% tbl_strata(strata = grade, .tbl_fun = ~ .x %>% tbl_uvregression(
        y = age, method = lm)) %>% as.data.frame()
    Output
            **Characteristic** **N** **Beta** **95% CI** **p-value** **N** **Beta**
      1                T Stage    66     <NA>       <NA>        <NA>    62     <NA>
      2                     T1  <NA>     <NA>       <NA>        <NA>  <NA>     <NA>
      3                     T2  <NA>     -1.1   -12, 9.5         0.8  <NA>      2.9
      4                     T3  <NA>      2.3   -8.4, 13         0.7  <NA>      7.3
      5                     T4  <NA>     -5.7   -17, 5.4         0.3  <NA>      2.9
      6 Chemotherapy Treatment    66     <NA>       <NA>        <NA>    62     <NA>
      7                 Drug A  <NA>     <NA>       <NA>        <NA>  <NA>     <NA>
      8                 Drug B  <NA>     0.55  -7.0, 8.1         0.9  <NA>      5.7
        **95% CI** **p-value** **N** **Beta** **95% CI** **p-value**
      1       <NA>        <NA>    61     <NA>       <NA>        <NA>
      2       <NA>        <NA>  <NA>     <NA>       <NA>        <NA>
      3   -6.0, 12         0.5  <NA>     0.93   -9.5, 11         0.9
      4   -3.2, 18         0.2  <NA>     -2.4   -14, 8.9         0.7
      5   -6.9, 13         0.6  <NA>     -4.0   -15, 6.6         0.5
      6       <NA>        <NA>    61     <NA>       <NA>        <NA>
      7       <NA>        <NA>  <NA>     <NA>       <NA>        <NA>
      8   -1.1, 13        0.10  <NA>     -5.3   -12, 1.9        0.15

---

    Code
      trial %>% select(grade, stage, trt) %>% mutate(grade = paste("Grade", grade)) %>%
        tbl_strata(strata = grade, .tbl_fun = ~ .x %>% tbl_cross() %>% add_p()) %>%
        as.data.frame()
    Output
                Drug A Drug B Total p-value Drug A Drug B Total p-value Drug A Drug B
      1 T Stage   <NA>   <NA>  <NA>     0.8   <NA>   <NA>  <NA>     0.3   <NA>   <NA>
      2      T1      8      9    17    <NA>     14      9    23    <NA>      6      7
      3      T2      8     10    18    <NA>      8      9    17    <NA>      9     10
      4      T3     11      7    18    <NA>      5      6    11    <NA>      6      8
      5      T4      8      7    15    <NA>      5     12    17    <NA>     10      8
      6   Total     35     33    68    <NA>     32     36    68    <NA>     31     33
        Total p-value
      1  <NA>    >0.9
      2    13    <NA>
      3    19    <NA>
      4    14    <NA>
      5    18    <NA>
      6    64    <NA>

---

    Code
      trial %>% select(grade, stage, trt) %>% mutate(grade = paste("Grade", grade)) %>%
        tbl_strata(strata = grade, .tbl_fun = ~ .x %>% tbl_cross() %>% add_p()) %>%
        as.data.frame()
    Output
                Drug A Drug B Total p-value Drug A Drug B Total p-value Drug A Drug B
      1 T Stage   <NA>   <NA>  <NA>     0.8   <NA>   <NA>  <NA>     0.3   <NA>   <NA>
      2      T1      8      9    17    <NA>     14      9    23    <NA>      6      7
      3      T2      8     10    18    <NA>      8      9    17    <NA>      9     10
      4      T3     11      7    18    <NA>      5      6    11    <NA>      6      8
      5      T4      8      7    15    <NA>      5     12    17    <NA>     10      8
      6   Total     35     33    68    <NA>     32     36    68    <NA>     31     33
        Total p-value
      1  <NA>    >0.9
      2    13    <NA>
      3    19    <NA>
      4    14    <NA>
      5    18    <NA>
      6    64    <NA>

---

    Code
      trial %>% select(grade, response) %>% tbl_strata2(strata = grade, .tbl_fun = ~
        .x %>% tbl_summary(include = response, label = list(response = .y), missing = "no")) %>%
        as.data.frame()
    Output
        **Characteristic** **N = 68** **N = 68** **N = 64**
      1              **I**   21 (31%)       <NA>       <NA>
      2             **II**       <NA>   19 (30%)       <NA>
      3            **III**       <NA>       <NA>   21 (33%)

---

    Code
      survey::svydesign(~1, data = trial, weights = ~1) %>% tbl_strata(strata = grade,
        ~ tbl_svysummary(.x, by = trt, include = c(stage, trt), percent = "cell") %>%
          modify_header(all_stat_cols() ~ "**{level}**"), .combine_with = "tbl_stack") %>%
        as.data.frame()
    Output
         **Group** **Characteristic** **Drug A** **Drug B**
      1          I              stage       <NA>       <NA>
      2       <NA>                 T1    8 (12%)    9 (13%)
      3       <NA>                 T2    8 (12%)   10 (15%)
      4       <NA>                 T3   11 (16%)    7 (10%)
      5       <NA>                 T4    8 (12%)    7 (10%)
      6         II              stage       <NA>       <NA>
      7       <NA>                 T1   14 (21%)    9 (13%)
      8       <NA>                 T2    8 (12%)    9 (13%)
      9       <NA>                 T3   5 (7.4%)   6 (8.8%)
      10      <NA>                 T4   5 (7.4%)   12 (18%)
      11       III              stage       <NA>       <NA>
      12      <NA>                 T1   6 (9.4%)    7 (11%)
      13      <NA>                 T2    9 (14%)   10 (16%)
      14      <NA>                 T3   6 (9.4%)    8 (13%)
      15      <NA>                 T4   10 (16%)    8 (13%)

---

    Code
      tbl %>% as.data.frame()
    Output
        **Characteristic** **N = 98** **N = 102**
      1              Grade       <NA>        <NA>
      2                  I   35 (36%)    33 (32%)
      3                 II   32 (33%)    36 (35%)
      4                III   31 (32%)    33 (32%)
      5            T Stage       <NA>        <NA>
      6                 T1   28 (29%)    25 (25%)
      7                 T2   25 (26%)    29 (28%)
      8                 T3   22 (22%)    21 (21%)
      9                 T4   23 (23%)    27 (26%)

