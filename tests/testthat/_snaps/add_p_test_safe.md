# testing statistical tests

    Code
      .get_add_p_test_fun(class = "tbl_summary", test = "t.test") %>%
        .run_add_p_test_fun(data = trial, variable = "age", by = "trt", type = "continuous",
          group = NULL) %>% purrr::pluck("df_result")
    Output
      # A tibble: 1 x 10
        estim~1 stati~2 param~3 conf.~4 conf.~5 p.value method estim~6 estim~7 alter~8
          <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    <dbl>   <dbl> <chr>  
      1  -0.438  -0.209    184.   -4.57    3.69   0.834 Welch~    47.0    47.4 two.si~
      # ... with abbreviated variable names 1: estimate, 2: statistic, 3: parameter,
      #   4: conf.low, 5: conf.high, 6: estimate1, 7: estimate2, 8: alternative

---

    Code
      .get_add_p_test_fun(class = "tbl_summary", test = "lme4") %>%
        .run_add_p_test_fun(data = trial, variable = "age", by = "trt", type = "continuous",
          group = "stage") %>% purrr::pluck("df_result")
    Message
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
    Output
      # A tibble: 1 x 2
        p.value method                              
          <dbl> <chr>                               
      1   0.833 random intercept logistic regression

---

    Code
      .get_add_p_test_fun(class = "tbl_summary", test = "lme4") %>%
        .run_add_p_test_fun(data = trial, variable = "response", by = "trt", type = "categorical",
          group = "stage") %>% purrr::pluck("df_result")
    Message
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
    Output
      # A tibble: 1 x 2
        p.value method                              
          <dbl> <chr>                               
      1   0.530 random intercept logistic regression

