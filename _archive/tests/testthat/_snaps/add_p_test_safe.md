# testing statistical tests

    Code
      .get_add_p_test_fun(class = "tbl_summary", test = "t.test") %>%
        .run_add_p_test_fun(data = trial, variable = "age", by = "trt", type = "continuous",
          group = NULL) %>% purrr::pluck("df_result")
    Output
      # A tibble: 1 x 10
        estimate statistic parameter conf.low conf.high p.value method       estimate1
           <dbl>     <dbl>     <dbl>    <dbl>     <dbl>   <dbl> <chr>            <dbl>
      1   -0.438    -0.209      184.    -4.57      3.69   0.834 Welch Two S~      47.0
      # i 2 more variables: estimate2 <dbl>, alternative <chr>

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

