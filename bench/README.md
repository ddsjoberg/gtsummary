{gtsummary} benchmark
================
Gustavo Zapata Wainberg & Daniel Sjoberg
March 06, 2021

``` r
# bench::cb_fetch()
```

<!-- <!-- This css stuff is just to enable rmarkdown to use a wider area of screen -->

–&gt;

<!-- <style type="text/css"> -->
<!-- .main-container { -->
<!--   max-width: 1800px; -->
<!--   margin-left: auto; -->
<!--   margin-right: auto; -->
<!-- } -->
<!-- </style> -->

## Execution times

<iframe src="/Users/runner/work/gtsummary/gtsummary/bench/index1.html" width="100%" height="600" scrolling="no" seamless="seamless" frameBorder="0"></iframe>

<iframe src="/Users/runner/work/gtsummary/gtsummary/bench/index2.html" width="100%" height="600" scrolling="no" seamless="seamless" frameBorder="0"></iframe>

The functions used for the benchmark are:

`simple = tbl_summary(trial)`

`complex = tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>% add_q(quiet = TRUE) %>% add_n()`

`big_data = big_trial %>% select(age, grade, trt) %>% tbl_summary(by = trt, missing = "no") %>% add_p()`
(NOTE: `big_trial` is a 5000-fold copy of `gtsummary::trial`)
