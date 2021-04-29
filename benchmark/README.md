{gtsummary} benchmark
================
April 28, 2021

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Execution times

**`tbl_sumary()`**

<img src="README_files/figure-gfm/unnamed-chunk-1-1.png" width="100%" />

The functions used for the benchmark are:

`simple = tbl_summary(trial)`

`complex = tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>% add_q(quiet = TRUE) %>% add_n()`

`big_data = big_trial %>% select(age, grade, trt) %>% tbl_summary(by = trt, missing = "no") %>% add_p()`
(NOTE: `big_trial` is a 5000-fold copy of `gtsummary::trial`)
