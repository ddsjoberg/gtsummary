library(gtsummary, warn.conflicts = FALSE)
library(bench)

# setup code
big_trial <- purrr::map_dfr(seq_len(5000), ~trial)

bench::mark(
  # simple summary
  simple = tbl_summary(trial),

  # simple calculation with comparisons+others
  # complex = tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>% add_q(quiet = TRUE) %>% add_n(),

  # big summary
  #big_data = big_trial %>% select(age, grade, trt) %>% tbl_summary(by = trt, missing = "no") %>% add_p(),

  check = FALSE,
  min_iterations = 5
)
