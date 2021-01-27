library(gtsummary, warn.conflicts = FALSE)
library(bench)

# setup code
big_trial <- purrr::map_dfr(seq_len(500), ~trial)

bench::mark(
  # simple summary
  tbl_summary(trial),

  # simple calculation with comparisons+others
  tbl_summary(trial, by = trt) %>% add_p() %>% add_q() %>% add_n(),

  # big summary
  big_trial %>% select(age, grade, trt) %>% tbl_summary(by = trt, missing = "no") %>% add_p(),

  check = FALSE
)
