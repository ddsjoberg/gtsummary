library(gtsummary, warn.conflicts = FALSE)
library(bench)
# library(rmarkdown)
# library(here)

# setup code
big_trial <- purrr::map_dfr(seq_len(5000), ~trial)
bench::cb_fetch("https://github.com/oranwutang/gtsummary/")
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

bench::cb_fetch("https://github.com/oranwutang/gtsummary/")
bench::cb_push("https://github.com/oranwutang/gtsummary/")


stored_benchmarks <- bench::cb_read("https://github.com/oranwutang/gtsummary/")
rmarkdown::render(input = here::here("bench/benchmark2html.Rmd"),
                  output_file = here::here("bench/benchmark2html.html"),
                  clean = T, params = list(stored_benchmarks = stored_benchmarks))

# The next call was added to the benchmark yaml file, if it works, it can be deleted form this script
# rmarkdown::render(input = here("bench/benchmark2html.Rmd"), # IMPORTANT: Relative paths doesn't work maybe solvable with {here}
#                  output_file = here("bench/benchmark2html.html"),  # IMPORTANT: Relative paths doesn't work maybe solvable with {here}
#                  clean = T)
