# bench::cb_fetch()
stored_benchmarks <- bench::cb_read()
rmarkdown::render(input = here::here("bench/benchmark2html.Rmd"),
output_file = here::here("bench/benchmark2html.html"),
clean = T, params = list(stored_benchmarks = stored_benchmarks))
