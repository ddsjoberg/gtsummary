## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

should_run_benchmarks <- function(x) {
  get("requireNamespace")("cpp11test", quietly = TRUE) && asNamespace("cpp11test")$should_run_benchmarks()
}

## ---- message = FALSE, eval = should_run_benchmarks()-------------------------
#  library(cpp11test)
#  grid <- expand.grid(len = 10 ^ (0:7), pkg = "cpp11", stringsAsFactors = FALSE)
#  grid <- rbind(
#    grid,
#    expand.grid(len = 10 ^ (0:4), pkg = "rcpp", stringsAsFactors = FALSE)
#  )
#  b_grow <- bench::press(.grid = grid,
#    {
#      fun = match.fun(sprintf("%sgrow_", ifelse(pkg == "cpp11", "", paste0(pkg, "_"))))
#      bench::mark(
#        fun(len)
#      )
#    }
#  )[c("len", "pkg", "min", "mem_alloc", "n_itr", "n_gc")]
#  saveRDS(b_grow, "growth.Rds", version = 2)

## ---- echo = FALSE, dev = "svg", fig.ext = "svg", eval = capabilities("cairo")----
b_grow <- readRDS("growth.Rds")
library(ggplot2)
ggplot(b_grow, aes(x = len, y = min, color = pkg)) +
  geom_point() +
  geom_line() +
  bench::scale_y_bench_time() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  coord_fixed() +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "log-log plot of vector size vs construction time", x = NULL, y = NULL)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(b_grow)

