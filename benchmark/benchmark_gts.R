# NOTE:Subfolders for libs must be created manually beforehand!
# They are created from the yaml file section "Create required directories"

install.packages("remotes", dependencies = c("Depends", "Imports") )
install.packages("devtools", dependencies = c("Depends", "Imports"))
install.packages("here", dependencies = c("Depends", "Imports"))


library(remotes)
library(devtools)
library(here)

# Installing different versions of the gtsummary package ----

.libPaths(new = here::here("benchmark/lib/cran"))
.libPaths(new = here::here("benchmark/lib/github"))
# .libPaths(new = here::here("benchmark/lib/current_branch"))

# Install gtsummary-CRAN version
install.packages(pkgs = "gtsummary", lib = here::here("benchmark/lib/cran/"), dependencies = c("Depends", "Imports"))

# Install gtsummary-devel version
remotes::install_github("https://github.com/ddsjoberg/gtsummary",
                        lib ="benchmark/lib/github",
                        dependencies = c("Depends", "Imports"))

# Install gtsummary-current commit version to the standard lib
# system("R CMD INSTALL . --library=/Users/runner/work/gtsummary/gtsummary/benchmark/lib/current_branch", ignore.stdout = FALSE)
devtools::install(dependencies = c("Depends", "Imports"))

install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("gt")
install.packages("microbenchmark")
install.packages("rmarkdown")
library(magrittr)
library(dplyr)
library(ggplot2)
library(forcats)
library(microbenchmark)


# Set how many times the benchmark will try each function:----
bm_times <- 30

# Define the size of dataframe for big_data tests:----
big_trial = purrr::map_dfr(seq_len(5000), ~trial)

# Benchmark CRAN version ----
# detach("package:gtsummary", unload=TRUE)
library(gtsummary, lib.loc = here::here("benchmark/lib/cran/"))
gt_ver <- as.character(packageVersion("gtsummary"))

bm_gtsummary <- microbenchmark(
  simple= tbl_summary(trial),
  complex=tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>% add_q(quiet = TRUE) %>% add_n(),
  big_data = big_trial %>% select(age, grade, trt) %>% tbl_summary(by = trt, missing = "no") %>% add_p(),
  times = bm_times, unit = "s")
# bm_gtsummary <- microbenchmark(
#   trial %>%
#     select(trt, age, grade, response) %>%
#     tbl_summary(by = trt) %>%
#     add_p(), times = bm_times, unit = "s")

benchmark_result <- summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver)
benchmark_data <- data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver)

# Benchmark github version ----
detach("package:gtsummary", unload=TRUE)
library(gtsummary, lib.loc = here::here("benchmark/lib/github/"))
gt_ver <- as.character(packageVersion("gtsummary"))

bm_gtsummary <- microbenchmark(
  simple= tbl_summary(trial),
  complex=tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>% add_q(quiet = TRUE) %>% add_n(),
  big_data = big_trial %>% select(age, grade, trt) %>% tbl_summary(by = trt, missing = "no") %>% add_p(),
  times = bm_times, unit = "s")

benchmark_result <- rbind(benchmark_result,summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver))
benchmark_data <- rbind(benchmark_data, data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver))

# Benchmark gtsummary current_commit ----
detach("package:gtsummary", unload=TRUE)
library(gtsummary)

gt_ver <- as.character(packageVersion("gtsummary"))
gt_ver <- paste0(gt_ver, "current_commit")

bm_gtsummary <- microbenchmark(
  simple= tbl_summary(trial),
  complex=tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>% add_q(quiet = TRUE) %>% add_n(),
  big_data = big_trial %>% select(age, grade, trt) %>% tbl_summary(by = trt, missing = "no") %>% add_p(),
  times = bm_times, unit = "s")

benchmark_result <- rbind(benchmark_result,summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver))
benchmark_data <- rbind(benchmark_data, data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver))

# Of note:
# benchmark_result contains the summary for each test,
# benchmark_data contains the raw data for each test

# Plots:----
benchmark_data$gt_ver <- as.factor(benchmark_data$gt_ver)

benchmark_data %>%
  ggplot(aes(x=as.factor(gt_ver), y=bm_gtsummary.time/1e9))+
  facet_wrap(vars(bm_gtsummary.expr), nrow = 3)+
  geom_boxplot()+
  theme_minimal()+
  labs(y="seconds", title = "Time to run each function",
       subtitle=paste0(bm_times, " runs"),
       x="gtsummary version")+
  ggsave(here::here("benchmark/benchmark.png"))

benchmark_data %>%
  ggplot(aes(color=gt_ver, x=gt_ver, y=bm_gtsummary.time/1e9))+
  facet_wrap(vars(bm_gtsummary.expr), nrow = 3)+
  geom_jitter(alpha=0.3)+
  geom_hline(yintercept = median(benchmark_data$bm_gtsummary.time/1e9), linetype=2, color = "red")+
  theme_minimal()+
  labs(y="seconds", title = "Time to run each function",
       subtitle=paste0(bm_times, " runs"),
       x="gtsummary version",
       caption="--- Global median")+
  ggsave(here::here("benchmark/benchmark_jitter.png"))

# Save data:----
write.csv2(benchmark_data, file = here::here("benchmark/benchmark.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# rmarkdown::render(input = here::here("bench/benchmark2html.Rmd"),
#                   output_file = here::here("bench/benchmark2html.html"),
#                   clean = T, params = list(stored_benchmarks = stored_benchmarks))

# benchmark_data %>% ggplot(aes(fill=gt_ver, x=bm_gtsummary.time/1e9))+
#   geom_density(alpha=0.3)+
#   theme_minimal()

# benchmark_data %>%
#   mutate(bm_gtsummary.time=bm_gtsummary.time/1e9) %>%
#   select(bm_gtsummary.time, gt_ver) %>%
#   tbl_summary(by=gt_ver) %>% add_p() %>% add_q()

# relacion <-  median(benchmark_data$bm_gtsummary.time[benchmark_data$gt_ver=="1.3.0.9019_no_theme"])/
#   median(benchmark_data$bm_gtsummary.time[benchmark_data$gt_ver=="1.3.1"])
