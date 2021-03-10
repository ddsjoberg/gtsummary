# NOTE:Subfolders for libs must be created manually beforehand!
# They are created from the yaml file section "Create required directories"

install.packages("remotes", dependencies = c("Depends", "Imports") )
install.packages("devtools", dependencies = c("Depends", "Imports"))
install.packages("here", dependencies = c("Depends", "Imports"))

library(remotes)
library(devtools)
library(here)

# Installing different versions of the gtsummary package ----

# Creating a library for each gtsummary version:
.libPaths(new = here::here("benchmark/lib/cran"))
.libPaths(new = here::here("benchmark/lib/github"))

# Install gtsummary-CRAN version
install.packages(pkgs = "gtsummary", lib = here::here("benchmark/lib/cran/"), dependencies = c("Depends", "Imports"),
                 quiet = TRUE)

# Install gtsummary-devel version
remotes::install_github("https://github.com/ddsjoberg/gtsummary",
                        lib ="benchmark/lib/github",
                        dependencies = c("Depends", "Imports"), quiet = TRUE)

#These print() functions are just for debugging within GA workflow, can be deleted
print(paste0("Here starts current commit installation ", Sys.time()))

# Install gtsummary-current commit version to the standard lib
# system("R CMD INSTALL . --library=/Users/runner/work/gtsummary/gtsummary/benchmark/lib/current_branch", ignore.stdout = FALSE)
devtools::install(dependencies = c("Depends", "Imports"), quiet = TRUE)

print(paste0("Here finishes current commit installation", Sys.time()))

install.packages("dplyr",quiet = TRUE)
install.packages("magrittr",quiet = TRUE)
install.packages("ggplot2",quiet = TRUE)
install.packages("microbenchmark",quiet = TRUE)
library(magrittr)
library(dplyr)
library(ggplot2)
library(microbenchmark)

# Set how many times the benchmark will try each function:----
bm_times <- 20

#The functions to be tested:
functions_list <- alist(simple= tbl_summary(trial),
                        complex=tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>% add_q(quiet = TRUE) %>% add_n(),
                        big_data = big_trial %>% select(age, grade, trt) %>% tbl_summary(by = trt, missing = 'no') %>% add_p())

print(.libPaths())

# Benchmark CRAN version ----
# detach("package:gtsummary", unload=TRUE)
library(gtsummary, lib.loc = here::here("benchmark/lib/cran/"))
gt_ver <- as.character(packageVersion("gtsummary"))

# Define the size of dataframe for big_data tests:----
# THe following function must remain here, after the first loading of gtsummary
big_trial <- purrr::map_dfr(seq_len(100), ~trial)

bm_gtsummary <- microbenchmark(
  list=functions_list,
  times = bm_times,
  unit = "s")

benchmark_result <- summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver)
benchmark_data <- data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver)

# Benchmark github version ----
detach("package:gtsummary", unload=TRUE)
library(gtsummary, lib.loc = here::here("benchmark/lib/github/"))
gt_ver <- as.character(packageVersion("gtsummary"))

bm_gtsummary <- microbenchmark(
  list=functions_list,
  times = bm_times, unit = "s")

benchmark_result <- rbind(benchmark_result,summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver))
benchmark_data <- rbind(benchmark_data, data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver))

# Benchmark gtsummary current_commit ----
detach("package:gtsummary", unload=TRUE)
library(gtsummary)

gt_ver <- as.character(packageVersion("gtsummary"))
gt_ver <- paste0(gt_ver, "current_commit")

bm_gtsummary <- microbenchmark(
  list=functions_list,
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
  facet_wrap(vars(bm_gtsummary.expr))+
  geom_boxplot()+
  theme_minimal()+
 theme(legend.position='none',
                  axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))+
  labs(y="seconds", title = "Time to run each function",
       subtitle=paste0(bm_times, " runs"),
       x="")+
  ggsave(here::here("benchmark/benchmark.png"))

benchmark_data %>%
  ggplot(aes(color=gt_ver, x=gt_ver, y=bm_gtsummary.time/1e9))+
  facet_wrap(vars(bm_gtsummary.expr))+
  geom_jitter(alpha=0.4)+
  theme_minimal()+
  theme(legend.position='none',
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))+
  labs(y="seconds", title = "Time to run each function",
       subtitle=paste0(bm_times, " runs"),
       x="")+
  ggsave(here::here("benchmark/benchmark_jitter.png"))

# Save data:----
write.csv2(benchmark_data, file = here::here("benchmark/benchmark.csv"),
           row.names = FALSE,
           fileEncoding = "UTF-8")

write.csv2(benchmark_result, file = here::here("benchmark/benchmark_summary.csv"),
           row.names = FALSE,
           fileEncoding = "UTF-8")

