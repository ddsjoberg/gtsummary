# NOTE:Subfolders for libs must be created manually beforehand!

install.packages("remotes")
install.packages("devtools")
install.packages("here")
library(remotes)
library(devtools)
library(here)
# Installing different versions of the gtsummary package ----

.libPaths(new = here::here("lib/cran"))
.libPaths(new = here::here("lib/github"))
# .libPaths(new = "lib/spanish_translation")

install.packages(pkgs = "gtsummary", lib = "lib/cran", dependencies = c("Depends", "Imports"))
remotes::install_github("https://github.com/ddsjoberg/gtsummary",
                        lib ="lib/github",
                        dependencies = c("Depends", "Imports"))
# remotes::install_github(repo="oranwutang/gtsummary@spanish_translation",
#                         lib ="lib/spanish_translation", force = TRUE, build = FALSE)
# .libPaths(new = "lib/spanish_translation")

# Installing packages dependencies manually to each library

# install.packages("dplyr", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("rlang", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("vctrs", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("R6", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("generics", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("glue", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("lifecycle", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("magrittr", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("tibble", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("ellipsis", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("pillar", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("crayon", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("pkgconfig", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("tidyselect", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("purrr", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("huxtable", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("assertthat", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("knitr", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("xfun", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("stringr", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("stringi", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("tidyr", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("Rcpp", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("ggplot2", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("withr", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("tidyverse", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("forcats", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("labeling", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))
# install.packages("gt", lib=c("lib/cran", "lib/github"))
# install.packages("microbenchmark", lib=c("lib/cran", "lib/github", "lib/spanish_translation"))

install.packages("dplyr", lib=c("lib/cran", "lib/github"))
install.packages("rlang", lib=c("lib/cran", "lib/github"))
install.packages("vctrs", lib=c("lib/cran", "lib/github"))
install.packages("R6", lib=c("lib/cran", "lib/github"))
install.packages("generics", lib=c("lib/cran", "lib/github"))
install.packages("glue", lib=c("lib/cran", "lib/github"))
install.packages("lifecycle", lib=c("lib/cran", "lib/github"))
install.packages("magrittr", lib=c("lib/cran", "lib/github"))
install.packages("tibble", lib=c("lib/cran", "lib/github"))
install.packages("ellipsis", lib=c("lib/cran", "lib/github"))
install.packages("pillar", lib=c("lib/cran", "lib/github"))
install.packages("crayon", lib=c("lib/cran", "lib/github"))
install.packages("pkgconfig", lib=c("lib/cran", "lib/github"))
install.packages("tidyselect", lib=c("lib/cran", "lib/github"))
install.packages("purrr", lib=c("lib/cran", "lib/github"))
install.packages("huxtable", lib=c("lib/cran", "lib/github"))
install.packages("assertthat", lib=c("lib/cran", "lib/github"))
install.packages("knitr", lib=c("lib/cran", "lib/github"))
install.packages("xfun", lib=c("lib/cran", "lib/github"))
install.packages("stringr", lib=c("lib/cran", "lib/github"))
install.packages("stringi", lib=c("lib/cran", "lib/github"))
install.packages("tidyr", lib=c("lib/cran", "lib/github"))
install.packages("Rcpp", lib=c("lib/cran", "lib/github"))
install.packages("ggplot2", lib=c("lib/cran", "lib/github"))
install.packages("withr", lib=c("lib/cran", "lib/github"))
install.packages("tidyverse", lib=c("lib/cran", "lib/github"))
install.packages("forcats", lib=c("lib/cran", "lib/github"))
install.packages("labeling", lib=c("lib/cran", "lib/github"))
install.packages("gt", lib=c("lib/cran", "lib/github"))
install.packages("microbenchmark", lib=c("lib/cran", "lib/github"))

# After installing all of the required libraries,
# the script should be run from here:

# Loading libraries ----
library(magrittr, lib=c("lib/cran", "lib/github"))
library(dplyr, lib=c("lib/cran", "lib/github"))
library(ggplot2, lib=c("lib/cran", "lib/github"))
library(forcats, lib=c("lib/cran", "lib/github"))
library(microbenchmark, lib=c("lib/cran", "lib/github"))

# Set how many times the benchmark will try each function:----

bm_times <- 5

# Benchmark CRAN version ----
detach("package:gtsummary", unload=TRUE)
library(gtsummary, lib.loc = "lib/cran/")
gt_ver <- as.character(packageVersion("gtsummary"))

bm_gtsummary <- microbenchmark(
  trial %>%
    select(trt, age, grade, response) %>%
    tbl_summary(by = trt) %>%
    add_p(), times = bm_times, unit = "s")

benchmark_result <- summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver)
benchmark_data <- data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver)

# Benchmark github version ----
detach("package:gtsummary", unload=TRUE)
library(gtsummary, lib.loc = "lib/github/")
gt_ver <- as.character(packageVersion("gtsummary"))

bm_gtsummary <- microbenchmark(
  trial %>%
    select(trt, age, grade, response) %>%
    tbl_summary(by = trt) %>%
    add_p(), times = bm_times, unit = "s")

benchmark_result <- rbind(benchmark_result,summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver))
benchmark_data <- rbind(benchmark_data, data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver))

# # Benchmark spanish_translation version w/o theme ----
# detach("package:gtsummary", unload=TRUE)
# library(gtsummary, lib.loc = "lib/spanish_translation/")
# gt_ver <- as.character(packageVersion("gtsummary"))
#
# gt_ver <- paste0(gt_ver, "_no_theme")
#
# bm_gtsummary <- microbenchmark(
#   trial %>%
#     select(trt, age, grade, response) %>%
#     tbl_summary(by = trt) %>%
#     add_p(), times = bm_times, unit = "s")
#
# benchmark_result <- rbind(benchmark_result,summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver))
# benchmark_data <- rbind(benchmark_data, data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver))
#
# # Benchmark spanish_translation version with spanish theme ----
# detach("package:gtsummary", unload=TRUE)
# library(gtsummary, lib.loc = "lib/spanish_translation/")
# gt_ver <- as.character(packageVersion("gtsummary"))
#
# gt_ver <- paste0(gt_ver, "_es_theme")
#
# set_gtsummary_theme(theme_gtsummary_language("es"))
#
# bm_gtsummary <- microbenchmark(
#   trial %>%
#     select(trt, age, grade, response) %>%
#     tbl_summary(by = trt) %>%
#     add_p(), times = bm_times, unit = "s")

# benchmark_result contains the summary for each test,
# benchmark_data contains the raw data for each test

benchmark_result <- rbind(benchmark_result,summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver))
benchmark_data <- rbind(benchmark_data, data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver))

benchmark_data$gt_ver <- as.factor(benchmark_data$gt_ver)

# levels(benchmark_data$gt_ver) <- c("1.3.1", "1.3.2", "1.3.0.9019_no_theme", "1.3.0.9019_es_theme")

benchmark_data %>% ggplot(aes(as.factor(gt_ver), bm_gtsummary.time/1e9))+
  geom_boxplot()+
  theme_minimal()+
  labs(y="seconds", title = "Time to run:\n trial %>%
    select(trt, age, grade, response) %>%
    tbl_summary(by = trt) %>%
    add_p() ", subtitle=paste0(bm_times, "runs"), x="gtsummary version")+ggsave(here::here("benchmark/benchmark.png"))

write.csv2(benchmark_data, file = here::here("benchmark/benchmark.csv"), row.names = FALSE, fileEncoding = "UTF-8")


benchmark_data %>% ggplot(aes(color=gt_ver, x=gt_ver, y=bm_gtsummary.time/1e9))+
  geom_jitter(alpha=0.3)+
  geom_hline(yintercept = median(benchmark_data$bm_gtsummary.time/1e9), linetype=2, color = "red")+
  theme_minimal()+
  labs(y="seconds", title = "Time to run:\n trial %>%
    select(trt, age, grade, response) %>%
    tbl_summary(by = trt) %>%
    add_p() ", subtitle=paste0(bm_times, "runs"), x="gtsummary version",
    caption="--- Global median")+ggsave(here::here("benchmark/benchmark_jitter.png"))


# benchmark_data %>% ggplot(aes(fill=gt_ver, x=bm_gtsummary.time/1e9))+
#   geom_density(alpha=0.3)+
#   theme_minimal()

# benchmark_data %>%
#   mutate(bm_gtsummary.time=bm_gtsummary.time/1e9) %>%
#   select(bm_gtsummary.time, gt_ver) %>%
#   tbl_summary(by=gt_ver) %>% add_p() %>% add_q()

# relacion <-  median(benchmark_data$bm_gtsummary.time[benchmark_data$gt_ver=="1.3.0.9019_no_theme"])/
#   median(benchmark_data$bm_gtsummary.time[benchmark_data$gt_ver=="1.3.1"])
