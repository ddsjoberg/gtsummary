library(magrittr)

# install previous releases of gtsummary ---------------------------------------
df_tags <- gert::git_tag_list()

# install version to folder if not already installed
purrr::walk(
  seq_len(nrow(df_tags)),
  function(.x) {
    path <- here::here("benchmark", "lib", df_tags$name[.x])
    # if directory exists and has files, then assume it's already installed
    if (dir.exists(path) && "gtsummary" %in% dir(path, all.files=TRUE)) {
      usethis::ui_done("{usethis::ui_value(df_tags$name[.x])} already installed...skipping")
      return(invisible(NULL))
    }

    usethis::ui_done("Installing {usethis::ui_value(df_tags$name[.x])}")
    dir.create(path)
    remotes::install_github(
      "ddsjoberg/gtsummary",
      ref = df_tags$commit[.x],
      lib = path
    )
  }
)

# installing master and current version ----------------------------------------
path_master <- here::here("benchmark", "lib", "master")
dir.create(path_master)

remotes::install_github("ddsjoberg/gtsummary", lib = path_master)
devtools::install()

# setting up benchmark paramters -----------------------------------------------
# Set how many times the benchmark will run
bm_times <- 30
big_trial <- purrr::map_dfr(seq_len(100), ~gtsummary::trial)

# The functions to be tested:
functions_list <-
  alist(
    simple = tbl_summary(trial),
    complex = tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>% add_q() %>% add_n(),
    big_data = big_trial %>% dplyr::select(age, grade, trt) %>% tbl_summary(by = trt, missing = 'no') %>% add_p()
  )

# run benchmark on current version ---------------------------------------------
library(gtsummary)

microbenchmark::microbenchmark(
  list = functions_list,
  times = bm_times,
  unit = "s"
) %>%
  summary() %>%
  dplyr::mutate(version = "current") %>%
  write.csv(file = here::here("benchmark", "results", "benchmark_current.csv"))

detach("package:gtsummary", unload = TRUE)

# run benchmark for other versions ---------------------------------------------
for (gtversion in c(df_tags$name, "master")) {
  library(gtsummary, lib.loc = here::here("benchmark", "lib", gtversion))

  # only run benchmark if old version is more than 60 days old -----------------
  output_filename_ext <- file.path("benchmark", "results", paste0("benchmark_", gtversion, ".csv"))
  output_filename <- here::here(output_filename_ext)

  days_since_last_update <-
    gert::git_ls() %>%
    dplyr::filter(path %in% output_filename_ext) %>%
    dplyr::pull(modified) %>%
    lubridate::as_date() %>%
    lubridate::interval(Sys.Date()) / lubridate::ddays()

  usethis::ui_done("Working on {usethis::ui_value(gtversion)}")
  if (!file.exists(output_filename) || (days_since_last_update > 45 && runif(1) < 0.2)) {
    tryCatch(
      microbenchmark::microbenchmark(
        list = functions_list,
        times = bm_times,
        unit = "s"
      ) %>%
        summary() %>%
        dplyr::mutate(version = gtversion) %>%
        write.csv(file = output_filename),
      error = function(e) {
        usethis::ui_oops("    Failed to benchmark")
        message(as.character(e))
      }
    )
  }
  else usethis::ui_done("    No need to update")

  detach("package:gtsummary", unload = TRUE)
}

















#
# # Installing different versions of the gtsummary package ----
# # Creating a library for each gtsummary version:
# .libPaths(new = here::here("benchmark/lib/cran"))
# .libPaths(new = here::here("benchmark/lib/github"))
#
# # Install gtsummary-CRAN version
# remotes::install_cran(pkgs = "gtsummary", lib = here::here("benchmark/lib/cran/"),
#                       dependencies = c("Depends", "Imports"),
#                       quiet = TRUE, build_manual = FALSE, build_vignettes = FALSE)
#
# # Install gtsummary-devel version
# devtools::install_github("https://github.com/ddsjoberg/gtsummary",
#                          lib ="benchmark/lib/github",
#                          dependencies = c("Depends", "Imports"), quiet = TRUE, build_manual = FALSE,
#                          build_vignettes = FALSE)
#
# #These print() functions are just for debugging within GA workflow, can be deleted
# print(paste0("Here starts current commit installation ", Sys.time()))
#
# # Install gtsummary-current commit version to the standard lib
# devtools::install(dependencies = c("Depends", "Imports"), quiet = TRUE, quick = TRUE)
#
# print(paste0("Here finishes current commit installation", Sys.time()))
#
# devtools::install_cran(c("dplyr", "magrittr", "ggplot2", "microbenchmark"),
#                        quiet = TRUE, dependencies = c("Depends", "Imports"),
#                        build_manual = FALSE, build_vignettes = FALSE)
#
# library(magrittr)
# library(dplyr)
# library(ggplot2)
# library(microbenchmark)
#
# # Set how many times the benchmark will try each function:----
# bm_times <- 30
#
# # The functions to be tested:
# functions_list <- alist(simple= tbl_summary(trial),
#                         complex=tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>%
#                           add_q(quiet = TRUE) %>% add_n(),
#                         big_data = big_trial %>% select(age, grade, trt) %>%
#                           tbl_summary(by = trt, missing = 'no') %>% add_p())
#
# # Benchmark CRAN version ----
# # detach("package:gtsummary", unload=TRUE)
# library(gtsummary, lib.loc = here::here("benchmark/lib/cran/"))
# gt_ver <- as.character(packageVersion("gtsummary"))
#
# # Define the size of dataframe for big_data tests:----
# # The following function must remain here, after the first loading of gtsummary
# big_trial <- purrr::map_dfr(seq_len(100), ~trial)
#
# bm_gtsummary <- microbenchmark(
#   list=functions_list,
#   times = bm_times,
#   unit = "s")
#
# benchmark_result <- summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver)
# benchmark_data <- data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver)
#
# # Benchmark github version ----
# detach("package:gtsummary", unload=TRUE)
# library(gtsummary, lib.loc = here::here("benchmark/lib/github/"))
# gt_ver <- as.character(packageVersion("gtsummary"))
#
# bm_gtsummary <- microbenchmark(
#   list=functions_list,
#   times = bm_times, unit = "s")
#
# benchmark_result <- rbind(benchmark_result,summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver))
# benchmark_data <- rbind(benchmark_data, data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver))
#
# # Benchmark gtsummary current_commit ----
# detach("package:gtsummary", unload=TRUE)
# library(gtsummary)
#
# gt_ver <- as.character(packageVersion("gtsummary"))
# gt_ver <- paste0(gt_ver, "_current_commit")
#
# bm_gtsummary <- microbenchmark(
#   list=functions_list,
#   times = bm_times, unit = "s")
#
# benchmark_result <- rbind(benchmark_result,summary(bm_gtsummary) %>% mutate("gtsummary version"=gt_ver))
# benchmark_data <- rbind(benchmark_data, data.frame(bm_gtsummary$expr, bm_gtsummary$time, gt_ver))
#
# # Of note:
# # benchmark_result contains the summary for each test,
# # benchmark_data contains the raw data for each test
#
# # Plots:----
# benchmark_data$gt_ver <- as.factor(benchmark_data$gt_ver)
#
# benchmark_data %>%
#   ggplot(aes(x=as.factor(gt_ver), y=bm_gtsummary.time/1e9))+
#   facet_wrap(vars(bm_gtsummary.expr))+
#   geom_boxplot()+
#   theme_minimal()+
#   theme(legend.position='none',
#         axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))+
#   labs(y="seconds", title = "Time to run each function",
#        subtitle=paste0(bm_times, " runs"),
#        x="")+
#   ggsave(here::here("benchmark/benchmark.png"))
#
# benchmark_data %>%
#   ggplot(aes(color=gt_ver, x=gt_ver, y=bm_gtsummary.time/1e9))+
#   facet_wrap(vars(bm_gtsummary.expr))+
#   geom_jitter(alpha=0.4)+
#   theme_minimal()+
#   theme(legend.position='none',
#         axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))+
#   labs(y="seconds", title = "Time to run each function",
#        subtitle=paste0(bm_times, " runs"),
#        x="")+
#   ggsave(here::here("benchmark/benchmark_jitter.png"))
#
# # Save data:----
# write.csv2(benchmark_data, file = here::here("benchmark/benchmark.csv"),
#            row.names = FALSE,
#            fileEncoding = "UTF-8")
#
# write.csv2(benchmark_result, file = here::here("benchmark/benchmark_summary.csv"),
#            row.names = FALSE,
#            fileEncoding = "UTF-8")
#
