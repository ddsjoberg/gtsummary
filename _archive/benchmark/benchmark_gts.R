
# install previous releases of gtsummary ---------------------------------------
df_tags <- gert::git_tag_list() |> dplyr::arrange(name)

# install version to folder if not already installed ---------------------------
fs::dir_create(here::here("benchmark", "lib"))
purrr::walk(
  seq_len(nrow(df_tags)),
  function(.x) {
    print(df_tags$name[.x])
    path <- here::here("benchmark", "lib", df_tags$name[.x])
    # if directory exists and has files, then assume it's already installed
    if (dir.exists(path) && "gtsummary" %in% dir(path, all.files=TRUE)) {
      usethis::ui_done("{usethis::ui_value(df_tags$name[.x])} already installed...skipping")
      return(invisible(NULL))
    }

    usethis::ui_done("Installing {usethis::ui_value(df_tags$name[.x])}")
    dir.create(path)
    tryCatch(
      remotes::install_github(
        "ddsjoberg/gtsummary",
        ref = df_tags$commit[.x],
        lib = path
      ),
      error = function(e) {
        fs::dir_delete(path)
        invisible()
      }
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
bm_times <- 100
big_trial <- purrr::map_dfr(seq_len(100), ~gtsummary::trial)

# The functions to be tested:
functions_list <-
  alist(
    simple = tbl_summary(trial),
    complex = tbl_summary(trial, by = trt) |> add_overall() |> add_p() |> add_q() |> add_n(),
    big_data = big_trial |> dplyr::select(age, grade, trt) |> tbl_summary(by = trt, missing = 'no') |> add_p()
  )

# run benchmark on current version ---------------------------------------------
library(gtsummary)

microbenchmark::microbenchmark(
  list = functions_list,
  times = bm_times,
  unit = "s"
) |>
  summary() |>
  dplyr::mutate(version = "current") |>
  write.csv(file = here::here("benchmark", "results", "benchmark_current.csv"))

detach("package:gtsummary", unload = TRUE)


# run benchmark for other versions ---------------------------------------------
for (gtversion in c(df_tags$name, "master")) {
  # load previous version of gtsummary
  library(gtsummary, lib.loc = here::here("benchmark", "lib", gtversion))

  # only run benchmark if old version is more than 45 days old -----------------
  output_filename_ext <- file.path("benchmark", "results", paste0("benchmark_", gtversion, ".csv"))
  output_filename <- here::here(output_filename_ext)

  days_since_last_update <-
    gert::git_ls() |>
    dplyr::filter(path %in% output_filename_ext) |>
    dplyr::pull(modified) |>
    lubridate::as_date() |>
    lubridate::interval(Sys.Date()) / lubridate::ddays()

  usethis::ui_done("Working on {usethis::ui_value(gtversion)}")
  # old benchmark results updates have random component so all versions don't
  # update on the same day...they take a long time to run....
  if (!file.exists(output_filename) || (days_since_last_update > 45 && runif(1) < 0.2)) {
    # using tryCatch as some old versions will just fail because the code is out of date
    tryCatch(
      microbenchmark::microbenchmark(
        list = functions_list,
        times = bm_times,
        unit = "s"
      ) |>
        summary() |>
        dplyr::mutate(version = gtversion) |>
        write.csv(file = output_filename),
      error = function(e) {
        usethis::ui_oops("    Failed to benchmark")
        message(as.character(e))
        unlink(output_filename) # delete file if one exists
      }
    )
  }
  else usethis::ui_done("    No need to update")

  detach("package:gtsummary", unload = TRUE)
}


# import benchmark results -----------------------------------------------------
df_results <-
  list.files(here::here("benchmark", "results"), full.names = TRUE) |>
  purrr::map_dfr(readr::read_csv) |>
  dplyr::mutate(
    version =
      factor(version, levels = c(df_tags$name, "master", "current")) |>
      forcats::fct_drop()
  ) |>
  dplyr::select(version, expr, median, lq, uq)

# plot results -----------------------------------------------------------------
gg_bench_tbl_summary <-
  df_results |>
  ggplot2::ggplot(ggplot2::aes(x = version, y = median, group = 1)) +
  ggplot2::geom_point() +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lq, ymax = uq), alpha = 0.3) +
  ggplot2::facet_wrap(ggplot2::vars(expr), ncol = 1, scales = "free_y") +
  ggplot2::labs(
    y = "seconds",
    x = " "
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
