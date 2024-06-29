#' Default Statistics Labels
#'
#' @return named list
#' @keywords internal
default_stat_labels <- function() {
  list(
    # standard summary stat labels
    mean = "Mean",
    sd = "SD",
    var = "Variance",
    median = "Median",
    min = "Min",
    max = "Max",
    sum = "Sum",
    n = "n",
    N = "N",
    p = "%",
    N_obs = "No. obs.",
    N_miss = "N Missing",
    N_nonmiss = "N Non-missing",
    p_miss = "% Missing",
    p_nonmiss = "% Non-missing",

    # survey statistics
    N_unweighted = "N (unweighted)",
    n_unweighted = "n (unweighted)",
    N_obs_unweighted = "Total N (unweighted)",
    N_miss_unweighted = "N Missing (unweighted)",
    N_nonmiss_unweighted = "N not Missing (unweighted)",
    p_unweighted = "% (unweighted)",
    p_miss_unweighted = "% Missing (unweighted)",
    p_nonmiss_unweighted = "% not Missing (unweighted)",
    mean.std.error = "SE",
    p.std.error = "SE(%)",
    deff = "Design effect"
  ) |>
    # adding the percentile labels
    c(paste0(0:100, "% Centile") |> as.list() |> set_names(paste0("p", 0:100))) |>
    utils::modifyList(val = list(p25 = "Q1", p50 = "Q2", p75 = "Q3"))
}
