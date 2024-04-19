# add_p.tbl_summary ------------------------------------------------------------

add_p_test_t.test <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_t_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      conf.level = conf.level,
      !!!test.args
    )
  )
}

add_p_test_wilcox.test <- function(data, variable, by, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_wilcox_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      !!!test.args
    )
  ) |>
    dplyr::mutate(
      stat = dplyr::case_when(
        .data$stat_name %in% "method" &
          .data$stat %in% "Wilcoxon rank sum test with continuity correction" ~ list("Wilcoxon rank sum test"),
        .default = .data$stat
      )
    )
}

add_p_test_chisq.test <- function(data, variable, by, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_chisq_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      !!!test.args
    )
  ) |>
    dplyr::mutate(
      stat = dplyr::case_when(
        .data$stat_name %in% "method" &
          .data$stat %in% "Pearson's Chi-squared test with Yates' continuity correction" ~ list("Pearson's Chi-squared test"),
        .default = .data$stat
      )
    )
}

add_p_test_chisq.test.no.correct <- function(data, variable, by, test.args, ...) {
  add_p_test_chisq.test(data = data, variable = variable, by = by,
                        test.args = c(list(correct = FALSE), test.args), ...)
}

add_p_test_mood.test <- function(data, variable, by, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  rlang::inject(
    cardx::ard_stats_mood_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      !!!test.args
    )
  )
}

add_p_test_kruskal.test <- function(data, variable, by, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  rlang::inject(
    cardx::ard_stats_kruskal_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by)
    )
  )
}

add_p_test_fisher.test <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_fisher_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      conf.level = conf.level,
      !!!test.args
    )
  ) |>
    dplyr::mutate(
      stat = dplyr::case_when(
        .data$stat_name %in% "method" &
          .data$stat %in% "Fisher's Exact Test for Count Data" ~ list("Fisher's exact test"),
        .default = .data$stat
      )
    )
}

add_p_test_lme4 <- function(data, variable, by, group, type, ...) {

  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_pkg_installed("lme4", reference_pkg = "cardx")
  data <-
    dplyr::select(data, all_of(c(variable, by, group))) %>%
    dplyr::filter(stats::complete.cases(.)) |>
    dplyr::mutate("{by}" := as.factor(.data[[by]]))

  # formula0 <- reformulate()


}

