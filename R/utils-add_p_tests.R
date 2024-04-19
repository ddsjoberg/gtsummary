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

add_p_test_mcnemar.test <- function(data, variable, by, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_mcnemar_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      conf.level = conf.level,
      !!!test.args
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

add_p_test_mcnemar.test_wide <- function(data, variable, by, test.args, ...) {
  # TODO: Add this function
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

add_p_test_aov <- function(data, variable, by, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_aov(
      # TODO: Update this to cardx::reformulate2()
      formula = stats::reformulate(termlabels = by, response = variable),
      data = data,
      !!!test.args
    )
  )
}

add_p_test_oneway.test <- function(data, variable, by, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_oneway_test(
      # TODO: Update this to cardx::reformulate2()
      formula = stats::reformulate(termlabels = by, response = variable),
      data = data,
      !!!test.args
    )
  )
}

add_p_test_mood.test <- function(data, variable, by, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_mood_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by)
    )
  )
}

add_p_test_lme4 <- function(data, variable, by, group, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_pkg_installed("lme4", reference_pkg = "cardx")

  cardx::ard_stats_anova(
    x = data,
    formulas = list(
      glue::glue("as.factor({cardx::bt(by)}) ~ 1 + (1 | {cardx::bt(group)})") |> stats::as.formula(),
      glue::glue("as.factor({cardx::bt(by)}) ~ {cardx::bt(variable)} + (1 | {cardx::bt(group)})") |> stats::as.formula()
    ),
    method = "glmer",
    method.args = list(family = stats::binomial),
    package = "lme4",
    method_text = "random intercept logistic regression"
  ) |>
    # keep results associated with 2nd model
    dplyr::filter(.data$variable %in% dplyr::last(.data$variable)) |>
    dplyr::mutate(
      variable = .env$variable,
      warning = list(NULL)
    )
}


add_p_tbl_summary_paired.t.test <- function(data, variable, by, group, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_paired_t_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(id),
      conf.level = conf.level,
      !!!test.args
    )
  )
}

add_p_tbl_summary_paired.wilcox.test <- function(data, variable, by, group, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_paired_wilcox_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(id),
      conf.level = conf.level,
      !!!test.args
    )
  )
}

add_p_test_prop.test <- function(data, variable, by, group, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_stats_prop_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(id),
      conf.level = conf.level,
      !!!test.args
    )
  )
}

add_p_test_ancova <- function(data, variable, by, adj.vars, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  cardx::ard_regression_basic(
    x =
      cardx::construct_model(
        data = data,
        formula = cardx::reformulate2(c(by, adj.vars), response = variable),
        method = "lm"
      )
  ) |>
    dplyr::filter(
      .data$variable %in% .env$by,
      .data$stat_name %in% c("estimate", "p.value", "statistic", "std.error", "conf.low", "conf.high")
    ) |>
    dplyr::bind_rows(
      .,
      dplyr::filter(., dplyr::row_number() == 1L) |>
        dplyr::mutate(
          stat = "method",
          stat_name = "method",
          stat =
            dplyr::case_when(
              is.null(adj.vars) ~ "One-way ANOVA",
              TRUE ~ "ANCOVA"
            ),
          fmt_fun = list(NULL)
        )
    )
}


add_p_test_cohens_d <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_effectsize_cohens_d(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      ci = conf.level,
      !!!test.args
    )
  )
}

# TODO: Add this function to the test xlsx file
add_p_test_paired_cohens_d <- function(data, variable, by, test.args, group, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_effectsize_paired_cohens_d(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(group),
      ci = conf.level,
      !!!test.args
    )
  )
}

# TODO: Add this function to the test xlsx file
add_p_test_hedges_g <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_effectsize_hedges_g(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      ci = conf.level,
      !!!test.args
    )
  )
}

# TODO: Add this function to the test xlsx file
add_p_test_paired_hedges_g <- function(data, variable, by, test.args, group, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  rlang::inject(
    cardx::ard_effectsize_paired_hedges_g(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(group),
      ci = conf.level,
      !!!test.args
    )
  )
}

add_p_test_smd <- function(data, variable, by, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  cardx::ard_smd_smd(
    data = data,
    variable = all_of(variable),
    by = all_of(by)
  )
}


# TODO: Do we need an ARD solution for this? This is just copied from the previous version
add_p_test_emmeans <- function(data, variable, by, adj.vars, conf.level = 0.95,
                               type, group = NULL, ...) {
  if (!is.null(group)) check_pkg_installed("lme4", reference_pkg = "cardx")
  if (inherits(data, "survey.design")) check_pkg_installed("survey", reference_pkg = "cardx")

  data_frame <-
    .extract_data_frame(data)

  # checking inputs
  if (!type %in% c("continuous", "dichotomous")) {
    cli::cli_abort("Variable must be summary type 'continuous' or 'dichotomous'", call = get_cli_abort_call())
  }
  if (dplyr::n_distinct(data_frame[[by]], na.rm = TRUE) != 2) {
    cli::cli_abort("`by=` must have exactly 2 levels", call = get_cli_abort_call())
  }
  if (type %in% "dichotomous" && dplyr::n_distinct(data_frame[[by]], na.rm = TRUE) != 2) {
    cli::cli_abort("`variable=` must have exactly 2 levels", call = get_cli_abort_call())
  }
  if (is_survey(data) && !is.null(group)) {
    cli::cli_abort("Cannot use `group=` argument with 'emmeans' and survey data.", call = get_cli_abort_call())
  }

  # assembling formula
  # styler: off
  termlabels <-
    if (is.null(group)) c(by, adj.vars)
    else c(by, adj.vars, glue::glue("(1 | {cardx::bt(group)})"))
  response <-
    if (type == "dichotomous") glue::glue("as.factor({cardx::bt(variable)})")
    else cardx::bt(variable)
  f <- stats::reformulate(termlabels, response)
  f_by <- cardx::reformulate2(by)

    rlang::inject(~ !!rlang::sym(chr_w_backtick(by)))
  # styler: on

  type2 <-
    dplyr::case_when(
      is.data.frame(data) && is.null(group) ~ type,
      inherits(data, "survey.design") && is.null(group) && type == "dichotomous" ~ "dichotomous_svy",
      inherits(data, "survey.design") && is.null(group) && type == "continuous" ~ "continuous_svy",
      is.data.frame(data) && type == "dichotomous" ~ "dichotomous_mixed",
      is.data.frame(data) && type == "continuous" ~ "continuous_mixed",
    )

  model_fun <-
    switch(type2,
           "dichotomous" =
             cardx::construct_model(x = data)
             purrr::partial(stats::glm, formula = f, data = data, family = stats::binomial),
           "continuous" =
             purrr::partial(stats::lm, formula = f, data = data),
           "dichotomous_svy" =
             purrr::partial(survey::svyglm, formula = f, design = data, family = stats::binomial),
           "continuous_svy" =
             purrr::partial(survey::svyglm, formula = f, design = data),
           "dichotomous_mixed" =
             purrr::partial(lme4::glmer, formula = f, data = data, family = stats::binomial),
           "continuous_mixed" =
             purrr::partial(lme4::lmer, formula = f, data = data)
    )

  emmeans_fun <-
    switch(type,
           "dichotomous" =
             purrr::partial(emmeans::emmeans, specs = f_by, regrid = "response"),
           "continuous" =
             purrr::partial(emmeans::emmeans, specs = f_by)
    )

  # building model
  model_fun() %>%
    emmeans_fun() %>%
    emmeans::contrast(method = "pairwise") %>%
    summary(infer = TRUE, level = conf.level) %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      conf.low = any_of("asymp.LCL"),
      conf.high = any_of("asymp.UCL"),
      conf.low = any_of("lower.CL"),
      conf.high = any_of("upper.CL")
    ) %>%
    dplyr::select(
      "estimate",
      std.error = "SE",
      "conf.low", "conf.high",
      "p.value"
    ) %>%
    dplyr::mutate(
      method =
        ifelse(
          is.null(.env$adj.vars),
          "Regression least-squares mean difference",
          "Regression least-squares adjusted mean difference"
        )
    )

}

# TODO: we should be able to delete this is the emmeans functions gets deleted.
.extract_data_frame <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  x$variables # return survey object data frame
}
