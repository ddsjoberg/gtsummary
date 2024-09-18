# add_p.tbl_summary ------------------------------------------------------------
add_p_test_t.test <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars"), ...)

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

add_p_test_wilcox.test <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars"), ...)

  rlang::inject(
    cardx::ard_stats_wilcox_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      conf.int = TRUE,
      conf.level = conf.level,
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


add_p_test_mcnemar.test <- function(data, variable, by, group, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("adj.vars"), ...)
  check_length(group, 1L)
  warn_unbalanced_pairs(data, by, variable, group)

  rlang::inject(
    cardx::ard_stats_mcnemar_test_long(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(group),
      !!!test.args
    )
  )
}

add_p_test_mcnemar.test_wide <- function(data, variable, by, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars"), ...)

  rlang::inject(
    cardx::ard_stats_mcnemar_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      !!!test.args
    )
  )
}

add_p_test_chisq.test <- function(data, variable, by, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars"), ...)

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
  add_p_test_chisq.test(
    data = data, variable = variable, by = by,
    test.args = c(list(correct = FALSE), test.args), ...
  )
}

add_p_test_mood.test <- function(data, variable, by, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars"), ...)

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
  check_empty(c("group", "adj.vars", "test.args"), ...)

  cardx::ard_stats_kruskal_test(
    data = data,
    variable = all_of(variable),
    by = all_of(by)
  )
}

add_p_test_fisher.test <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars"), ...)

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
  cli::cli_warn(c(
    "The test {.val aov} in {.code add_p(test)} was deprecated in {.pkg gtsummary} 2.0.0.",
    i = "The same functionality is covered in {.val oneway.test} with argument `var.equal = TRUE`."
  ))

  add_p_test_oneway.test(data = data, variable = variable, by = by, test.args = list(var.equal = TRUE))
}

add_p_test_oneway.test <- function(data, variable, by, test.args, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars"), ...)

  rlang::inject(
    cardx::ard_stats_oneway_test(
      formula = cardx::reformulate2(termlabels = by, response = variable),
      data = data,
      !!!test.args
    )
  )
}

add_p_test_mood.test <- function(data, variable, by, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars", "test.args"), ...)

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
  check_empty(c("test.args", "adj.vars"), ...)

  if (is_empty(group)) {
    cli::cli_abort("The {.arg group} argument cannot be missing for {.val lme4} tests.")
  }
  check_length(group, 1L)

  cardx::ard_stats_anova(
    x = data |> tidyr::drop_na(any_of(c(variable, by, group))),
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
  check_length(group, 1L)
  warn_unbalanced_pairs(data, by, variable, group)
  check_empty(c("adj.vars"), ...)

  rlang::inject(
    cardx::ard_stats_paired_t_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(group),
      conf.level = conf.level,
      !!!test.args
    )
  )
}

add_p_tbl_summary_paired.wilcox.test <- function(data, variable, by, group, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("adj.vars"), ...)
  check_length(group, 1L)
  warn_unbalanced_pairs(data, by, variable, group)

  rlang::inject(
    cardx::ard_stats_paired_wilcox_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(group),
      conf.level = conf.level,
      !!!test.args
    )
  )
}

add_p_test_prop.test <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("adj.vars", "group"), ...)

  rlang::inject(
    cardx::ard_stats_prop_test(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      conf.level = conf.level,
      !!!test.args
    )
  )
}

add_p_test_ancova <- function(data, variable, by, adj.vars = NULL, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "test.args"), ...)
  check_n_levels(data[[by]], 2L, message = "The {.arg by} column must have {.val {length}} levels.")

  # reverse coding the 'by' variable
  data[[by]] <- fct_rev(factor(data[[by]]))

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
    ) %>%
    dplyr::bind_rows(
      .,
      dplyr::filter(., dplyr::row_number() == 1L) |>
        dplyr::mutate(
          stat = "method",
          stat_name = "method",
          stat_label = "method",
          stat =
            dplyr::case_when(
              is_empty(adj.vars) ~ list("One-way ANOVA"),
              TRUE ~ list("ANCOVA")
            ),
          fmt_fun = list(NULL)
        )
    ) |>
    dplyr::select(-cards::all_ard_variables("levels")) |>
    dplyr::mutate(
      group1 = .env$by,
      variable = .env$variable,
      .before = 1L
    )
}


add_p_test_cohens_d <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars"), ...)

  rlang::inject(
    cardx::ard_effectsize_cohens_d(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      conf.level = conf.level,
      verbose = FALSE,
      !!!test.args
    )
  )
}

add_p_test_paired_cohens_d <- function(data, variable, by, test.args, group, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("adj.vars"), ...)
  check_length(group, 1L)
  warn_unbalanced_pairs(data, by, variable, group)

  rlang::inject(
    cardx::ard_effectsize_paired_cohens_d(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(group),
      conf.level = conf.level,
      verbose = FALSE,
      !!!test.args
    )
  )
}

add_p_test_hedges_g <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars"), ...)

  rlang::inject(
    cardx::ard_effectsize_hedges_g(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      conf.level = conf.level,
      verbose = FALSE,
      !!!test.args
    )
  )
}

add_p_test_paired_hedges_g <- function(data, variable, by, test.args, group, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("adj.vars"), ...)
  check_length(group, 1L)
  warn_unbalanced_pairs(data, by, variable, group)

  rlang::inject(
    cardx::ard_effectsize_paired_hedges_g(
      data = data,
      variable = all_of(variable),
      by = all_of(by),
      id = all_of(group),
      conf.level = conf.level,
      verbose = FALSE,
      !!!test.args
    )
  )
}

add_p_test_smd <- function(data, variable, by, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("group", "adj.vars", "test.args"), ...)

  cardx::ard_smd_smd(
    data = data,
    variable = all_of(variable),
    by = all_of(by),
    std.error = TRUE
  )
}

add_p_test_emmeans <- function(data, variable, by, adj.vars = NULL, conf.level = 0.95,
                               type, group = NULL, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("test.args"), ...)

  if (!is_empty(group)) check_pkg_installed("lme4", reference_pkg = "cardx")
  if (inherits(data, "survey.design")) check_pkg_installed("survey", reference_pkg = "cardx")

  # checking inputs
  if (!type %in% c("continuous", "dichotomous")) {
    cli::cli_abort("Variable {.val {variable}} must be summary type 'continuous' or 'dichotomous'", call = get_cli_abort_call())
  }
  if (dplyr::n_distinct(.extract_data_frame(data)[[by]], na.rm = TRUE) != 2) {
    cli::cli_abort("The {.arg by} argument must have exactly 2 levels.", call = get_cli_abort_call())
  }
  if (type %in% "dichotomous" && dplyr::n_distinct(.extract_data_frame(data)[[by]], na.rm = TRUE) != 2) {
    cli::cli_abort("Variable {.val {variable}} must have exactly 2 levels.", call = get_cli_abort_call())
  }
  if (inherits(data, "survey.design") && !is_empty(group)) {
    cli::cli_abort("Cannot use {.arg group} argument with {.val emmeans} and survey data.", call = get_cli_abort_call())
  }

  # assembling formula
  # styler: off
  termlabels <-
    if (is_empty(group)) cardx::bt(c(by, adj.vars))
  else c(cardx::bt(by), cardx::bt(adj.vars), glue::glue("(1 | {cardx::bt(group)})"))
  response <-
    if (type == "dichotomous") glue::glue("as.factor({cardx::bt(variable)})")
  else cardx::bt(variable)
  formula <- stats::reformulate(termlabels, response)
  # styler: on

  # set regression function and any additional arguments
  # styler: off
  method.args <- expr(list())
  package <- "base"
  if (is.data.frame(data) && is_empty(group) && type %in% c("continuous", "continuous2")) {
    method <- "lm"
  }
  else if (is.data.frame(data) && is_empty(group) && type %in% "dichotomous") {
    method <- "glm"
    method.args <- expr(list(family = stats::binomial()))
  }
  else if (inherits(data, "survey.design")) {
    package <- "survey"
    method <- "svyglm"
    if (type %in% "dichotomous") method.args <- expr(list(family = stats::binomial()))
  }
  else if (is.data.frame(data) && !is_empty(group)) {
    package <- "lme4"
    if (type %in% "dichotomous") {
      method <- "glmer"
      method.args <- expr(list(family = stats::binomial()))
    }
    else method <- "lmer"
  }
  # styler: on

  cardx::ard_emmeans_mean_difference(
    data = data,
    formula = formula,
    method = method,
    method.args = !!method.args,
    package = package,
    response_type = type,
    conf.level = conf.level,
    primary_covariate = by
  ) |>
    dplyr::select(-cards::all_ard_variables("levels")) |>
    dplyr::mutate(
      group1 = .env$by,
      variable = .env$variable,
      .before = 1L
    )
}

add_p_test_ancova_lme4 <- function(data, variable, by, group, conf.level = 0.95, adj.vars = NULL, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("test.args"), ...)
  check_length(group, 1L)

  # reverse coding the 'by' variable
  data[[by]] <- fct_rev(factor(data[[by]]))

  # building model
  cardx::ard_regression_basic(
    x =
      cardx::construct_model(
        data = data,
        formula =
          stats::reformulate(
            termlabels = c(cardx::bt(c(by, adj.vars)), glue("(1 | {cardx::bt(group)})")),
            response = cardx::bt(variable)
          ),
        method = "lmer",
        package = "lme4"
      ),
    effects = "fixed"
  ) |>
    dplyr::filter(
      .data$variable %in% .env$by,
      .data$stat_name %in% c("estimate", "std.error", "statistic", "conf.low", "conf.high", "p.value")
    ) %>%
    dplyr::bind_rows(
      .,
      dplyr::filter(., dplyr::row_number() == 1L) |>
        dplyr::mutate(
          stat = "method",
          stat_name = "method",
          stat_label = "method",
          stat =
            dplyr::case_when(
              is_empty(adj.vars) ~ list("One-way ANOVA with random intercept"),
              TRUE ~ list("ANCOVA with random intercept")
            ),
          fmt_fun = list(NULL)
        )
    ) |>
    dplyr::select(-cards::all_ard_variables("levels")) |>
    dplyr::mutate(
      group1 = .env$by,
      variable = .env$variable,
      .before = 1L
    )
}

# tbl_svysummary ---------------------------------------------------------------
add_p_test_svy.t.test <- function(data, variable, by, conf.level = 0.95, ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("test.args"), ...)

  cardx::ard_survey_svyttest(
    data = data,
    variable = all_of(variable),
    by = all_of(by),
    conf.level = conf.level
  )
}

add_p_test_svy.svyranktest <- function(data,
                                       variable,
                                       by,
                                       test = c("wilcoxon", "vanderWaerden", "median", "KruskalWallis"), ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("test.args"), ...)
  test <- arg_match(test)


  cardx::ard_survey_svyranktest(
    data = data,
    variable = all_of(variable),
    by = all_of(by),
    test = test
  )
}

add_p_test_svychisq.test <- function(data,
                                      variable,
                                      by,
                                      statistic = c("F", "Chisq", "Wald", "adjWald", "lincom",
                                                    "saddlepoint", "wls-score"), ...) {
  check_pkg_installed("cardx", reference_pkg = "gtsummary")
  check_empty(c("test.args"), ...)
  statistic <- arg_match(statistic)


  cardx::ard_survey_svychisq(
    data = data,
    variable = all_of(variable),
    by = all_of(by),
    statistic = statistic
  )
}

# add_p.tbl_continuous ---------------------------------------------------------
add_p_test_anova_2way <- function(data, variable, by, continuous_variable, ...) {
  stats::lm(
    formula = cardx::reformulate2(c(variable, by), response = continuous_variable),
    data = data |> dplyr::mutate(across(all_of(c(variable, by)), factor))
  ) |>
    broom::glance() |>
    dplyr::select("statistic", "p.value") |>
    mutate(method = "Two-way ANOVA")
}

add_p_test_tbl_summary_to_tbl_continuous <- function(data, variable, by, continuous_variable, test.args = NULL, group = NULL,
                                                     test_name, ...) {
  if (!is_empty(by)) {
    cli::cli_abort(
      "The {.val {test_name}} cannot be used when the {.arg by} argument is specified.",
      call = get_cli_abort_call()
    )
  }

  switch(test_name,
         "t.test" = add_p_test_t.test(
           data = data, variable = continuous_variable,
           by = variable, test.args = test.args, ...
         ),
         "aov" = add_p_test_aov(
           data = data, variable = continuous_variable,
           by = variable, test.args = test.args, ...
         ),
         "oneway.test" = add_p_test_oneway.test(
           data = data, variable = continuous_variable,
           by = variable, test.args = test.args, ...
         ),
         "kruskal.test" = add_p_test_kruskal.test(
           data = data, variable = continuous_variable,
           by = variable, test.args = test.args, ...
         ),
         "wilcox.test" = add_p_test_wilcox.test(
           data = data, variable = continuous_variable,
           by = variable, test.args = test.args, ...
         ),
         "lme4" = add_p_test_lme4(
           data = data, variable = continuous_variable,
           by = variable, test.args = test.args, group = group, ...
         ),
         "ancova" = add_p_test_ancova(
           data = data, variable = continuous_variable,
           by = variable, test.args = test.args, ...
         ),
         "ancova_lme4" = add_p_test_ancova_lme4(
           data = data, variable = continuous_variable,
           by = variable, test.args = test.args,
           group = group, ...
         )
  ) %||%
    stop("No test selected", call. = FALSE) %>%
    dplyr::select(-dplyr::any_of(c("estiamte", "conf.low", "conf.high")))
}

# tbl_survfit ------------------------------------------------------------------
# returns a list of the formula and data arg calls
extract_formula_data_call <- function(x) {
  # extracting survfit call
  survfit_call <- x$call %>% as.list()
  # index of formula and data
  call_index <- names(survfit_call) %in% c("formula", "data") %>% which()

  survfit_call[call_index]
}

add_p_tbl_survfit_survdiff <- function(data, variable, test.args, ...) {
  check_pkg_installed(c("cardx", "survival"), reference_pkg = "gtsummary")

  # formula and data calls
  formula_data_call <-
    extract_formula_data_call(data[[variable]]) |>
    imap(~safe_survfit_eval(.x))

  # calculate results
  inject(cardx::ard_survival_survdiff(!!!formula_data_call, !!!test.args))
}

add_p_tbl_survfit_logrank <- function(data, variable, ...) {
  add_p_tbl_survfit_survdiff(data = data, variable = variable, test.args = list(rho = 0))
}

add_p_tbl_survfit_tarone <- function(data, variable, ...) {
  add_p_tbl_survfit_survdiff(data = data, variable = variable, test.args = list(rho = 1.5))
}

add_p_tbl_survfit_petopeto_gehanwilcoxon <- function(data, variable, ...) {
  add_p_tbl_survfit_survdiff(data = data, variable = variable, test.args = list(rho = 1))
}

add_p_tbl_survfit_coxph <- function(data, variable, test_type = c("log", "sc", "wald"), test.args, ...) {
  check_pkg_installed(c("cardx", "survival", "broom"), reference_pkg = "gtsummary")
  test_type <- arg_match(test_type)

  # formula and data calls
  formula_data_call <-
    extract_formula_data_call(data[[variable]]) |>
    imap(~safe_survfit_eval(.x))

  inject(survival::coxph(!!!formula_data_call, !!!test.args)) |>
    broom::glance() |>
    dplyr::select(ends_with(paste0(".", test_type))) |>
    dplyr::rename_with(.fn = ~str_remove(.x, pattern = paste0(".", test_type, "$"))) |>
    dplyr::mutate(
      method =
        switch(
          test_type,
          "log" = "Cox regression (LRT)",
          "wald" = "Cox regression (Wald)",
          "sc" = "Cox regression (Score)"
        )
    )

}

# UTILITY FUNCTIONS ------------------------------------------------------------
.extract_data_frame <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  x$variables # return survey object data frame
}

warn_unbalanced_pairs <- function(data, by, variable, group) {
  balanced_pairs <-
    data[c(group, by, variable)] |>
    tidyr::drop_na() |>
    tidyr::pivot_wider(
      id_cols = all_of(group),
      names_from = all_of(by),
      values_from = all_of(variable)
    ) |>
    dplyr::select(-all_of(group)) |>
    dplyr::mutate(across(everything(), is.na)) |>
    apply(MARGIN = 1, FUN = sum) |>
    keep(~ . == 1L) |>
    is_empty()

  if (!balanced_pairs) {
    cli::cli_warn(
      "Some observations included in the stratified summary statistics were omitted
       from the comparison due to unbalanced missingness within group."
    )
  }
  invisible()
}

check_empty <- function(x, variable = get("variable", envir = caller_env()),
                        call = get_cli_abort_call(), ...) {
  dots <- dots_list(...)
  walk(
    x,
    \(.x) {
      if (!is_empty(dots[[.x]])) {
        cli::cli_abort(
          message = "The {.arg {.x}} argument is not used for the test selected for of variable {.val {variable}}.",
          call = call
        )
      }
    }
  )

  invisible()
}
