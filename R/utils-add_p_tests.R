# add_p.tbl_summary ------------------------------------------------------------
add_p_test_t.test <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  .superfluous_args(variable, ...)
  expr(stats::t.test(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)),
    data = !!data, conf.level = !!conf.level, !!!test.args
  )) %>%
    eval() %>%
    broom::tidy()
}

add_p_test_aov <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)

  rlang::expr(stats::aov(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)), data = !!data)) %>%
    eval() %>%
    broom::tidy() %>%
    dplyr::mutate(method = "One-way ANOVA") %>%
    select(-"term") %>%
    dplyr::slice(1)
}

add_p_test_oneway.test <- function(data, variable, by, test.args, ...) {
  .superfluous_args(variable, ...)

  rlang::inject(
    stats::oneway.test(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)), data = data, !!!test.args)
  ) %>%
    broom::tidy()
}

add_p_test_mood.test <- function(data, variable, by, test.args, ...) {
  .superfluous_args(variable, ...)

  rlang::inject(
    stats::mood.test(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)), data = data, !!!test.args)
  ) %>%
    broom::tidy()
}

add_p_test_kruskal.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  stats::kruskal.test(data[[variable]], as.factor(data[[by]])) %>%
    broom::tidy()
}

add_p_test_wilcox.test <- function(data, variable, by, test.args, ...) {
  .superfluous_args(variable, ...)
  expr(stats::wilcox.test(as.numeric(!!rlang::sym(variable)) ~ as.factor(!!rlang::sym(by)),
    data = !!data, !!!test.args
  )) %>%
    eval() %>%
    broom::tidy() %>%
    mutate(
      method = case_when(
        .data$method == "Wilcoxon rank sum test with continuity correction" ~ "Wilcoxon rank sum test",
        TRUE ~ .data$method
      )
    )
}

add_p_test_chisq.test <- function(data, variable, by, test.args, ...) {
  .superfluous_args(variable, ...)
  expr(stats::chisq.test(x = !!data[[variable]], y = as.factor(!!data[[by]]), !!!test.args)) %>%
    eval() %>%
    broom::tidy() %>%
    mutate(
      method = case_when(
        .data$method == "Pearson's Chi-squared test with Yates' continuity correction" ~ "Pearson's Chi-squared test",
        TRUE ~ .data$method
      )
    )
}

add_p_test_chisq.test.no.correct <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  add_p_test_chisq.test(data = data, variable = variable, by = by, test.args = list(correct = FALSE))
}

add_p_test_fisher.test <- function(data, variable, by, test.args, conf.level = 0.95, ...) {
  .superfluous_args(variable, ...)
  expr(stats::fisher.test(!!data[[variable]], as.factor(!!data[[by]]), conf.level = !!conf.level, !!!test.args)) %>%
    eval() %>%
    broom::tidy() %>%
    mutate(
      method = case_when(
        .data$method == "Fisher's Exact Test for Count Data" ~ "Fisher's exact test",
        TRUE ~ .data$method
      )
    )
}

add_p_test_lme4 <- function(data, variable, by, group, type, ...) {
  .superfluous_args(variable, ...)
  assert_package("lme4", "add_p(test = variable ~ 'lme4')")
  if (is.null(group)) {
    glue(
      "Error in 'lme4' test for variable '{variable}'. ",
      "`add_p(group=)` cannot by NULL"
    ) %>%
      stop(call. = FALSE)
  }

  data <-
    select(data, all_of(c(variable, by, group))) %>%
    filter(stats::complete.cases(.))

  # creating formulas for base model (without variable) and full model
  formula0 <- paste0("as.factor(`", by, "`) ~ 1 + (1 | `", group, "`)")
  if (type %in% c("continuous", "continuous2")) {
    formula1 <- paste0("as.factor(`", by, "`) ~ `", variable, "` + (1 | `", group, "`)")
  } else {
    formula1 <- paste0("as.factor(`", by, "`) ~ as.factor(`", variable, "`) + (1 | `", group, "`)")
  }

  # building base and full models
  mod0 <- lme4::glmer(stats::as.formula(formula0),
    data = data, family = stats::binomial
  )
  mod1 <- lme4::glmer(stats::as.formula(formula1),
    data = data, family = stats::binomial
  )

  # returning p-value
  p.value <- stats::anova(mod0, mod1)$"Pr(>Chisq)"[2]
  tibble::tibble(p.value = p.value, method = "random intercept logistic regression")
}

add_p_tbl_summary_paired.t.test <- function(data, variable, by, group,
                                            test.args = NULL, conf.level = 0.95, ...) {
  quiet <- FALSE # need to add support for quiet later
  if (missing(group) || is.null(group)) stop("Paired tests require the `group=` argument.", call. = FALSE)
  .superfluous_args(variable, ...)
  # checking inputs
  if (length(data[[by]] %>% stats::na.omit() %>% unique()) != 2) {
    stop("`by=` must have exactly 2 levels", call. = FALSE)
  }
  if (dplyr::group_by_at(data, c(by, group)) %>% dplyr::count(name = "..n..") %>%
    pull("..n..") %>% max(na.rm = TRUE) > 1) {
    stop("'{variable}': There may only be one observation per `group=` per `by=` level.", call. = FALSE)
  }

  # reshaping data
  data_wide <-
    tidyr::pivot_wider(data,
      id_cols = all_of(group),
      names_from = all_of(by),
      values_from = all_of(variable)
    )

  # message about missing data
  if (!quiet && any(is.na(data_wide[[2]]) + is.na(data_wide[[3]]) == 1)) {
    glue(
      "Note for variable '{variable}': Some observations included in the ",
      "calculation of summary statistics ",
      "were omitted from the p-value calculation due to unbalanced missingness ",
      "within group."
    ) %>%
      rlang::inform()
  }


  # calculate p-value
  expr(stats::t.test(data_wide[[2]], data_wide[[3]],
    paired = TRUE,
    conf.level = !!conf.level, !!!test.args
  )) %>%
    eval() %>%
    broom::tidy()
}


add_p_test_mcnemar.test <- function(data, variable, by, group = NULL,
                                    test.args = NULL, ...) {
  quiet <- FALSE # need to add support for quiet later
  .superfluous_args(variable, ...)

  # note about deprecated method without a `group=` argument
  if (is.null(group)) {
    lifecycle::deprecate_stop(
      when = "1.5.0",
      what = "gtsummary::add_p(group='cannot be NULL with `mcnemar.test()`')",
      details =
        paste(
          "Follow the link for an example of the updated syntax",
          "or update the test to `test = varname ~ 'mcnemar.test.wide'`.",
          "https://www.danieldsjoberg.com/gtsummary/articles/gallery.html#paired-test"
        )
    )
  }

  # checking inputs
  if (length(data[[by]] %>% stats::na.omit() %>% unique()) != 2) {
    stop("`by=` must have exactly 2 levels", call. = FALSE)
  }
  if (dplyr::group_by_at(data, c(by, group)) %>% dplyr::count(name = "..n..") %>%
    pull("..n..") %>% max(na.rm = TRUE) > 1) {
    stop("'{variable}': There may only be one observation per `group=` per `by=` level.", call. = FALSE)
  }

  # reshaping data
  data_wide <-
    tidyr::pivot_wider(data,
      id_cols = all_of(group),
      names_from = all_of(by),
      values_from = all_of(variable)
    )

  # message about missing data
  if (quiet && any(is.na(data_wide[[2]]) + is.na(data_wide[[3]]) == 1)) {
    glue(
      "Note for variable '{variable}': Some observations included in the ",
      "calculation of summary statistics ",
      "were omitted from the p-value calculation due to unbalanced missingness ",
      "within group."
    ) %>%
      rlang::inform()
  }

  # calculate p-value
  rlang::expr(stats::mcnemar.test(data_wide[[2]], data_wide[[3]], !!!test.args)) %>%
    eval() %>%
    broom::tidy()
}

add_p_test_mcnemar.test_wide <- function(data, variable, by, test.args = NULL, ...) {
  .superfluous_args(variable, ...)
  rlang::expr(stats::mcnemar.test(data[[variable]], data[[by]], !!!test.args)) %>%
    eval() %>%
    broom::tidy()
}

add_p_tbl_summary_paired.wilcox.test <- function(data, variable, by, group,
                                                 test.args = NULL, conf.level = 0.95,
                                                 quiet = FALSE, ...) {
  .superfluous_args(variable, ...)
  if (missing(group) || is.null(group)) stop("Paired tests require the `group=` argument.", call. = FALSE)
  # checking inputs
  if (length(data[[by]] %>% stats::na.omit() %>% unique()) != 2) {
    stop("`by=` must have exactly 2 levels", call. = FALSE)
  }
  if (dplyr::group_by_at(data, c(by, group)) %>% dplyr::count(name = "..n..") %>%
    pull("..n..") %>% max(na.rm = TRUE) > 1) {
    stop("'{variable}': There may only be one observation per `group=` per `by=` level.", call. = FALSE)
  }

  # reshaping data
  data_wide <-
    tidyr::pivot_wider(data,
      id_cols = all_of(group),
      names_from = all_of(by),
      values_from = all_of(variable)
    )

  # message about missing data
  if (quiet && any(is.na(data_wide[[2]]) + is.na(data_wide[[3]]) == 1)) {
    glue(
      "Note for variable '{variable}': Some observations included in the ",
      "calculation of summary statistics ",
      "were omitted from the p-value calculation due to unbalanced missingness ",
      "within group."
    ) %>%
      rlang::inform()
  }

  # calculate p-value
  expr(stats::wilcox.test(as.numeric(data_wide[[2]]), as.numeric(data_wide[[3]]),
    paired = TRUE, !!!test.args
  )) %>%
    eval() %>%
    broom::tidy()
}

add_p_test_prop.test <- function(tbl, variable, test.args = NULL, conf.level = 0.95, ...) {
  .superfluous_args(variable, ...)
  df_counts <-
    tbl$meta_data %>%
    filter(variable == .env$variable) %>%
    purrr::pluck("df_stats", 1)

  expr(stats::prop.test(df_counts$n, df_counts$N, conf.level = !!conf.level, !!!test.args)) %>%
    eval() %>%
    broom::tidy() %>%
    mutate(estimate = .data$estimate1 - .data$estimate2) %>%
    mutate(
      method = case_when(
        .data$method == "2-sample test for equality of proportions with continuity correction" ~
          "Two sample test for equality of proportions",
        TRUE ~ .data$method
      )
    )
}

add_p_test_ancova <- function(data, variable, by, conf.level = 0.95, adj.vars = NULL, ...) {
  .superfluous_args(variable, ...)
  # reverse coding the 'by' variable
  data[[by]] <-
    switch(!is.factor(data[[by]]),
      forcats::fct_rev(factor(data[[by]]))
    ) %||%
    forcats::fct_rev(data[[by]])

  # assembling formula
  rhs <- c(by, adj.vars) %>%
    chr_w_backtick() %>%
    paste(collapse = " + ")
  f <- stringr::str_glue("{chr_w_backtick(variable)} ~ {rhs}") %>% as.formula()

  # building model
  stats::lm(formula = f, data = data) %>%
    broom.helpers::tidy_and_attach(conf.int = TRUE, conf.level = conf.level) %>%
    broom.helpers::tidy_remove_intercept() %>%
    dplyr::filter(.data$variable %in% .env$by) %>%
    select(
      "estimate", "std.error", "statistic",
      "conf.low", "conf.high", "p.value"
    ) %>%
    dplyr::mutate(
      method = case_when(
        is.null(adj.vars) ~ "One-way ANOVA",
        TRUE ~ "ANCOVA"
      )
    )
}

add_p_test_emmeans <- function(data, variable, by, type,
                               group = NULL,
                               conf.level = 0.95, adj.vars = NULL, ...) {
  .superfluous_args(variable, ...)

  assert_package("emmeans")
  if (!is.null(group)) assert_package("lme4")
  if (is_survey(data)) assert_package("survey")
  data_frame <-
    .extract_data_frame(data)

  # checking inputs
  if (!type %in% c("continuous", "dichotomous")) {
    stop("Variable must be summary type 'continuous' or 'dichotomous'", call. = FALSE)
  }
  if (length(data_frame[[by]] %>% stats::na.omit() %>% unique()) != 2) {
    stop("`by=` must have exactly 2 levels", call. = FALSE)
  }
  if (type %in% "dichotomous" &&
    length(data_frame[[variable]] %>% stats::na.omit() %>% unique()) != 2) {
    stop("`variable=` must have exactly 2 levels", call. = FALSE)
  }
  if (is_survey(data) && !is.null(group)) {
    stop("Cannot use `group=` argument with 'emmeans' and survey data.", call. = FALSE)
  }

  # assembling formula
  rhs <- c(by, adj.vars) %>%
    chr_w_backtick() %>%
    paste(collapse = " + ")
  if (!is.null(group)) {
    rhs <- paste0(rhs, "+ (1 | ", chr_w_backtick(group), ")")
  }
  f <-
    ifelse(
      type == "dichotomous",
      stringr::str_glue("as.factor({chr_w_backtick(variable)}) ~ {rhs}"),
      stringr::str_glue("{chr_w_backtick(variable)} ~ {rhs}")
    ) %>%
    as.formula()
  f_by <- rlang::inject(~ !!rlang::sym(chr_w_backtick(by)))

  type2 <-
    dplyr::case_when(
      is.data.frame(data) && is.null(group) ~ type,
      is_survey(data) && is.null(group) && type == "dichotomous" ~ "dichotomous_svy",
      is_survey(data) && is.null(group) && type == "continuous" ~ "continuous_svy",
      is.data.frame(data) && type == "dichotomous" ~ "dichotomous_mixed",
      is.data.frame(data) && type == "continuous" ~ "continuous_mixed",
    )

  model_fun <-
    switch(type2,
      "dichotomous" =
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

add_p_test_ancova_lme4 <- function(data, variable, by, group, conf.level = 0.95, adj.vars = NULL, ...) {
  assert_package("lme4")
  assert_package("broom.mixed")
  .superfluous_args(variable, ...)
  # reverse coding the 'by' variable
  data[[by]] <-
    switch(!is.factor(data[[by]]),
      forcats::fct_rev(factor(data[[by]]))
    ) %||%
    forcats::fct_rev(data[[by]])

  # assembling formula
  rhs <- c(by, adj.vars) %>%
    chr_w_backtick() %>%
    paste(collapse = " + ")
  f <- stringr::str_glue("{chr_w_backtick(variable)} ~ {rhs} + (1|{chr_w_backtick(group)})") %>% as.formula()

  # building model
  lme4::lmer(formula = f, data = data) %>%
    broom.helpers::tidy_and_attach(
      conf.int = TRUE,
      conf.level = conf.level,
      tidy_fun = broom.mixed::tidy
    ) %>%
    broom.helpers::tidy_remove_intercept() %>%
    dplyr::filter(.data$variable %in% .env$by) %>%
    select(any_of(c("estimate", "std.error", "statistic", "conf.low", "conf.high", "p.value"))) %>%
    dplyr::mutate(
      method = case_when(
        is.null(adj.vars) ~ "One-way ANOVA with random intercept",
        TRUE ~ "ANCOVA with random intercept"
      )
    )
}

add_p_test_cohens_d <- function(data, variable, by, conf.level = 0.95, test.args = NULL, ...) {
  assert_package("effectsize")
  .superfluous_args(variable, ...)
  f <- stringr::str_glue("{chr_w_backtick(variable)} ~ {chr_w_backtick(by)}") %>% as.formula()

  rlang::expr(effectsize::cohens_d(x = !!f, data = !!data, ci = !!conf.level, !!!test.args)) %>%
    eval() %>%
    tibble::as_tibble() %>%
    select(estimate = "Cohens_d", conf.low = "CI_low", conf.high = "CI_high") %>%
    dplyr::mutate(method = "Cohen's D")
}

add_p_test_smd <- function(data, variable, by, tbl, type,
                           conf.level = 0.95, ...) {
  # formulas from https://support.sas.com/resources/papers/proceedings12/335-2012.pdf
  assert_package("smd")
  if (is_survey(data)) assert_package("survey")

  if (.extract_data_frame(data)[[by]] %>% stats::na.omit() %>% unique() %>% length() != 2L) {
    stop("SMD requires exactly two levels of `by=` variable", call. = FALSE)
  }

  smd_args <-
    list(
      x = .extract_data_frame(data)[[variable]],
      g = .extract_data_frame(data)[[by]],
      std.error = TRUE,
      na.rm = TRUE
    )

  if (is_survey(data)) {
    smd_args <- c(smd_args, list(w = stats::weights(data)))
  }

  rlang::inject(smd::smd(!!!smd_args)) %>%
    select("estimate", "std.error") %>%
    mutate(
      conf.low = .data$estimate + stats::qnorm((1 - .env$conf.level) / 2) * .data$std.error,
      conf.high = .data$estimate - stats::qnorm((1 - .env$conf.level) / 2) * .data$std.error,
      method = "Standardized Mean Difference"
    )
}

# add_p.tbl_svysummary ---------------------------------------------------------
add_p_test_svy.chisq.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "F") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "chi-squared test with Rao & Scott's second-order correction")
}

add_p_test_svy.adj.chisq.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "Chisq") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "chi-squared test adjusted by a design effect estimate")
}

add_p_test_svy.wald.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "Wald") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "Wald test of independence for complex survey samples") %>%
    dplyr::mutate(dplyr::across(where(is.matrix), c))
}

add_p_test_svy.adj.wald.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "adjWald") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    dplyr::mutate_at(vars("statistic", "p.value"), as.numeric) %>%
    # default saves these cols as a matrix
    mutate(method = "adjusted Wald test of independence for complex survey samples") %>%
    dplyr::mutate(dplyr::across(where(is.matrix), c))
}

add_p_test_svy.lincom.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "lincom") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "test of independence using the exact asymptotic distribution for complex survey samples")
}

add_p_test_svy.saddlepoint.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "saddlepoint") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "test of independence using a saddlepoint approximation for complex survey samples")
}

add_p_test_svy.t.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svyttest(c_form(variable, by), data) %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "t-test adapted to complex survey samples")
}

add_p_test_svy.wilcox.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svyranktest(c_form(variable, by), data, test = "wilcoxon") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "Wilcoxon rank-sum test for complex survey samples")
}

add_p_test_svy.kruskal.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svyranktest(c_form(variable, by), data, test = "KruskalWallis") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "Kruskal-Wallis rank-sum test for complex survey samples")
}

add_p_test_svy.vanderwaerden.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svyranktest(c_form(variable, by), data, test = "vanderWaerden") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "van der Waerden's normal-scores test for complex survey samples")
}

add_p_test_svy.median.test <- function(data, variable, by, ...) {
  .superfluous_args(variable, ...)
  survey::svyranktest(c_form(variable, by), data, test = "median") %>%
    {
      suppressMessages(broom::tidy(.))
    } %>%
    mutate(method = "Mood's test for the median for complex survey samples")
}

# add_p.tbl_survfit ------------------------------------------------------------
# returns a list of the formula and data arg calls
extract_formula_data_call <- function(x) {
  # extracting survfit call
  survfit_call <- x$call %>% as.list()
  # index of formula and data
  call_index <- names(survfit_call) %in% c("formula", "data") %>% which()

  survfit_call[call_index]
}

add_p_tbl_survfit_survdiff <- function(data, variable, test.args, ...) {
  .superfluous_args(variable, ...)
  # formula and data calls
  formula_data_call <- extract_formula_data_call(data)

  # converting call into a survdiff call
  survdiff_call <- rlang::call2(rlang::expr(survival::survdiff), !!!formula_data_call, !!!test.args)

  # evaluating `survdiff()`
  survdiff_result <- rlang::eval_tidy(survdiff_call)

  # returning p-value
  broom::glance(survdiff_result) %>%
    dplyr::mutate(
      method =
        .purrr_when(
          is.null(test.args$rho) || test.args$rho == 0 ~ "Log-rank test",
          test.args$rho == 1 ~ "Peto & Peto modification of Gehan-Wilcoxon test",
          test.args$rho == 1.5 ~ "Tarone-Ware test",
          TRUE ~ stringr::str_glue("G-rho (\U03C1 = {test.args$rho}) test")
        )
    )
}

add_p_tbl_survfit_logrank <- function(data, variable, ...) {
  .superfluous_args(variable, ...)
  add_p_tbl_survfit_survdiff(data, test.args = list(rho = 0))
}

add_p_tbl_survfit_petopeto_gehanwilcoxon <- function(data, variable, ...) {
  .superfluous_args(variable, ...)
  add_p_tbl_survfit_survdiff(data, test.args = list(rho = 1))
}

add_p_tbl_survfit_tarone <- function(data, variable, ...) {
  .superfluous_args(variable, ...)
  add_p_tbl_survfit_survdiff(data, test.args = list(rho = 1.5))
}

add_p_tbl_survfit_coxph <- function(data, variable, test_type, test.args, ...) {
  .superfluous_args(variable, ...)
  # formula and data calls
  formula_data_call <- extract_formula_data_call(data)

  # converting call into a survdiff call
  coxph_call <- rlang::call2(rlang::expr(survival::coxph), !!!formula_data_call, !!!test.args)

  # evaluating `coxph()`
  coxph_result <- rlang::eval_tidy(coxph_call)

  # returning p-value
  method <- switch(test_type,
    "log" = "Cox regression (LRT)",
    "wald" = "Cox regression (Wald)",
    "sc" = "Cox regression (Score)"
  )
  broom::glance(coxph_result) %>%
    select(all_of(paste0(c("statistic.", "p.value."), test_type))) %>%
    set_names(c("statistic", "p.value")) %>%
    mutate(method = method)
}

# add_p.tbl_continuous ---------------------------------------------------------
add_p_test_anova_2way <- function(data, variable, by, continuous_variable, ...) {
  rlang::inject(
    stats::lm(!!sym(continuous_variable) ~ factor(!!sym(variable)) + factor(!!sym(by)),
      data = data
    )
  ) %>%
    broom::glance() %>%
    dplyr::select("statistic", "p.value") %>%
    mutate(method = "Two-way ANOVA")
}

add_p_test_tbl_summary_to_tbl_continuous <- function(data, variable, by, continuous_variable, test.args = NULL, group = NULL,
                                                     test_name, ...) {
  if (!is.null(by)) {
    stop("This test cannot be used with `by=` variable specified.", call. = FALSE)
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


# checks if test.args was passed incorrectly
.superfluous_args <- function(variable, ...) {
  superfluous_args <- list(...) %>%
    purrr::discard(is.null) %>%
    names() %>%
    intersect("test.args")
  if (!rlang::is_empty(superfluous_args)) {
    glue::glue(
      "Note for variable '{variable}': Argument(s) {quoted_list(superfluous_args)} ",
      "do not apply and were ignored. ",
      "See `?tests` for details."
    ) %>%
      stringr::str_wrap() %>%
      rlang::inform()
  }
}
