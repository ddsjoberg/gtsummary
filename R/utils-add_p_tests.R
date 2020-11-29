# add_p.tbl_summary ------------------------------------------------------------
add_p_test_t.test <- function(data, variable, by, test.args, ...) {
  expr(stats::t.test(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)),
                     data = !!data, !!!test.args)) %>%
    eval() %>%
    broom::tidy()
}

add_p_test_aov <- function(data, variable, by,...) {
  p.value <-
    rlang::expr(stats::aov(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)), data = !!data)) %>%
    eval() %>%
    summary() %>%
    pluck(1, "Pr(>F)", 1)

  tibble::tibble(p.value = p.value, method = "One-way ANOVA")
}

add_p_test_kruskal.test <- function(data, variable, by, ...) {
  stats::kruskal.test(data[[variable]], as.factor(data[[by]])) %>%
    broom::tidy()
}

add_p_test_wilcox.test <- function(data, variable, by, test.args, ...) {
  expr(stats::wilcox.test(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)),
                          data = !!data, !!!test.args)) %>%
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
  add_p_test_chisq.test(data = data, variable = variable, by = by, test.args = list(correct = FALSE))
}

add_p_test_fisher.test <- function(data, variable, by, test.args, ...) {
  expr(stats::fisher.test(!!data[[variable]], as.factor(!!data[[by]]), !!!test.args)) %>%
    eval() %>%
    broom::tidy() %>%
    mutate(
      method = case_when(
        .data$method == "Fisher's Exact Test for Count Data" ~ "Fisher's exact test",
        TRUE ~ .data$method)
    )
}

add_p_test_mcnemar.test <- function(data, variable, by, test.args = NULL, ...) {
  rlang::expr(stats::mcnemar.test(data[[variable]], data[[by]], !!!test.args)) %>%
    eval() %>%
    broom::tidy() %>%
    mutate(
      method = case_when(
        .data$method == "McNemar's Chi-squared test with continuity correction" ~ "McNemar's Chi-squared test",
        TRUE ~ .data$method
      )
    )
}

add_p_test_lme4 <- function(data, variable, by, group, type, ...) {
  assert_package("lme4", "add_p(test = variable ~ 'lme4')")
  if (is.null(group))
    glue("Error in 'lme4' test for variable '{variable}'. ",
         "`add_p(group=)` cannot by NULL") %>%
    stop(call. = FALSE)

  data <-
    select(data, variable, by, group) %>%
    filter(stats::complete.cases(.))

  # creating formulas for base model (without variable) and full model
  formula0 <- paste0("as.factor(`", by, "`) ~ 1 + (1 | `", group, "`)")
  if (type %in% c("continuous", "continuous2")) {
    formula1 <-  paste0("as.factor(`", by, "`) ~ `", variable, "` + (1 | `", group, "`)")
  } else {
    formula1 <-  paste0("as.factor(`", by, "`) ~ as.factor(`", variable, "`) + (1 | `", group, "`)")
  }

  # building base and full models
  mod0 <- lme4::glmer(stats::as.formula(formula0),
                      data = data, family = stats::binomial)
  mod1 <- lme4::glmer(stats::as.formula(formula1),
                      data = data, family = stats::binomial)

  # returning p-value
  p.value <- stats::anova(mod0, mod1)$"Pr(>Chisq)"[2]
  tibble::tibble(p.value = p.value, method = "random intercept logistic regression")
}

add_p_tbl_summary_paired.t.test <- function(data, variable, by, group,
                                            test.args = NULL, quiet = FALSE, ...) {
  # checking inputs
  if (length(data[[by]] %>% na.omit() %>% unique()) != 2)
    stop("`by=` must have exactly 2 levels", call. = FALSE)
  if (dplyr::group_by_at(data, c(by, group)) %>% dplyr::count(name = "..n..") %>%
      pull(.data$..n..) %>% max(na.rm = TRUE) > 1)
    stop("'{variable}': There may only be one observation per `group=` per `by=` level.", call. = FALSE)

  # reshaping data
  data_wide <-
    tidyr::pivot_wider(data,
                       id_cols = all_of(group),
                       names_from = all_of(by),
                       values_from = all_of(variable))

  # message about missing data
  if (quiet && any(is.na(data_wide[[2]]) + is.na(data_wide[[3]]) == 1))
    glue("Note for variable '{variable}': Some observations included in the ",
         "calculation of summary statistics ",
         "were omitted from the p-value calculation due to unbalanced missingness ",
         "within group.") %>%
    rlang::inform()


  # calculate p-value
  expr(stats::t.test(data_wide[[2]], data_wide[[3]], paired = TRUE, !!!test.args)) %>%
    eval() %>%
    broom::tidy()
}

add_p_tbl_summary_paired.wilcox.test <- function(data, variable, by, group,
                                                 test.args = NULL, quiet = FALSE, ...) {
  # checking inputs
  if (length(data[[by]] %>% na.omit() %>% unique()) != 2)
    stop("`by=` must have exactly 2 levels", call. = FALSE)
  if (dplyr::group_by_at(data, c(by, group)) %>% dplyr::count(name = "..n..") %>%
      pull(.data$..n..) %>% max(na.rm = TRUE) > 1)
    stop("'{variable}': There may only be one observation per `group=` per `by=` level.", call. = FALSE)

  # reshaping data
  data_wide <-
    tidyr::pivot_wider(data,
                       id_cols = all_of(group),
                       names_from = all_of(by),
                       values_from = all_of(variable))

  # message about missing data
  if (quiet && any(is.na(data_wide[[2]]) + is.na(data_wide[[3]]) == 1))
    glue("Note for variable '{variable}': Some observations included in the ",
         "calculation of summary statistics ",
         "were omitted from the p-value calculation due to unbalanced missingness ",
         "within group.") %>%
    rlang::inform()

  # calculate p-value
  expr(stats::wilcox.test(data_wide[[2]], data_wide[[3]], paired = TRUE, !!!test.args)) %>%
    eval() %>%
    broom::tidy()
}

# add_p.tbl_svysummary ---------------------------------------------------------
add_p_test_svy.chisq.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "F") %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "chi-squared test with Rao & Scott's second-order correction")
}

add_p_test_svy.adj.chisq.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "Chisq") %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "chi-squared test adjusted by a design effect estimate")
}

add_p_test_svy.wald.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "Wald") %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "Wald test of independence for complex survey samples")
}

add_p_test_svy.adj.wald.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "adjWald") %>%
    {suppressMessages(broom::tidy(.))} %>%
    dplyr::mutate_at(vars(statistic, p.value), as.numeric) %>% # default saves these cols as a matrix
    mutate(method = "adjusted Wald test of independence for complex survey samples")
}

add_p_test_svy.lincom.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "lincom") %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "test of independence using the exact asymptotic distribution for complex survey samples")
}

add_p_test_svy.saddlepoint.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "saddlepoint") %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "test of independence using a saddlepoint approximation for complex survey samples")
}

add_p_test_svy.t.test <- function(data, variable, by, ...) {
  survey::svyttest(c_form(variable, by), data) %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "t-test adapted to complex survey samples")
}

add_p_test_svy.wilcox.test <- function(data, variable, by, ...) {
  survey::svyranktest(c_form(variable, by), data, test = "wilcoxon") %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "Wilcoxon rank-sum test for complex survey samples")
}

add_p_test_svy.kruskal.test <- function(data, variable, by, ...) {
  survey::svyranktest(c_form(variable, by), data, test = "KruskalWallis") %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "Kruskal-Wallis rank-sum test for complex survey samples")
}

add_p_test_svy.vanderwaerden.test <- function(data, variable, by, ...) {
  survey::svyranktest(c_form(variable, by), data, test = "vanderWaerden") %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "van der Waerden's normal-scores test for complex survey samples")
}

add_p_test_svy.median.test <- function(data, variable, by, ...) {
  survey::svyranktest(c_form(variable, by), data, test = "median") %>%
    {suppressMessages(broom::tidy(.))} %>%
    mutate(method = "Mood's test for the median for complex survey samples")
}

# add_p.tbl_survfit ------------------------------------------------------------
# returns a list of the formula and data arg calls
extract_formula_data_call <- function(x) {
  #extracting survfit call
  survfit_call <- x$call %>% as.list()
  # index of formula and data
  call_index <- names(survfit_call) %in% c("formula", "data") %>% which()

  survfit_call[call_index]
}

add_p_tbl_survfit_survdiff <- function(data, test.args, ...) {
  # formula and data calls
  formula_data_call <- extract_formula_data_call(data)

  # converting call into a survdiff call
  survdiff_call <- rlang::call2(rlang::expr(survival::survdiff), !!!formula_data_call, !!!test.args)

  # evaluating `survdiff()`
  survdiff_result <- rlang::eval_tidy(survdiff_call)

  # returning p-value
  broom::glance(survdiff_result) %>%
    dplyr::mutate(method = "G-rho family test")
}

add_p_tbl_survfit_logrank <- function(data, ...) {
  add_p_tbl_survfit_survdiff(data, test.args = list(rho = 0)) %>%
    dplyr::mutate(method = "Log-rank test")
}

add_p_tbl_survfit_petopeto_gehanwilcoxon <- function(data, ...) {
  add_p_tbl_survfit_survdiff(data, test.args = list(rho = 1)) %>%
    dplyr::mutate(method = "Peto & Peto modification of Gehan-Wilcoxon test")
}

add_p_tbl_survfit_coxph <- function(data, test_type, test.args, ...) {
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
                   "sc" = "Cox regression (Score)")
  broom::glance(coxph_result) %>%
    select(all_of(paste0(c("statistic.", "p.value."), test_type))) %>%
    set_names(c("statistic", "p.value")) %>%
    mutate(method = method)

}
