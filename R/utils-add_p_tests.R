# add_p.tbl_summary ------------------------------------------------------------
add_p_test_t.test <- function(data, variable, by, test.args, ...) {
  expr(stats::t.test(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)),
                     data = !!data, !!!test.args)) %>%
    eval() %>%
    purrr::chuck("p.value")
}

add_p_test_aov <- function(data, variable, by,...) {
  rlang::expr(stats::aov(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)), data = !!data)) %>%
    eval() %>%
    summary() %>%
    pluck(1, "Pr(>F)", 1)
}

add_p_test_kruskal.test <- function(data, variable, by, ...) {
  stats::kruskal.test(data[[variable]], as.factor(data[[by]]))$p.value
}

add_p_test_wilcox.test <- function(data, variable, by, test.args, ...) {
  expr(stats::wilcox.test(!!rlang::sym(variable) ~ as.factor(!!rlang::sym(by)),
                          data = !!data, !!!test.args)) %>%
    eval() %>%
    purrr::chuck("p.value")
}

add_p_test_chisq.test <- function(data, variable, by, test.args, ...) {
  expr(stats::chisq.test(x = !!data[[variable]], y = as.factor(!!data[[by]]), !!!test.args)$p.value) %>%
    eval()
}

add_p_test_chisq.test.no.correct <- function(data, variable, by, ...) {
  add_p_test_chisq.test(data = data, variable = variable, by = by, test.args = list(correct = FALSE))
}

add_p_test_fisher.test <- function(data, variable, by, test.args, ...) {
  expr(stats::fisher.test(!!data[[variable]], as.factor(!!data[[by]]), !!!test.args)$p.value) %>%
    eval()
}

add_p_test_lme4 <- function(data, variable, by, group, type, ...) {
  assert_package("lme4", "add_p(test = variable ~ 'lme4')")
  if (is.null(group))
    glue("Error in 'lme4' test for variable '{variable}'. ",
         "`add_p(group=)` cannot by NULL") %>%
    stop(call. = FALSE)

  data <-
    select(data, variable, by, group) %>%
    filter(complete.cases(.))

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
  stats::anova(mod0, mod1)$"Pr(>Chisq)"[2]
}

# add_p.tbl_svysummary ---------------------------------------------------------
add_p_test_svy.chisq.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "F")$p.value
}

add_p_test_svy.adj.chisq.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "Chisq")$p.value
}

add_p_test_svy.wald.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "Wald")$p.value
}

add_p_test_svy.adj.wald.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "adjWald")$p.value
}

add_p_test_svy.lincom.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "lincom")$p.value
}

add_p_test_svy.saddlepoint.test <- function(data, variable, by, ...) {
  survey::svychisq(c_form(right = c(variable, by)), data, statistic = "saddlepoint")$p.value
}

add_p_test_svy.t.test <- function(data, variable, by, ...) {
  survey::svyttest(c_form(variable, by), data)$p.value
}

add_p_test_svy.wilcox.test <- function(data, variable, by, ...) {
  survey::svyranktest(c_form(variable, by), data, test = "wilcoxon")$p.value
}

add_p_test_svy.kruskal.test <- function(data, variable, by, ...) {
  survey::svyranktest(c_form(variable, by), data, test = "KruskalWallis")$p.value
}

add_p_test_svy.vanderwaerden.test <- function(data, variable, by, ...) {
  survey::svyranktest(c_form(variable, by), data, test = "vanderWaerden")$p.value
}

add_p_test_svy.median.test <- function(data, variable, by, ...) {
  survey::svyranktest(c_form(variable, by), data, test = "median")$p.value
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
  broom::glance(survdiff_result)$p.value
}

add_p_tbl_survfit_logrank <- function(data, ...) {
  add_p_tbl_survfit_survdiff(data, test.args = list(rho = 0))
}

add_p_tbl_survfit_petopeto_gehanwilcoxon <- function(data, ...) {
  add_p_tbl_survfit_survdiff(data, test.args = list(rho = 1))
}

add_p_tbl_survfit_coxph <- function(data, test_type, test.args, ...) {
  # formula and data calls
  formula_data_call <- extract_formula_data_call(data)

  # converting call into a survdiff call
  coxph_call <- rlang::call2(rlang::expr(survival::coxph), !!!formula_data_call, !!!test.args)

  # evaluating `survdiff()`
  coxph_result <- rlang::eval_tidy(coxph_call)

  # returning p-value
  pvalue_column <- switch(test_type,
                          "lrt" = "p.value.log",
                          "wald" = "p.value.wald",
                          "score" = "p.value.sc")
  broom::glance(coxph_result)[[pvalue_column]]
}
