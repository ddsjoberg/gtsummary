# internal data ----------------------------------------------------------------
devtools::load_all(".") # this loads the gtsummary internal objects as well (to be saved into `df_add_p_tests`)
df_theme_elements <- readr::read_csv("data-raw/gtsummary_theme_elements.csv")
df_translations <- readxl::read_excel("data-raw/gtsummary_translated.xlsx")

special_char <- list()
special_char$interpunct <- "·"

# add_p.* data -----------------------------------------------------------------
# creating a tibble of all possible tests to select from all `add_p.*()` functions
df_add_p_tests <- tibble::tribble(
  ~class,           ~test_package, ~test_name,               ~test_fun,           ~fun_to_run,                                                          ~accept_dots, ~description,
  "tbl_summary",    "stats",       "t.test",                 stats::t.test,       rlang::expr(add_p_test_t.test),                                       TRUE,         "t-test",
  "tbl_summary",    "stats",       "aov",                    stats::aov,          rlang::expr(add_p_test_aov),                                          FALSE,        "One-way ANOVA",
  "tbl_summary",    "stats",       "kruskal.test",           stats::kruskal.test, rlang::expr(add_p_test_kruskal.test),                                 FALSE,        "Kruskal-Wallis test",
  "tbl_summary",    "stats",       "wilcox.test",            stats::wilcox.test,  rlang::expr(add_p_test_wilcox.test),                                  TRUE,         "Wilcoxon rank-sum test",
  "tbl_summary",    "stats",       "chisq.test",             stats::chisq.test,   rlang::expr(add_p_test_chisq.test),                                   TRUE,         "chi-square test of independence",
  "tbl_summary",    "stats",       "chisq.test.no.correct",  NULL,                rlang::expr(add_p_test_chisq.test.no.correct),                        FALSE,        "chi-square test of independence",
  "tbl_summary",    "stats",       "fisher.test",            stats::fisher.test,  rlang::expr(add_p_test_fisher.test),                                  TRUE,         "Fisher's exact test",
  "tbl_summary",    "lme4",        "lme4",                   lme4::glmer,         rlang::expr(add_p_test_lme4),                                         FALSE,        "random intercept logistic regression",
  "tbl_svysummary", "survey",      "svy.t.test",             survey::svyttest,    rlang::expr(add_p_test_svy.t.test),                                   FALSE,        "t-test adapted to complex survey samples",
  "tbl_svysummary", "survey",      "svy.wilcox.test",        survey::svyranktest, rlang::expr(add_p_test_svy.wilcox.test),                              FALSE,        "Wilcoxon rank-sum test for complex survey samples",
  "tbl_svysummary", "survey",      "svy.kruskal.test",       NULL,                rlang::expr(add_p_test_svy.kruskal.test),                             FALSE,        "Kruskal-Wallis rank-sum test for complex survey samples",
  "tbl_svysummary", "survey",      "svy.vanderwaerden.test", NULL,                rlang::expr(add_p_test_svy.vanderwaerden.test),                       FALSE,        "van der Waerden's normal-scores test for complex survey samples",
  "tbl_svysummary", "survey",      "svy.median.test",        NULL,                rlang::expr(add_p_test_svy.median.test),                              FALSE,        "Mood's test for the median for complex survey samples",
  "tbl_svysummary", "survey",      "svy.chisq.test",         survey::svychisq,    rlang::expr(add_p_test_svy.chisq.test),                               FALSE,        "chi-squared test with Rao & Scott's second-order correction",
  "tbl_svysummary", "survey",      "svy.adj.chisq.test",     NULL,                rlang::expr(add_p_test_svy.adj.chisq.test),                           FALSE,        "chi-squared test adjusted by a design effect estimate",
  "tbl_svysummary", "survey",      "svy.wald.test",          NULL,                rlang::expr(add_p_test_svy.wald.test),                                FALSE,        "Wald test of independence for complex survey samples",
  "tbl_svysummary", "survey",      "svy.adj.wald.test",      NULL,                rlang::expr(add_p_test_svy.adj.wald.test),                            FALSE,        "adjusted Wald test of independence for complex survey samples",
  "tbl_svysummary", "survey",      "svy.lincom.test",        NULL,                rlang::expr(add_p_test_svy.lincom.test),                              FALSE,        "test of independence using the exact asymptotic distribution for complex survey samples",
  "tbl_svysummary", "survey",      "svy.saddlepoint.test",   NULL,                rlang::expr(add_p_test_svy.saddlepoint.test),                         FALSE,        "test of independence using a saddlepoint approximation for complex survey samples",
  "tbl_survfit",    "survival",    "survdiff",               NULL,                rlang::expr(add_p_tbl_survfit_survdiff),                              TRUE,         "G-rho family test",
  "tbl_survfit",    "survival",    "logrank",                NULL,                rlang::expr(add_p_tbl_survfit_survdiff),                              FALSE,        "Log-rank test",
  "tbl_survfit",    "survival",    "petopeto_gehanwilcoxon", NULL,                rlang::expr(purrr::partial(add_p_tbl_survfit_survdiff, rho = 1)),     FALSE,        "Peto & Peto modification of Gehan-Wilcoxon test",
  "tbl_survfit",    "survival",    "coxph_lrt",              survival::coxph,     rlang::expr(purrr::partial(add_p_tbl_survfit_coxph, type = "lrt")),   TRUE,         "Cox regression (LRT)",
  "tbl_survfit",    "survival",    "coxph_wald",             NULL,                rlang::expr(purrr::partial(add_p_tbl_survfit_coxph, type = "wald")),  TRUE,         "Cox regression (Wald)",
  "tbl_survfit",    "survival",    "coxph_score",            NULL,                rlang::expr(purrr::partial(add_p_tbl_survfit_coxph, type = "score")), TRUE,         "Cox regression (Score)"
)


usethis::use_data(df_theme_elements,
                  df_translations,
                  special_char,
                  df_add_p_tests,
                  internal = TRUE, overwrite = TRUE)
