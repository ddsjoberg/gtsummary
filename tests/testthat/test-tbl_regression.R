skip_on_cran()
skip_if_not(broom.helpers::.assert_package("Hmisc", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))

library(survival)
library(lme4)


mod_lm <- lm(hp ~ am, data = mtcars)
mod_survreg <- survreg(Surv(time, status) ~ age + ph.ecog, data = lung)
mod_logistic <- glm(response ~ age + stage, trial, family = binomial)
mod_poisson <- glm(count ~ age + trt,
                   trial %>% dplyr::mutate(count = sample.int(20, size = nrow(trial), replace = TRUE)),
                   family = poisson
)
mod_lmer <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
mod_glmer <- glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial)

mod_lm_interaction <- lm(age ~ trt * grade * response, data = trial)

lung2 <- lung
Hmisc::label(lung2$sex) <- "Gender"
Hmisc::label(lung2$age) <- "AGE"
cox_hmisclbl <- coxph(Surv(time, status) ~ age + sex, data = lung2)


test_that("glm: logistic and poisson regression", {
  expect_error(tbl_regression(mod_logistic), NA)
  expect_warning(tbl_regression(mod_logistic), NA)
  expect_error(tbl_regression(mod_poisson, show_single_row = "trt"), NA)
  expect_warning(tbl_regression(mod_poisson, show_single_row = "trt"), NA)
  expect_equal(
    tbl_regression(mod_logistic)$table_styling$header %>% filter(column == "estimate") %>% pull(label),
    "**log(OR)**"
  )
  expect_equal(
    tbl_regression(mod_poisson)$table_styling$header %>% filter(column == "estimate") %>% pull(label),
    "**log(IRR)**"
  )

  expect_false(
    "ci" %in% names(tbl_regression(mod_logistic, conf.int = FALSE) %>% as_tibble(col_labels = FALSE))
  )
  expect_error(tbl_regression(mod_logistic, exponentiate = TRUE), NA)
  expect_warning(tbl_regression(mod_logistic, exponentiate = TRUE), NA)
  expect_error(tbl_regression(mod_poisson, exponentiate = TRUE, show_single_row = "trt"), NA)
  expect_warning(tbl_regression(mod_poisson, exponentiate = TRUE, show_single_row = "trt"), NA)
  expect_equal(
    tbl_regression(mod_logistic, exponentiate = TRUE)$table_styling$header %>% filter(column == "estimate") %>% pull(label),
    "**OR**"
  )
  expect_equal(
    tbl_regression(mod_poisson, exponentiate = TRUE)$table_styling$header %>% filter(column == "estimate") %>% pull(label),
    "**IRR**"
  )
})

test_that("lm: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lm), NA)
  expect_warning(tbl_regression(mod_lm), NA)
})

test_that("lm with tidyfun: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lm, tidy_fun = broom::tidy), NA)
  expect_warning(tbl_regression(mod_lm, tidy_fun = broom::tidy), NA)
})


test_that("survreg: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_survreg), NA)
  expect_warning(tbl_regression(mod_survreg), NA)
})

test_that("lmer: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lmer), NA)
  expect_warning(tbl_regression(mod_lmer), NA)
})

test_that("glmer: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_glmer), NA)
  expect_warning(tbl_regression(mod_glmer), NA)
})

test_that("lm with interactions: no errors/warnings with standard use", {
  expect_error(tbl_regression(mod_lm_interaction), NA)
  expect_warning(tbl_regression(mod_lm_interaction), NA)
})

test_that("tbl_regression creates errors when non-function in input", {
  expect_error(
    tbl_regression(mod_lm_interaction, pvalue_fun = mtcars),
    NULL
  )
  expect_error(
    tbl_regression(mod_lm_interaction, estimate_fun = mtcars),
    NULL
  )
  expect_error(
    tbl_regression(mod_lm_interaction, tidy_fun = mtcars),
    NULL
  )
})


test_that("tbl_regression creates errors when inputs are wrong", {
  expect_error(
    tbl_regression(mod_lm_interaction, label = "Age"),
    NULL
  )
  expect_error(
    tbl_regression(mod_lm_interaction, label = list("Age")),
    NULL
  )
  expect_error(
    tbl_regression(mod_lm_interaction, label = list("age" ~ c("Age", "Two"))),
    NULL
  )
  expect_error(
    tbl_regression(mod_lm_interaction, include = "INCLUDE ME!"),
    NULL
  )
})

test_that("No errors/warnings when data is labelled using Hmisc", {
  expect_error(tbl_regression(cox_hmisclbl), NA)
  expect_warning(tbl_regression(cox_hmisclbl), NA)

  expect_equal(
    tbl_regression(cox_hmisclbl)$table_styling$header %>% filter(column == "estimate") %>% pull(label),
    "**log(HR)**"
  )
  expect_equal(
    tbl_regression(cox_hmisclbl, exponentiate = TRUE)$table_styling$header %>% filter(column == "estimate") %>% pull(label),
    "**HR**"
  )
})

test_that("show_single_row errors print", {
  expect_error(
    tbl_regression(mod_lm_interaction, show_single_row = "NOT_A_VA"),
    NULL
  )
  expect_error(
    tbl_regression(mod_lm_interaction, show_single_row = "grade"),
    NULL
  )
})


test_that("All labels print with cubic splines", {
  spline_fun <- Hmisc::rcspline.eval
  rsc_mod <- lm(age ~ spline_fun(marker, inclx = TRUE) + response, trial)

  expect_equal(
    tbl_regression(rsc_mod) %>% purrr::pluck("table_body", "label") %>% {
      sum(is.na(.))
    },
    0
  )
})


test_that("Testing lme4 results", {
  mod_glmer <- glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial)

  # tbl_regerssion runs without error
  expect_error(
    tbl_lme4 <- tbl_regression(mod_glmer,
                               exponentiate = TRUE,
                               conf.level = 0.90
    ),
    NA
  )

  # coefs are exponentiated properly
  expect_equal(
    coef(mod_glmer)[[1]] %>%
      {
        .[1, 2:ncol(.)]
      } %>% purrr::map_dbl(exp) %>% as.numeric(),
    tbl_lme4$table_body %>% pull(estimate) %>% discard(is.na)
  )
})


test_that("Interaction modifications", {
  # no error with interaction
  expect_error(
    tbl_i <- lm(age ~ factor(response) * marker, trial) %>%
      tbl_regression(
        show_single_row = `factor(response):marker`,
        label = `factor(response):marker` ~ "Interaction"
      ),
    NA
  )

  # checking modifications to table
  expect_equal(
    dplyr::filter(tbl_i$table_body, variable == "factor(response):marker") %>%
      dplyr::pull(label) %>% .[[1]],
    "Interaction"
  )

  expect_equal(
    dplyr::filter(tbl_i$table_body, variable == "factor(response):marker") %>%
      nrow(),
    1L
  )
})

test_that("tidymodels/parsnip/workflows", {
  expect_equal(
    parsnip::linear_reg() %>%
      parsnip::set_engine('lm') %>%
      parsnip::set_mode('regression') %>%
      parsnip::fit(age ~ grade + stage, data = trial) %>%
      tbl_regression() %>%
      purrr::pluck("table_body"),
    lm(age ~ grade + stage, data = trial) %>%
      tbl_regression() %>%
      purrr::pluck("table_body")
  )

  expect_equal(
    workflows::workflow() %>%
      workflows::add_model(parsnip::logistic_reg() %>% parsnip::set_engine("glm")) %>%
      workflows::add_formula(factor(response) ~ age + stage) %>%
      parsnip::fit(data = trial) %>%
      tbl_regression() %>%
      as_tibble(col_labels = FALSE) %>%
      select(estimate, ci),
    glm(response ~ age + stage, data = trial, family = binomial) %>%
      tbl_regression() %>%
      as_tibble(col_labels = FALSE) %>%
      select(estimate, ci) %>%
      dplyr::filter(!is.na(estimate))
  )
})

test_that("tidycrr models work", {
  mod <- tidycmprsk::crr(tidycmprsk::Surv(ttdeath, death_cr) ~  age + grade, tidycmprsk::trial)

  expect_error(
    tbl <- tbl_regression(mod, exponentiate = TRUE),
    NA
  )
  expect_equal(
    as_tibble(tbl, col_labels = FALSE)$estimate,
    c("1.01", NA, NA, "1.06", "1.54")
  )
  expect_error(add_global_p(tbl), NA)
})

test_that("cmprsk::crr models message", {
  set.seed(10)
  ftime <- rexp(200)
  fstatus <- sample(0:2,200,replace=TRUE)
  cov <- matrix(runif(600),nrow=200)
  dimnames(cov)[[2]] <- c('x1','x2','x3')
  mod <- cmprsk::crr(ftime,fstatus,cov)

  expect_message(tbl_regression(mod))
})
