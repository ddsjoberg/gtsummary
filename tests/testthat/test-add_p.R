context("test-add_p.tbl_summary")
testthat::skip_on_cran()

test_that("add_p creates output without error/warning", {
  expect_error(
    tbl_summary(trial, by = grade) %>% add_p(),
    NA
  )

  expect_error(
    tbl_summary(mtcars, by = am) %>% add_p(),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = am) %>% add_p(),
    NA
  )

  expect_error(
    trial %>%
      tbl_summary(by = trt) %>%
      add_p(),
    NA
  )

  expect_warning(
    trial %>%
      tbl_summary(by = trt) %>%
      add_p(),
    NA
  )

  expect_message(
    trial %>%
      tbl_summary(by = trt) %>%
      add_p(),
    NA
  )

  expect_error(
    tbl_summary(trial, by = trt, include = -response) %>%
      add_p(group = response),
    NA
  )

  expect_message(
    tbl_summary(trial, by = trt) %>%
      add_p(test = everything() ~ "lme4", group = response),
    NULL
  )
})

test_that("add_p creates output without error/warning for continuous2", {
  expect_error(
    tbl_summary(trial, by = grade, type = all_continuous() ~ "continuous2") %>% add_p(),
    NA
  )

  expect_error(
    tbl_summary(mtcars, by = am, type = all_continuous() ~ "continuous2") %>% add_p(),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = am, type = all_continuous() ~ "continuous2") %>% add_p(),
    NA
  )

  expect_error(
    trial %>%
      tbl_summary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(),
    NA
  )

  expect_warning(
    trial %>%
      tbl_summary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(),
    NA
  )

  expect_message(
    trial %>%
      tbl_summary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(),
    NA
  )

  expect_error(
    tbl_summary(trial, by = trt, include = -response, type = all_continuous() ~ "continuous2") %>%
      add_p(group = response),
    NA
  )

  expect_message(
    tbl_summary(trial, by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(test = everything() ~ "lme4", group = response),
    NULL
  )
})

test_that("add_p creates errors with bad args", {
  expect_error(
    tbl_summary(mtcars, by = am) %>%
      add_p(pvalue_fun = mtcars),
    NULL
  )

  expect_error(
    tbl_summary(trial, by = grade, include = -response) %>%
      add_p(group = response),
    NULL
  )
})


test_that("add_p works well", {
  expect_error(
    tbl_summary(mtcars, by = am) %>%
      add_p(test = list(
        vars(mpg) ~ "t.test",
        disp ~ "aov",
        cyl ~ "chisq.test.no.correct"
      )),
    NA
  )

  expect_error(
    tbl_summary(mtcars, by = am) %>%
      add_p(test = list(
        vars(mpg) ~ t.test,
        disp ~ aov
      )),
    NA
  )
})

test_that("add_p with custom p-value function", {
  my_mcnemar <- function(data, variable, by, ...) {
    result <- list()
    result$p <- stats::mcnemar.test(data[[variable]], data[[by]])$p.value
    result$test <- "McNemar's test"
    result
  }

  expect_error(
    trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ "my_mcnemar"),
    NA
  )

  expect_error(
    trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ my_mcnemar),
    NA
  )
})

test_that("Wilcoxon and Kruskal-Wallis p-values match ", {
  t1 <- trial[c("trt", "age", "marker")] %>% tbl_summary(by = trt) %>% add_p(test = all_continuous() ~ wilcox.test)
  t2 <- trial[c("trt", "age", "marker")] %>% tbl_summary(by = trt) %>% add_p(test = all_continuous() ~ kruskal.test)
  expect_true(
    all(t1$meta_data$p.value - t2$meta_data$p.value < 0.001)
  )
})


# test-add_p.tbl_cross----------------------------------------------------------
context("test-add_p.tbl_cross")

test_that("add_p.tbl_cross", {
  expect_error(
    trial %>% tbl_cross(response, death) %>% add_p(),
    NA
  )
  expect_error(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(),
    NA
  )
  expect_error(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(source_note = TRUE),
    NA
  )
  expect_error(
    mtcars %>%
      tbl_cross(gear, carb) %>%
      add_p(test = "fisher.test"),
    NA
  )
})


# test-add_p.tbl_survfit -------------------------------------------------------
context("test-add_p.tbl_survfit")
library(survival)

test_that("add_p.tbl_survfit works", {
  survfit_list <-
    list(survfit(Surv(ttdeath, death) ~ trt, trial),
         survfit(Surv(trial$ttdeath, trial$death) ~ trial$trt))

  expect_error(
    survfit_list %>%
      purrr::map(~tbl_survfit(.x, times = c(12, 24)) %>% add_p()),
    NA
  )

  expect_error(
    survfit_list %>%
      tbl_survfit(prob = c(seq(0.1, 0.9, by = 0.1))) %>% add_p(),
    NA
  )

  expect_error(
    survfit_list[[1]] %>%
      tbl_survfit(prob = c(seq(0.1, 0.9, by = 0.1))) %>% add_p(),
    NA
  )

  expect_error(
    trial %>%
      select(trt, grade, ttdeath, death) %>%
      tbl_survfit(times = c(12, 24), y = Surv(ttdeath, death)) %>% add_p(),
    NA
  )
})



test_that("add_p.tbl_survfit survdiff family checks", {
  tbl_survfit <-
    list(survfit(Surv(ttdeath, death) ~ trt, trial),
         survfit(Surv(ttdeath, death) ~ response, trial),
         survfit(Surv(ttdeath, death) ~ grade, trial)) %>%
    tbl_survfit(times = c(12, 24))

  # logrank
  logrank_trt <-
    survdiff(Surv(ttdeath, death) ~ trt, trial) %>%
    broom::glance() %>%
    dplyr::pull(p.value)

  # G-rho
  grho_response <-
    survdiff(Surv(ttdeath, death) ~ response, trial, rho = 0.5) %>%
    broom::glance() %>%
    dplyr::pull(p.value)

  # pete, peto
  peto_grade <-
    survdiff(Surv(ttdeath, death) ~ grade, trial, rho = 1) %>%
    broom::glance() %>%
    dplyr::pull(p.value)


  expect_equivalent(
    tbl_survfit %>%
      add_p(test = list(trt ~ "logrank",
                        response ~ "survdiff",
                        grade ~ "petopeto_gehanwilcoxon"),
            test.args = response ~ list(rho = 0.5)) %>%
      purrr::pluck("meta_data", "p.value"),
    c(logrank_trt, grho_response, peto_grade)
  )
})


test_that("add_p.tbl_survfit coxph family checks", {
  tbl_survfit <-
    list(survfit(Surv(ttdeath, death) ~ trt, trial),
         survfit(Surv(ttdeath, death) ~ response, trial),
         survfit(Surv(ttdeath, death) ~ grade, trial)) %>%
    tbl_survfit(times = c(12, 24))

  # LRT
  lrt_trt <-
    coxph(Surv(ttdeath, death) ~ trt, trial) %>%
    broom::glance() %>%
    dplyr::pull(p.value.log)

  # Wald
  wald_response <-
    coxph(Surv(ttdeath, death) ~ response, trial) %>%
    broom::glance() %>%
    dplyr::pull(p.value.wald)

  # Score
  score_grade <-
    coxph(Surv(ttdeath, death) ~ grade, trial) %>%
    broom::glance() %>%
    dplyr::pull(p.value.sc)


  expect_equivalent(
    tbl_survfit %>%
      add_p(test = list(trt ~ "coxph_lrt",
                        response ~ "coxph_wald",
                        grade ~ "coxph_score")) %>%
      purrr::pluck("meta_data", "p.value"),
    c(lrt_trt, wald_response, score_grade)
  )
})



