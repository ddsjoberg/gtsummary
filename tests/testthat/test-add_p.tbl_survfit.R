skip_on_cran()
# test-add_p.tbl_survfit -------------------------------------------------------
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

library(survival)

test_that("add_p.tbl_survfit works", {
  survfit_list <-
    list(
      survfit(Surv(ttdeath, death) ~ trt, trial),
      survfit(Surv(trial$ttdeath, trial$death) ~ trial$trt)
    )

  expect_error(
    survfit_list %>%
      purrr::map(~ tbl_survfit(.x, times = c(12, 24)) %>% add_p()),
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
    list(
      survfit(Surv(ttdeath, death) ~ trt, trial),
      survfit(Surv(ttdeath, death) ~ response, trial),
      survfit(Surv(ttdeath, death) ~ grade, trial),
      survfit(Surv(ttdeath, death) ~ stage, trial)
    ) %>%
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

  # tarone-ware
  tarone_stage <-
    survdiff(Surv(ttdeath, death) ~ stage, trial, rho = 1.5) %>%
    broom::glance() %>%
    dplyr::pull(p.value)


  expect_equal(
    tbl_survfit %>%
      add_p(
        test = list(
          trt ~ "logrank",
          response ~ "survdiff",
          grade ~ "petopeto_gehanwilcoxon",
          stage ~ "tarone"
        ),
        test.args = response ~ list(rho = 0.5)
      ) %>%
      purrr::pluck("meta_data", "p.value"),
    c(logrank_trt, grho_response, peto_grade, tarone_stage)
  )
})


test_that("add_p.tbl_survfit coxph family checks", {
  tbl_survfit <-
    list(
      survfit(Surv(ttdeath, death) ~ trt, trial),
      survfit(Surv(ttdeath, death) ~ response, trial),
      survfit(Surv(ttdeath, death) ~ grade, trial)
    ) %>%
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


  expect_equal(
    tbl_survfit %>%
      add_p(test = list(
        trt ~ "coxph_lrt",
        response ~ "coxph_wald",
        grade ~ "coxph_score"
      )) %>%
      purrr::pluck("meta_data", "p.value"),
    c(lrt_trt[[1]], wald_response[[1]], score_grade[[1]])
  )
})
