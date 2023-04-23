skip_on_cran()
# test-add_p.tbl_survfit -------------------------------------------------------
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

test_that("add_p.tbl_survfit works", {
  survfit_list <-
    list(
      survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial),
      survival::survfit(survival::Surv(trial$ttdeath, trial$death) ~ trial$trt)
    )

  expect_snapshot(
    survfit_list %>%
      purrr::map(~ tbl_survfit(.x, times = c(12, 24)) %>%
        add_p() %>%
        as.data.frame())
  )

  expect_snapshot(
    survfit_list %>%
      tbl_survfit(prob = c(seq(0.1, 0.9, by = 0.1))) %>% add_p() %>% as.data.frame(),
    NA
  )

  expect_snapshot(
    survfit_list[[1]] %>%
      tbl_survfit(prob = c(seq(0.1, 0.9, by = 0.1))) %>%
      add_p() %>%
      as.data.frame()
  )

  expect_snapshot(
    trial %>%
      select(trt, grade, ttdeath, death) %>%
      tbl_survfit(times = c(12, 24), y = survival::Surv(ttdeath, death)) %>%
      add_p() %>%
      as.data.frame()
  )
})



test_that("add_p.tbl_survfit survdiff family checks", {
  tbl_survfit <-
    list(
      survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial),
      survival::survfit(survival::Surv(ttdeath, death) ~ response, trial),
      survival::survfit(survival::Surv(ttdeath, death) ~ grade, trial),
      survival::survfit(survival::Surv(ttdeath, death) ~ stage, trial)
    ) %>%
    tbl_survfit(times = c(12, 24))
  expect_snapshot(tbl_survfit %>% as.data.frame())

  # logrank
  logrank_trt <-
    survival::survdiff(survival::Surv(ttdeath, death) ~ trt, trial) %>%
    broom::glance() %>%
    dplyr::pull(p.value)

  # G-rho
  grho_response <-
    survival::survdiff(survival::Surv(ttdeath, death) ~ response, trial, rho = 0.5) %>%
    broom::glance() %>%
    dplyr::pull(p.value)

  # pete, peto
  peto_grade <-
    survival::survdiff(survival::Surv(ttdeath, death) ~ grade, trial, rho = 1) %>%
    broom::glance() %>%
    dplyr::pull(p.value)

  # tarone-ware
  tarone_stage <-
    survival::survdiff(survival::Surv(ttdeath, death) ~ stage, trial, rho = 1.5) %>%
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
      survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial),
      survival::survfit(survival::Surv(ttdeath, death) ~ response, trial),
      survival::survfit(survival::Surv(ttdeath, death) ~ grade, trial)
    ) %>%
    tbl_survfit(times = c(12, 24))
  expect_snapshot(tbl_survfit %>% as.data.frame())

  # LRT
  lrt_trt <-
    survival::coxph(survival::Surv(ttdeath, death) ~ trt, trial) %>%
    broom::glance() %>%
    dplyr::pull(p.value.log)

  # Wald
  wald_response <-
    survival::coxph(survival::Surv(ttdeath, death) ~ response, trial) %>%
    broom::glance() %>%
    dplyr::pull(p.value.wald)

  # Score
  score_grade <-
    survival::coxph(survival::Surv(ttdeath, death) ~ grade, trial) %>%
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
