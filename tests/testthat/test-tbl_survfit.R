skip_on_cran()
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

test_that("no errors/warnings with stratified variable", {
  s1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)
  s1.1 <- survival::survfit(survival::Surv(trial$ttdeath, trial$death) ~ trial$trt)
  expect_snapshot(
    tbl_survfit(
      s1,
      times = c(12, 24)
    ) %>%
      as.data.frame()
  )

  expect_snapshot(
    tbl_survfit(
      s1.1,
      times = c(12, 24)
    ) %>%
      as.data.frame()
  )
  expect_snapshot(
    tbl_survfit(
      s1,
      times = c(12, 24),
      reverse = TRUE
    ) %>%
      as.data.frame()
  )
  expect_warning(
    tbl_survfit(
      s1,
      times = c(12, 24)
    ),
    NA
  )
  expect_snapshot(
    tbl_survfit(
      s1,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ) %>%
      as.data.frame()
  )
  expect_warning(
    tbl_survfit(
      s1,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )
})

test_that("no errors/warnings with no stratified variable", {
  s2 <- survival::survfit(survival::Surv(ttdeath, death) ~ 1, trial)
  s2.1 <- survival::survfit(survival::Surv(trial$ttdeath, trial$death) ~ 1)
  expect_snapshot(
    tbl_survfit(
      s2,
      times = c(12, 24)
    ) %>%
      as.data.frame()
  )
  expect_snapshot(
    tbl_survfit(
      s2.1,
      times = c(12, 24)
    ) %>%
      as.data.frame()
  )
  expect_warning(
    tbl_survfit(
      s2,
      times = c(12, 24)
    ),
    NA
  )
  expect_snapshot(
    tbl_survfit(
      s2,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ) %>%
      as.data.frame()
  )
  expect_warning(
    tbl_survfit(
      s2,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )

  # expecting errors/messaging
  expect_message(
    tbl_survfit(
      s2,
      probs = c(0.2, 0.4),
      reverse = TRUE,
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    "*"
  )

  expect_error(
    tbl_survfit(
      s2,
      probs = c(0.2, 0.4),
      statistic = style_percent,
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    "*"
  )

  expect_error(
    tbl_survfit(
      s2,
      probs = c(0.2, 0.4),
      times = c(12, 24),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    "*"
  )

  expect_error(
    tbl <- tbl_survfit(trial, y = survival::Surv(ttdeath, death), include = c(grade, trt), times = 10),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())

  expect_message(
    tbl_survfit(survival::survfit(survival::Surv(ttdeath, death) ~ grade + trt, trial), times = 10),
    "*"
  )
})



# Competing Events Example --------

test_that("no errors/warnings with competing events", {
  # adding a competing event for death (cancer vs other causes)
  trial2 <- trial %>%
    dplyr::mutate(
      death_cr = dplyr::case_when(
        death == 0 ~ "censor",
        dplyr::row_number() %% 2 == 0L ~ "death from cancer",
        TRUE ~ "death other causes"
      ) %>% factor()
    )
  cr_1 <- survival::survfit(survival::Surv(ttdeath, death_cr) ~ 1, data = trial2)
  cr_2 <- survival::survfit(survival::Surv(ttdeath, death_cr) ~ grade, data = trial2)

  expect_snapshot(
    tbl_survfit(cr_1, times = c(12, 24)) %>% as.data.frame()
  )
  expect_error(
    summod2 <- tbl_survfit(cr_2, times = c(12, 24), label = ~"Tumor Grade"), NA
  )
  expect_snapshot(summod2 %>% as.data.frame())

  # output is identical in tbl_survfit and summary
  summod <- summary(cr_2, times = c(12, 24))
  outcome_index <- which(summod$states %in% "death from cancer")

  summod1b <- data.frame(
    strata = summod$strata, Time = summod$time,
    cancerdeath = summod$pstate[, outcome_index]
  )

  expect_equal(
    summod1b$cancerdeath,
    summod2$meta_data$df_stats[[1]]$estimate,
    ignore_attr = TRUE
  )
})

test_that("Factor ordering preserved", {
  trial2 <- mutate(trial,
    trt = ifelse(trt == "Drug A", 1, 0),
    trt = factor(trt, levels = c(0, 1), labels = c("Drug B", "Drug A"))
  )
  mod1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial2)

  tbl1 <- tbl_survfit(mod1, times = 12)
  tbl2 <- tbl_survfit(mod1, probs = 0.2)

  expect_equal(
    tbl1$table_body$label,
    c("trt", "Drug B", "Drug A")
  )
  expect_equal(
    tbl2$table_body$label,
    c("trt", "Drug B", "Drug A")
  )
})
