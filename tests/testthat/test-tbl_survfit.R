context("test-tbl_survfit")
testthat::skip_on_cran()
library(survival)

s1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
test_that("no errors/warnings with stratified variable", {
  expect_error(
    tbl_survfit(
      s1,
      times = c(12, 24)
    ),
    NA
  )
  expect_error(
    tbl_survfit(
      s1,
      times = c(12, 24),
      reverse = TRUE
    ),
    NA
  )
  expect_warning(
    tbl_survfit(
      s1,
      times = c(12, 24)
    ),
    NA
  )
  expect_error(
    tbl_survfit(
      s1,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
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

s2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
test_that("no errors/warnings with no stratified variable", {
  expect_error(
    tbl_survfit(
      s2,
      times = c(12, 24)
    ),
    NA
  )
  expect_warning(
    tbl_survfit(
      s2,
      times = c(12, 24)
    ),
    NA
  )
  expect_error(
    tbl_survfit(
      s2,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )
  expect_warning(
    tbl_survfit(
      s2,
      probs = c(0.2, 0.4),
      estimate_fun = partial(style_sigfig, digits = 4)
    ),
    NA
  )
})


test_that("expecting errors/messaging", {
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
})



# Competing Events Example --------

test_that("no errors/warnings with competing events", {
  # adding a competing event for death (cancer vs other causes)
  trial2 <- trial %>%
    dplyr::mutate(
      death_cr = dplyr::case_when(
        death == 0 ~ "censor",
        runif(nrow(.)) < 0.5 ~ "death from cancer",
        TRUE ~ "death other causes"
      ) %>% factor()
    )
  cr_1 <- survfit(Surv(ttdeath, death_cr) ~ 1, data = trial2)
  cr_2 <- survfit(Surv(ttdeath, death_cr) ~ grade, data = trial2)

  expect_error(
    tbl_survfit(cr_1, times = c(12, 24)), NA
  )
  expect_error(
    tbl_survfit(cr_2, times = c(12, 24), label = "Tumor Grade"), NA
  )

  # output is idential in tbl_survfit and summary
  summod <- summary(cr_2, times = c(12,24))

  summod1b <- data.frame(strata = summod$strata,Time = summod$time,
                         cancerdeath = summod$pstate[,2])

  summod2 <- tbl_survfit(cr_2, times = c(12,24))

  expect_equal(summod1b$cancerdeath, summod2$table_stats$estimate)
})

