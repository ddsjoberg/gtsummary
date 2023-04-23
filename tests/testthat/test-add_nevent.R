skip_on_cran()
skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

fit_cox <- survival::coxph(survival::Surv(time, status) ~ sex, survival::lung)
fit_glm <- glm(response ~ trt, trial, family = binomial)

test_that("add_nevent after tbl_regression creates output without error/warning", {
  # cox model
  expect_error(
    res <-
      fit_cox %>%
      tbl_regression() %>%
      add_nevent(),
    NA
  )
  expect_snapshot(res %>% as.data.frame())

  # glm model
  expect_error(
    res <-
      fit_glm %>%
      tbl_regression() %>%
      add_nevent(),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
})



test_that("add_nevent after tbl_uvregression creates output without error/warning", {
  # cox model
  expect_error(
    res <-
      tbl_uvregression(
        trial,
        method = survival::coxph,
        y = survival::Surv(ttdeath, death),
      ) %>%
      add_nevent(),
    NA
  )
  expect_snapshot(res %>% as.data.frame())


  # glm model
  expect_error(
    res <-
      tbl_uvregression(
        trial,
        method = glm,
        y = response,
        method.args = list(family = binomial)
      ) %>%
      add_nevent(),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
})

test_that("add_nevent error with bad inputs", {
  expect_error(
    lm(hp ~ mpg, mtcars) %>%
      tbl_regression() %>%
      add_nevent(),
    NULL
  )
  expect_error(
    lme4::lmer(hp ~ mpg + (1 | cyl), mtcars) %>%
      tbl_regression() %>%
      add_nevent(),
    NULL
  )
})

# add_nevent.tbl_surfit --------------------------------------------------------

test_that("add_nevent.tbl_surfit", {
  tbl_survfit <-
    list(
      survival::survfit(survival::Surv(ttdeath, death) ~ 1, trial),
      survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)
    ) %>%
    tbl_survfit(times = c(12, 24))

  expect_error(
    res <- add_nevent(tbl_survfit),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
})

# add_nevent.tbl_regression ---------------------------------------------------------
test_that("add_nevent.tbl_regression", {
  tbl <-
    glm(response ~ grade + age, trial, family = binomial) %>%
    tbl_regression()

  expect_error(
    res <- tbl %>% add_nevent(), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl %>% add_nevent(location = "level"), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl %>% add_nevent(location = c("label", "level")), NA
  )
  expect_snapshot(res %>% as.data.frame())
})

# add_nevent.tbl_uvregression ---------------------------------------------------------
test_that("add_nevent.tbl_regression", {
  tbl <-
    trial %>%
    select(response, age, grade) %>%
    tbl_uvregression(
      y = response,
      method = glm,
      method.args = list(family = binomial)
    )

  expect_error(
    res <- tbl %>% add_nevent(), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl %>% add_nevent(location = "level"), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl %>% add_nevent(location = c("label", "level")), NA
  )
  expect_snapshot(res %>% as.data.frame())
})
