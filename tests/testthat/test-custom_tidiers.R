skip_on_cran()

mod <- lm(age ~ marker + grade, trial)

test_that("no errors/warnings with tidy_standardize", {
  skip_if_not(broom.helpers::.assert_package("effectsize", pkg_search = "gtsummary", boolean = TRUE))
  expect_error(tbl_regression(mod, tidy_fun = tidy_standardize), NA)
})

test_that("no errors/warnings with tidy_bootstrap", {
  skip_if_not(broom.helpers::.assert_package("parameters", pkg_search = "gtsummary", boolean = TRUE))
  expect_error(tbl_regression(mod, tidy_fun = tidy_bootstrap), NA)
  expect_warning(tbl_regression(mod, tidy_fun = tidy_bootstrap), NA)
})


test_that("no errors/warnings with pool_and_tidy_mice", {
  skip_if_not(broom.helpers::.assert_package("mice", pkg_search = "gtsummary", boolean = TRUE))
  mod_mice <-
    suppressWarnings(mice::mice(trial, m = 2)) %>%
    with(glm(response ~ age + marker + grade, family = binomial))

  expect_error(tbl_regression(mod_mice), NA)
  expect_error(mice::pool(mod_mice) %>% tbl_regression(), NA)
  expect_output(mice::pool(mod_mice), NA)
})


test_that("no errors/warnings with tbl_regression.multinom", {
  skip_if_not(broom.helpers::.assert_package("nnet", pkg_search = "gtsummary", boolean = TRUE))
  expect_output(
    tbl_nnet <-
      nnet::multinom(grade ~ age, trial) %>%
      tbl_regression()
  )
  expect_error(
    tbl_nnet %>%
      as_tibble(),
    NA
  )
})

test_that("no errors/warnings with tbl_regression.gam", {
  skip_if_not(broom.helpers::.assert_package("mgcv", pkg_search = "gtsummary", boolean = TRUE))
  mod <- mgcv::gam(response ~ s(marker, age) + grade, data = trial, family = binomial)

  expect_error(
    mod %>% tidy_gam(),
    NA
  )

  # test the exp argument is working
  expect_equal(
    mod %>% tidy_gam(exponentiate = TRUE, conf.int = TRUE),
    mod %>%
      tidy_gam(exponentiate = FALSE, conf.int = TRUE) %>%
      dplyr::mutate_at(vars(any_of(c("estimate", "conf.low", "conf.high"))), exp)
  )

  expect_error(
    mod %>%
      tbl_regression(
        exponentiate = TRUE,
        label = `s(marker,age)` ~ "Smoothed marker/age"
      ),
    NA
  )
})

test_that("no errors/warnings with tidy_robust()", {
  skip_if_not(broom.helpers::.assert_package("parameters", pkg_search = "gtsummary", boolean = TRUE))
  skip_if_not(broom.helpers::.assert_package("insight", pkg_search = "gtsummary", boolean = TRUE))
  expect_error(
    glm(response ~ age + trt, trial, family = binomial) %>%
      tbl_regression(
        tidy_fun = purrr::partial(tidy_robust, vcov_estimation = "CL"),
        exponentiate = TRUE
      ),
    NA
  )
})

test_that("no errors/warnings with tidy_wald_test()", {
  skip_if_not(broom.helpers::.assert_package("aod", pkg_search = "gtsummary", boolean = TRUE))

  mod <- lm(age ~ stage + marker, trial)

  # why is the statistic different stage!?
  expect_equal(
    tidy_wald_test(mod) %>%
      dplyr::select(term, df, p.value),
    car::Anova(mod, type = 3) %>%
      broom::tidy() %>%
      dplyr::slice_head(n = 3) %>%
      dplyr::select(term, df, p.value),
    tolerance = 10e-2,
    ignore_attr = TRUE
  )
})



