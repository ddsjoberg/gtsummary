skip_on_cran()
skip_if_not(is_pkg_installed(c("broom.helpers", "withr")))

test_that("tbl_regression.model_fit()", {
  skip_if_not(is_pkg_installed("parsnip"))

  expect_snapshot(
    parsnip::linear_reg() |>
      parsnip::set_engine("lm") |>
      parsnip::set_mode("regression") |>
      parsnip::fit(age ~ grade + stage, data = trial) |>
      tbl_regression() |>
      as.data.frame()
  )
})

test_that("tbl_regression.workflow()", {
  skip_if_not(is_pkg_installed(c("workflows", "parsnip")))

  expect_snapshot(
    workflows::workflow() |>
      workflows::add_model(parsnip::logistic_reg() |> parsnip::set_engine("glm")) |>
      workflows::add_formula(factor(response) ~ age + stage) |>
      parsnip::fit(data = trial) |>
      tbl_regression() |>
      as.data.frame()
  )
})

test_that("tbl_regression.survreg()", {
  skip_if_not(is_pkg_installed("survival"))

  expect_snapshot(
    survival::survreg(survival::Surv(time, status) ~ age + ph.ecog, data = survival::lung) |>
      tbl_regression() |>
      as.data.frame()
  )
})

test_that("tbl_regression.mira()", {
  withr::local_seed(11235)
  skip_if_not(is_pkg_installed("mice"))

  expect_snapshot(
    suppressWarnings(mice::mice(trial, m = 2)) |>
      with(lm(age ~ marker + grade)) |>
      tbl_regression() |>
      as.data.frame()
  )

  # proper message about not pooling the results before `tbl_regression()`
  expect_snapshot(
    suppressWarnings(mice::mice(trial, m = 2)) |>
      with(lm(age ~ marker + grade)) |>
      mice::pool() |>
      tbl_regression() |>
      as.data.frame()
  )
})

test_that("tbl_regression.lmerMod()", {
  skip_if_not(is_pkg_installed(c("lme4", "broom.mixed")))

  expect_snapshot(
    lme4::lmer(mpg ~ hp + (1 | cyl), mtcars) |>
      tbl_regression() |>
      as.data.frame()
  )
})

test_that("tbl_regression.gam()", {
  skip_if_not(is_pkg_installed("mgcv"))
  withr::local_package("mgcv")

  expect_snapshot(
    gam(mpg ~ s(hp) + factor(cyl), data = mtcars) |>
      tbl_regression() |>
      as.data.frame()
  )
})

test_that("tbl_regression.crr()", {
  skip_if_not(is_pkg_installed("cmprsk"))
  withr::local_package("cmprsk")
  withr::local_seed(10)

  expect_snapshot({
    set.seed(10)
    ftime <- rexp(200)
    fstatus <- sample(0:2, 200, replace = TRUE)
    cov <- matrix(runif(600), nrow = 200)
    dimnames(cov)[[2]] <- c("x1", "x2", "x3")

    crr(ftime, fstatus, cov) |>
      tbl_regression() |>
      as.data.frame()
  })
})

test_that("tbl_regression.multinom()", {
  skip_if_not(is_pkg_installed("nnet"))

  expect_snapshot(
    nnet::multinom(cyl ~ am, mtcars) |>
      tbl_regression() |>
      as.data.frame()
  )
})
