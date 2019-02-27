context("test-as_tibble")
library(survival)
library(lme4)



fmt_r_lm <- fmt_regression(lm(hp ~ am + gear + wt, data = mtcars))
fmt_r_survreg <- fmt_regression(survreg(Surv(time, status) ~ age + ph.ecog, data = lung))
fmt_r_lmer <- fmt_regression(lmer(Reaction ~ Days + (Days | Subject), sleepstudy))
fmt_r_glmer <- fmt_regression(glmer(am ~ hp + factor(cyl) + (1 | gear), mtcars, family = binomial))
fmt_uni_test <- fmt_uni_regression(
  trial,
  method = "glm",
  y = "response",
  method.args = list(family = binomial),
  exponentiate = TRUE
)

test_that("lm: no errors/warnings with standard use", {
  expect_error(as_tibble(fmt_r_lm), NA)
  expect_warning(as_tibble(fmt_r_lm), NA)
})


test_that("survreg: no errors/warnings with standard use", {
  expect_error(as_tibble(fmt_r_survreg), NA)
  expect_warning(as_tibble(fmt_r_survreg), NA)
})

test_that("lmer: no errors/warnings with standard use", {
  expect_error(as_tibble(fmt_r_lmer), NA)
  expect_warning(as_tibble(fmt_r_lmer), NA)
})

test_that("glmer: no errors/warnings with standard use", {
  expect_error(as_tibble(fmt_r_glmer), NA)
  expect_warning(as_tibble(fmt_r_glmer), NA)
})

test_that("uni_regression: no errors/warnings with standard use", {
  expect_error(as_tibble(fmt_uni_test), NA)
  expect_warning(as_tibble(fmt_uni_test), NA)
})

test_that("as_tibble creates output without error/warning (no by var)", {
  expect_error(
    purrr::map(list(mtcars, iris), ~ as_tibble(fmt_table1(.x))),
    NA
  )
  expect_warning(
    purrr::map(list(mtcars, iris), ~ as_tibble(fmt_table1(.x))),
    NA
  )
})


test_that("as_tibble creates output without error/warning (with by var)", {
  expect_error(
    as_tibble(fmt_table1(mtcars, by = "am")),
    NA
  )
  expect_warning(
    as_tibble(fmt_table1(mtcars, by = "am")),
    NA
  )
})
