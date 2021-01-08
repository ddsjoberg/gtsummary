test_that("tidy_select_variables() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables()

  # no change by default
  res2 <- res %>% tidy_select_variables()
  expect_equivalent(res, res2)

  # include
  res2 <- res %>% tidy_select_variables(include = "stage")
  expect_equivalent(
    res2$variable,
    c("(Intercept)", "stage", "stage", "stage")
  )
  res2 <- res %>% tidy_select_variables(include = c("grade", "trt"))
  expect_equivalent(
    res2$variable,
    c("(Intercept)", "grade", "grade", "trt")
  )

  # select and de-select
  expect_equivalent(
    res %>% tidy_select_variables(include = stage),
    res %>% tidy_select_variables(include = -c(grade, trt))
  )

  # tidyselect fns
  expect_equivalent(
    res %>% tidy_select_variables(include = contains("tage")),
    res %>% tidy_select_variables(include = stage)
  )

  # testing vars() selector
  expect_equivalent(
    res %>% tidy_select_variables(include = vars(grade, trt)),
    res %>% tidy_select_variables(include = c(grade, trt))
  )

  # no error when none selected
  expect_error(
    res %>% tidy_select_variables(include = starts_with("zzzzzzz")),
    NA
  )
  expect_error(
    res %>% tidy_select_variables(include = -everything()),
    NA
  )
  expect_error(
    res %>% tidy_select_variables(include = where(is.character)),
    NA
  )
})


test_that("test tidy_select_variables() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod %>% broom::tidy() %>% tidy_select_variables())

  # could be apply twice (no error)
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_select_variables() %>% tidy_select_variables(),
    NA
  )
})

