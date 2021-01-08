test_that("tidy_remove_intercept() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_remove_intercept()
  expect_equal(
    res %>% dplyr::filter(var_type == "intercept") %>% nrow(),
    0L
  )
})


test_that("test tidy_remove_intercept() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod %>% broom::tidy() %>% tidy_remove_intercept())

  # could be apply twice (no error)
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_remove_intercept() %>% tidy_remove_intercept(),
    NA
  )
})
