context("test-as_tibble")

t1 <- trial %>% tbl_summary()
t2 <-
  glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
  tbl_regression(exponentiate = TRUE)
t3 <-
  tbl_uvregression(
    trial %>% dplyr::select(response, age, grade),
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE
  )

test_that("as_tibble works with standard use", {
  expect_error(as_tibble(t1), NA)
  expect_warning(as_tibble(t1), NA)
  expect_error(as_tibble(t1, return_calls = TRUE), NA)
  expect_warning(as_tibble(t1, return_calls = TRUE), NA)
  expect_error(as_tibble(t2), NA)
  expect_warning(as_tibble(t2), NA)
  expect_error(as_tibble(t3), NA)
  expect_warning(as_tibble(t3), NA)
})
