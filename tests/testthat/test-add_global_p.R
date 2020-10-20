context("test-add_global_p")
testthat::skip_on_cran()

test_that("no errors/warnings with standard use after tbl_regression", {
  mod1 <- lm(hp ~ factor(cyl) + mpg + factor(am), mtcars)
  tbl1 <- tbl_regression(mod1)

  expect_warning(
    tbl1 %>% add_global_p(), NA
  )
  expect_equal(
    car::Anova(mod1) %>% select(last_col()) %>% pull() %>% discard(is.na),
    tbl1 %>% add_global_p(include = everything()) %>% pluck("table_body", "p.value") %>% discard(is.na)
  )

  expect_warning(
    tbl1 %>% add_global_p(keep = TRUE, type = "II"), NA
  )
  expect_equal(
    car::Anova(mod1, type = "II") %>% select(last_col()) %>% pull() %>% discard(is.na),
    tbl1 %>% add_global_p(include = everything(), type = "II") %>% pluck("table_body", "p.value") %>% discard(is.na)
  )
  # testing that p.values are kept with keep = TRUE (only one line without missing p-value)
  expect_equal(
    tbl1 %>% add_global_p(keep = TRUE, type = "II") %>%
      pluck("table_body") %>% filter(variable == "factor(cyl)") %>%
      pull("p.value") %>% {sum(is.na(.))},
    1L
  )

  expect_message(tbl1 %>% add_global_p(quiet = TRUE), NA)
  expect_message(tbl1 %>% add_global_p(quiet = FALSE), "*")
})

test_that("no errors/warnings with standard use after tbl_uvregression", {
  tbl2 <- trial %>% tbl_uvregression(method = lm, y = age)
  expect_error(
    tbl2 %>% add_global_p(), NA
  )
  expect_warning(
    tbl2 %>% add_global_p(), NA
  )

  expect_error(
    tbl2 %>% add_global_p(type = 2, keep = TRUE), NA
  )
  expect_warning(
    tbl2 %>% add_global_p(type = "II"), NA
  )
  expect_equal(
    lm(age ~ trt, trial) %>% car::Anova(type = "II") %>% select(last_col()) %>%
      pull() %>% discard(is.na),
    tbl2 %>% add_global_p(type = "II", include = trt) %>% pluck("table_body", "p.value", 1)
  )

  expect_message(tbl2 %>% add_global_p(quiet = TRUE), NA)
  expect_message(tbl2 %>% add_global_p(quiet = FALSE), "*")
})


test_that("no errors/warnings with standard use after tbl_regression with non-standard names", {
  mod1 <- lm(hp ~ factor(`number of cylinders`) * `miles per galon` + factor(`type of transmission`), mtcars %>% rename(`miles per galon` = mpg, `type of transmission` = am, `number of cylinders` = cyl))
  tbl1 <- tbl_regression(mod1)

  expect_warning(
    tbl1 %>% add_global_p(), NA
  )
  expect_equal(
    car::Anova(mod1, type = "II") %>% select(last_col()) %>% pull() %>% discard(is.na),
    tbl1 %>% add_global_p(include = everything(), type = "II") %>% pluck("table_body", "p.value") %>% discard(is.na)
  )
  expect_equal(
    car::Anova(mod1, type = "III") %>% select(last_col()) %>% pull() %>% discard(is.na) %>% .[-1],
    tbl1 %>% add_global_p(keep = FALSE, type = "III") %>% pluck("table_body", "p.value") %>% discard(is.na)
  )

  # testing that p.values are kept with keep = TRUE (only one line without missing p-value)
  expect_equal(
    tbl1 %>% add_global_p(keep = TRUE, type = "II") %>%
      pluck("table_body") %>% filter(variable == "factor(`number of cylinders`)") %>%
      pull("p.value") %>% {sum(is.na(.))},
    1L
  )

  expect_message(tbl1 %>% add_global_p(quiet = TRUE), NA)
  expect_message(tbl1 %>% add_global_p(quiet = FALSE), "*")
})
