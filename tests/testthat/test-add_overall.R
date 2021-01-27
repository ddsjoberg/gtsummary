context("test-add_overall")
testthat::skip_on_cran()
library(survival)

test_that("no errors/warnings with standard use", {
  expect_error(mtcars %>% tbl_summary(by = am) %>% add_overall(), NA)
  expect_warning(mtcars %>% tbl_summary(by = am) %>% add_overall(), NA)

  expect_error(mtcars %>% tbl_summary(by = am) %>% add_overall(last = TRUE), NA)
  expect_warning(mtcars %>% tbl_summary(by = am) %>% add_overall(last = TRUE), NA)

  expect_error(iris %>% tbl_summary(by = Species) %>% add_overall(), NA)
  expect_warning(iris %>% tbl_summary(by = Species) %>% add_overall(), NA)
})

test_that("no errors/warnings with missing data", {
  expect_error(lung %>% tbl_summary(by = sex) %>% add_overall(), NA)
  expect_warning(lung %>% tbl_summary(by = sex) %>% add_overall(), NA)

  expect_error(lung %>% tbl_summary(by = sex) %>% add_overall(last = TRUE), NA)
  expect_warning(lung %>% tbl_summary(by = sex) %>% add_overall(last = TRUE), NA)
})


test_that("no errors/warnings with standard use for continuous 2", {
  expect_error(mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
  expect_warning(mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)

  expect_error(mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2") %>% add_overall(last = TRUE), NA)
  expect_warning(mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2") %>% add_overall(last = TRUE), NA)

  expect_error(iris %>% tbl_summary(by = Species, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
  expect_warning(iris %>% tbl_summary(by = Species, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
})

test_that("no errors/warnings with missing data for continuous 2", {
  expect_error(lung %>% tbl_summary(by = sex, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
  expect_warning(lung %>% tbl_summary(by = sex, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)

  expect_error(lung %>% tbl_summary(by = sex, type = all_continuous() ~ "continuous2") %>% add_overall(last = TRUE), NA)
  expect_warning(lung %>% tbl_summary(by = sex, type = all_continuous() ~ "continuous2") %>% add_overall(last = TRUE), NA)
})


test_that("no errors/warnings with missing data in by variable", {
  expect_error(trial %>% tbl_summary(by = response) %>% add_overall(), NA)
})

test_that("add_overall-works with ordered factors", {
  expect_error(
    trial %>%
      select(response, trt) %>%
      dplyr::mutate_at(
        vars(response, trt),
        ~ factor(., ordered = TRUE)
      ) %>%
      tbl_summary(by = trt) %>%
      add_overall(),
    NA
  )
})

test_that("no errors/warnings with standard use for tbl_svysummary", {
  t <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt)

  expect_error(t %>% add_overall(), NA)
  expect_warning(t %>% add_overall(), NA)

  expect_error(t %>% add_overall(last = TRUE), NA)
  expect_warning(t %>% add_overall(last = TRUE), NA)

  t <- Titanic %>%
    as.data.frame() %>%
    survey::svydesign(data = ., ids = ~1, weights = ~Freq) %>%
    tbl_svysummary(by = Survived)

  expect_error(t %>% add_overall(), NA)
  expect_warning(t %>% add_overall(), NA)
})

test_that("no errors/warnings with standard use for tbl_svysummary with continuous2", {
  t <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2")

  expect_error(t %>% add_overall(), NA)
  expect_warning(t %>% add_overall(), NA)

  expect_error(t %>% add_overall(last = TRUE), NA)
  expect_warning(t %>% add_overall(last = TRUE), NA)

  t <- Titanic %>%
    as.data.frame() %>%
    survey::svydesign(data = ., ids = ~1, weights = ~Freq) %>%
    tbl_svysummary(by = Survived, type = all_continuous() ~ "continuous2")

  expect_error(t %>% add_overall(), NA)
  expect_warning(t %>% add_overall(), NA)
})

test_that("errors produced when expected", {
  expect_error(mtcars %>% select(am, mpg) %>% tbl_summary() %>% add_overall(), "*")
  expect_error(mtcars %>% select(am, mpg) %>% tbl_summary(by = am) %>%
    add_stat_label() %>% add_overall(), "*")
})
