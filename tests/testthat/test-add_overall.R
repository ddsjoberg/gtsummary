skip_on_cran()
skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

test_that("no errors/warnings with standard use", {
  expect_error(res <- mtcars %>% tbl_summary(by = am) %>% add_overall(), NA)
  expect_warning(mtcars %>% tbl_summary(by = am) %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())

  expect_error(res <- mtcars %>% tbl_summary(by = am) %>% add_overall(last = TRUE), NA)
  expect_warning(mtcars %>% tbl_summary(by = am) %>% add_overall(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())

  expect_error(res <- iris %>% tbl_summary(by = Species) %>% add_overall(col_label = "**All Species**"), NA)
  expect_warning(iris %>% tbl_summary(by = Species) %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())
})

test_that("no errors/warnings with missing data", {
  expect_error(res <- survival::lung %>% tbl_summary(by = sex) %>% add_overall(), NA)
  expect_warning(survival::lung %>% tbl_summary(by = sex) %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())

  expect_error(res <- survival::lung %>% tbl_summary(by = sex) %>% add_overall(last = TRUE), NA)
  expect_warning(survival::lung %>% tbl_summary(by = sex) %>% add_overall(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())
})


test_that("no errors/warnings with standard use for continuous 2", {
  expect_error(res <- mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
  expect_warning(mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())

  expect_error(res <- mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2") %>% add_overall(last = TRUE), NA)
  expect_warning(mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2") %>% add_overall(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())

  expect_error(res <- iris %>% tbl_summary(by = Species, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
  expect_warning(iris %>% tbl_summary(by = Species, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())
})

test_that("no errors/warnings with missing data for continuous 2", {
  expect_error(res <- survival::lung %>% tbl_summary(by = sex, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
  expect_warning(survival::lung %>% tbl_summary(by = sex, type = all_continuous() ~ "continuous2") %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())

  expect_error(res <- survival::lung %>% tbl_summary(by = sex, type = all_continuous() ~ "continuous2") %>% add_overall(last = TRUE), NA)
  expect_warning(survival::lung %>% tbl_summary(by = sex, type = all_continuous() ~ "continuous2") %>% add_overall(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())
})


test_that("no errors/warnings with missing data in by variable", {
  expect_error(res <- trial %>% tbl_summary(by = response) %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())
})

test_that("add_overall-works with ordered factors", {
  expect_error(
    res <-
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
  expect_snapshot(res %>% as.data.frame())
})

test_that("no errors/warnings with standard use for tbl_svysummary", {
  skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))

  t <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt)

  expect_error(res <- t %>% add_overall(), NA)
  expect_warning(t %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())

  expect_error(res <- t %>% add_overall(last = TRUE), NA)
  expect_warning(t %>% add_overall(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())

  t <- Titanic %>%
    as.data.frame() %>%
    survey::svydesign(data = ., ids = ~1, weights = ~Freq) %>%
    tbl_svysummary(by = Survived)

  expect_error(res <- t %>% add_overall(), NA)
  expect_warning(t %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())
})

test_that("no errors/warnings with standard use for tbl_svysummary with continuous2", {
  skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))

  t <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2")

  expect_error(res <- t %>% add_overall(), NA)
  expect_warning(t %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())

  expect_error(res <- t %>% add_overall(last = TRUE), NA)
  expect_warning(t %>% add_overall(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())

  t <- Titanic %>%
    as.data.frame() %>%
    survey::svydesign(data = ., ids = ~1, weights = ~Freq) %>%
    tbl_svysummary(by = Survived, type = all_continuous() ~ "continuous2")

  expect_error(res <- t %>% add_overall(), NA)
  expect_warning(t %>% add_overall(), NA)
  expect_snapshot(res %>% as.data.frame())
})

test_that("errors produced when expected", {
  expect_error(mtcars %>% select(am, mpg) %>% tbl_summary() %>% add_overall(), "*")

  tbl <- trial[c("age", "trt")] %>% tbl_summary(by = trt)
  tbl$table_body$label[1] <- "Agggge, Years"
  expect_error(tbl %>% add_overall(), "*")
})

test_that("Users can modify statistics and digits arguments", {
  expect_equal(
    trial %>%
      select(trt, grade, response) %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_overall(
        statistic = ~"{p}% (n={n})",
        digits = ~ c(3, 0)
      ) %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull(stat_0),
    c(NA, "34.000% (n=68)", "34.000% (n=68)", "32.000% (n=64)", "31.606% (n=61)")
  )

  expect_equal(
    Titanic %>%
      as.data.frame() %>%
      survey::svydesign(data = ., ids = ~1, weights = ~Freq) %>%
      tbl_svysummary(by = Survived, include = c(Class, Sex)) %>%
      add_overall(
        statistic = ~"{p}% (n={n})",
        digits = ~ c(3, 0)
      ) %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull(stat_0),
    c(
      NA, "14.766% (n=325)", "12.949% (n=285)", "32.076% (n=706)",
      "40.209% (n=885)", NA, "78.646% (n=1,731)", "21.354% (n=470)"
    )
  )
})
