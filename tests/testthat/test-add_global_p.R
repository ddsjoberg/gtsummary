skip_on_cran()
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))

test_that("no errors/warnings with standard use after tbl_regression", {
  mod1 <- lm(hp ~ factor(cyl) + mpg + factor(am), mtcars)
  tbl1 <- tbl_regression(mod1)

  expect_warning(
    res <- tbl1 %>% add_global_p(), NA
  )
  expect_equal(
    car::Anova(mod1) %>% select(last_col()) %>% pull() %>% discard(is.na),
    tbl1 %>% add_global_p(include = everything()) %>% pluck("table_body", "p.value") %>% discard(is.na)
  )
  expect_snapshot(res %>% as.data.frame())

  expect_warning(
    res <- tbl1 %>% add_global_p(keep = TRUE, type = "II"), NA
  )
  expect_equal(
    car::Anova(mod1, type = "II") %>% select(last_col()) %>% pull() %>% discard(is.na),
    tbl1 %>% add_global_p(include = everything(), type = "II") %>% pluck("table_body", "p.value") %>% discard(is.na)
  )
  expect_snapshot(res %>% as.data.frame())

  # testing that p.values are kept with keep = TRUE (only one line without missing p-value)
  expect_equal(
    tbl1 %>% add_global_p(keep = TRUE, type = "II") %>%
      pluck("table_body") %>% filter(variable == "factor(cyl)") %>%
      pull("p.value") %>%
      {
        sum(is.na(.))
      },
    1L
  )

  expect_message(tbl1 %>% add_global_p(quiet = TRUE), NA)
})

test_that("no errors/warnings with standard use after tbl_uvregression", {
  tbl2 <- trial %>% tbl_uvregression(method = lm, y = age)
  expect_error(
    res <- tbl2 %>% add_global_p(), NA
  )
  expect_warning(
    tbl2 %>% add_global_p(), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl2 %>% add_global_p(type = 2, keep = TRUE), NA
  )
  expect_snapshot(res %>% as.data.frame())
  expect_warning(
    tbl2 %>% add_global_p(type = "II"), NA
  )
  expect_equal(
    lm(age ~ trt, trial) %>% car::Anova(type = "II") %>% select(last_col()) %>%
      pull() %>% discard(is.na),
    tbl2 %>% add_global_p(type = "II", include = trt) %>% pluck("table_body", "p.value", 1)
  )

  expect_message(tbl2 %>% add_global_p(quiet = TRUE), NA)
})


test_that("no errors/warnings with standard use after tbl_regression with non-standard names", {
  mod1 <- lm(hp ~ factor(`number + cylinders`) * `miles per galon` + factor(`type of transmission`), mtcars %>% rename(`miles per galon` = mpg, `type of transmission` = am, `number + cylinders` = cyl))
  mod2 <- lm(hp ~ factor(cyl) * mpg + factor(am), mtcars)
  tbl1 <- tbl_regression(mod1)
  tbl2 <- tbl_regression(mod2)

  expect_equal(
    suppressWarnings(car::Anova(mod1, type = "II")) %>% select(last_col()) %>% pull() %>% discard(is.na),
    tbl1 %>% {suppressWarnings(add_global_p(., include = everything(), type = "II"))} %>% pluck("table_body", "p.value") %>% discard(is.na)
  )
  expect_equal(
    suppressWarnings(car::Anova(mod1, type = "III")) %>% select(last_col()) %>% pull() %>% discard(is.na) %>% .[-1],
    tbl1 %>% {suppressWarnings(add_global_p(., keep = FALSE, type = "III"))} %>% pluck("table_body", "p.value") %>% discard(is.na)
  )

  # testing that p.values are kept with keep = TRUE (only one line without missing p-value)
  expect_equal(
    tbl1 %>% {suppressWarnings(add_global_p(., keep = TRUE, type = "II"))} %>%
      pluck("table_body") %>% filter(variable == "factor(`number + cylinders`)") %>%
      pull("p.value") %>%
      {
        sum(is.na(.))
      },
    1L
  )

  # testing that using non-standard characters don't change the global p-values
  expect_equal(
    tbl1 %>% {suppressWarnings(add_global_p(., include = everything(), type = "III"))} %>% pluck("table_body", "p.value") %>% discard(is.na),
    tbl2 %>% {suppressWarnings(add_global_p(., include = everything(), type = "III"))} %>% pluck("table_body", "p.value") %>% discard(is.na)
  )

  expect_error(
    tbl_bad_names <-
      trial %>%
      select(response, `grade at dx` = grade, `m?ker` = marker, grade1 = grade) %>%
      tbl_uvregression(
        y = response,
        method = lm
      ) %>%
      add_global_p(),
    NA
  )
  # expect_snapshot(tbl_bad_names %>% as_gt() %>% gt::as_raw_html())
  expect_equal(
    dplyr::filter(tbl_bad_names$table_body, variable == "grade at dx") %>%
      pull(p.value),
    dplyr::filter(tbl_bad_names$table_body, variable == "grade1") %>%
      pull(p.value)
  )
})

test_that("`add_global_p()` works with `tbl_uvregression(x=)`", {
  expect_error(
    tbl <-
      trial %>%
      select(`A g e` = age, `m$rkr` = marker, `gr..de` = grade) %>%
      tbl_uvregression(
        method = lm,
        x = `gr..de`
      ) %>%
      add_global_p(keep = TRUE),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())
  expect_equal(
    as_tibble(tbl, col_labels = FALSE)$p.value,
    c("0.7", NA, "0.6", "0.4", "0.025", NA, "0.010", "0.6")
  )
})
