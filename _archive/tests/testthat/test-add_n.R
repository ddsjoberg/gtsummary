skip_on_cran()

t1 <- trial %>% tbl_summary()
t2 <- trial %>% tbl_summary(by = trt)
test_that("no errors/warnings with standard use", {
  expect_error(res <- t1 %>% add_n(), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_error(res <- t2 %>% add_n(), NA)
  expect_snapshot(res %>% as.data.frame())

  expect_warning(t1 %>% add_n(), NA)
  expect_warning(t2 %>% add_n(), NA)

  expect_error(res <- t1 %>% add_n(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_error(res <- t2 %>% add_n(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t1 %>% add_n(last = TRUE), NA)
  expect_warning(t2 %>% add_n(last = TRUE), NA)

  expect_error(
    res <-
      t1 %>%
      add_n(
        statistic = "{N}{n}{n_miss}{p}{p_miss}",
        footnote = TRUE
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
  expect_error(
    t2 %>%
      add_n(
        statistic = "{N}{n}{n_miss}{p}{p_miss}",
        footnote = TRUE
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t1 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t2 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)

  # check that the Ns are the same when add overall is run before/after add_n()
  tbl1 <- tbl_summary(trial, by = trt, include = c(age, stage))
  expect_equal(
    tbl1 %>% add_n() %>% add_overall() %>% purrr::pluck("table_body", "n"),
    tbl1 %>% add_overall() %>% add_n() %>% purrr::pluck("table_body", "n")
  )
})


t1 <- trial %>% tbl_summary(type = all_continuous() ~ "continuous2")
t2 <- trial %>% tbl_summary(by = trt, type = all_continuous() ~ "continuous2")
test_that("no errors/warnings with standard use with continuous2", {
  expect_error(res <- t1 %>% add_n(), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_error(res <- t2 %>% add_n(), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t1 %>% add_n(), NA)
  expect_warning(t2 %>% add_n(), NA)

  expect_error(res <- t1 %>% add_n(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_error(res <- t2 %>% add_n(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t1 %>% add_n(last = TRUE), NA)
  expect_warning(t2 %>% add_n(last = TRUE), NA)

  expect_error(
    res <- t1 %>%
      add_n(
        statistic = "{N}{n}{n_miss}{p}{p_miss}",
        footnote = TRUE
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- t2 %>%
      add_n(
        statistic = "{N}{n}{n_miss}{p}{p_miss}",
        footnote = TRUE
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_warning(t1 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t2 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)

  expect_equal(
    data.frame(
      x = c(1, 2, NA, NA, NA, 1, 2, 1),
      y = c(rep(0, 4), rep(1, 4))
    ) %>%
      tbl_summary(by = y) %>%
      add_n(statistic = "{n_miss}/{N} = {p_miss}%") %>%
      as_tibble(col_labels = FALSE) %>%
      purrr::pluck("n", 1),
    "3/8 = 38%"
  )

  expect_equal(
    data.frame(
      x = c(1, 2, NA, NA, NA, 1, 2, 1),
      y = c(rep(0, 4), rep(1, 4))
    ) %>%
      select(x) %>%
      tbl_summary() %>%
      add_n(statistic = "{n_miss}/{N} = {p_miss}%") %>%
      as_tibble(col_labels = FALSE) %>%
      purrr::pluck("n", 1),
    "3/8 = 38%"
  )
})


test_that("no errors/warnings with standard use for tbl_svysummary", {
  skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))

  t <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt)

  expect_error(res <- t %>% add_n(), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t %>% add_n(), NA)

  expect_error(res <- t %>% add_n(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t %>% add_n(last = TRUE), NA)

  t <- Titanic %>%
    as.data.frame() %>%
    survey::svydesign(data = ., ids = ~1, weights = ~Freq) %>%
    tbl_svysummary(by = Survived)

  expect_error(
    res <-
      t %>%
      add_n(
        statistic = "{N}{n}{n_miss}{p}{p_miss}",
        footnote = TRUE
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
})

test_that("no errors/warnings with standard use for tbl_svysummary with continuous2", {
  skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))

  t <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2")

  expect_error(res <- t %>% add_n(), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t %>% add_n(), NA)

  expect_error(res <- t %>% add_n(last = TRUE), NA)
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t %>% add_n(last = TRUE), NA)

  t <- Titanic %>%
    as.data.frame() %>%
    survey::svydesign(data = ., ids = ~1, weights = ~Freq) %>%
    tbl_svysummary(by = Survived, type = all_continuous() ~ "continuous2")

  expect_error(
    res <- t %>%
      add_n(
        statistic = "{N}{n}{n_miss}{p}{p_miss}",
        footnote = TRUE
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
  expect_warning(t %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
})


# add_n.tbl_survfit --------------------------------------------------------

test_that("add_n.tbl_survfit", {
  skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

  tbl_survfit <-
    list(
      survival::survfit(survival::Surv(ttdeath, death) ~ 1, trial),
      survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)
    ) %>%
    tbl_survfit(times = c(12, 24))

  expect_error(
    res <- add_n(tbl_survfit),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
})


# add_n.tbl_regression ---------------------------------------------------------
test_that("add_n.tbl_regression", {
  tbl <-
    glm(response ~ grade + age, trial, family = binomial) %>%
    tbl_regression()

  expect_error(
    res <- tbl %>% add_n(), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl %>% add_n(location = "level"), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl %>% add_n(location = c("label", "level")), NA
  )
  expect_snapshot(res %>% as.data.frame())
})

# add_n.tbl_uvregression ---------------------------------------------------------
test_that("add_n.tbl_regression", {
  tbl <-
    trial %>%
    select(response, age, grade) %>%
    tbl_uvregression(
      y = response,
      method = glm,
      method.args = list(family = binomial)
    )

  expect_error(
    res <- tbl %>% add_n(), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl %>% add_n(location = "level"), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl %>% add_n(location = c("label", "level")), NA
  )
  expect_snapshot(res %>% as.data.frame())
})
