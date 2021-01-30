skip_on_cran()

t1 <- trial %>% tbl_summary()
t2 <- trial %>% tbl_summary(by = trt)
test_that("no errors/warnings with standard use", {
  expect_error(t1 %>% add_n(), NA)
  expect_error(t2 %>% add_n(), NA)
  expect_warning(t1 %>% add_n(), NA)
  expect_warning(t2 %>% add_n(), NA)

  expect_error(t1 %>% add_n(last = TRUE), NA)
  expect_error(t2 %>% add_n(last = TRUE), NA)
  expect_warning(t1 %>% add_n(last = TRUE), NA)
  expect_warning(t2 %>% add_n(last = TRUE), NA)

  expect_error(t1 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_error(t2 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t1 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t2 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
})


t1 <- trial %>% tbl_summary(type = all_continuous() ~ "continuous2")
t2 <- trial %>% tbl_summary(by = trt, type = all_continuous() ~ "continuous2")
test_that("no errors/warnings with standard use wit continuous2", {
  expect_error(t1 %>% add_n(), NA)
  expect_error(t2 %>% add_n(), NA)
  expect_warning(t1 %>% add_n(), NA)
  expect_warning(t2 %>% add_n(), NA)

  expect_error(t1 %>% add_n(last = TRUE), NA)
  expect_error(t2 %>% add_n(last = TRUE), NA)
  expect_warning(t1 %>% add_n(last = TRUE), NA)
  expect_warning(t2 %>% add_n(last = TRUE), NA)

  expect_error(t1 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_error(t2 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t1 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t2 %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
})


test_that("no errors/warnings with standard use for tbl_svysummary", {
  t <- trial %>%
    survey::svydesign(data = ., ids = ~ 1, weights = ~ 1) %>%
    tbl_svysummary(by = trt)

  expect_error(t %>% add_n(), NA)
  expect_warning(t %>% add_n(), NA)

  expect_error(t %>% add_n(last = TRUE), NA)
  expect_warning(t %>% add_n(last = TRUE), NA)

  t <- Titanic %>%
    as.data.frame() %>%
    survey::svydesign(data = ., ids = ~ 1, weights = ~ Freq) %>%
    tbl_svysummary(by = Survived)

  expect_error(t %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
})

test_that("no errors/warnings with standard use for tbl_svysummary with continuous2", {
  t <- trial %>%
    survey::svydesign(data = ., ids = ~ 1, weights = ~ 1) %>%
    tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2")

  expect_error(t %>% add_n(), NA)
  expect_warning(t %>% add_n(), NA)

  expect_error(t %>% add_n(last = TRUE), NA)
  expect_warning(t %>% add_n(last = TRUE), NA)

  t <- Titanic %>%
    as.data.frame() %>%
    survey::svydesign(data = ., ids = ~ 1, weights = ~ Freq) %>%
    tbl_svysummary(by = Survived, type = all_continuous() ~ "continuous2")

  expect_error(t %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
  expect_warning(t %>% add_n(
    statistic = "{N}{n}{n_miss}{p}{p_miss}",
    footnote = TRUE
  ), NA)
})


# add_nevent.tbl_surfit --------------------------------------------------------

test_that("add_n.tbl_surfit", {
  library(survival)

  tbl_survfit <-
    list(
      survfit(Surv(ttdeath, death) ~ 1, trial),
      survfit(Surv(ttdeath, death) ~ trt, trial)
    ) %>%
    tbl_survfit(times = c(12, 24))

  expect_error(
    add_n(tbl_survfit),
    NA
  )
})
