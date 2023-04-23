skip_on_cran()

test_that("no errors/warnings with standard use", {
  tbl <- mtcars %>% tbl_summary(by = am)
  expect_snapshot(tbl %>% add_stat_label() %>% as.data.frame())
  expect_warning(tbl %>% add_stat_label(), NA)

  expect_error(tbl00 <- tbl %>% add_stat_label() %>% add_p(), NA)
  expect_snapshot(tbl00 %>% as.data.frame())
  expect_warning(tbl %>% add_stat_label() %>% add_p(), NA)

  expect_snapshot(tbl %>% add_overall() %>% add_stat_label() %>% as.data.frame())
  expect_warning(tbl %>% add_overall() %>% add_stat_label(), NA)

  expect_snapshot(
    tbl %>%
      add_stat_label(location = "column", label = all_categorical() ~ "no. (%)") %>%
      as.data.frame()
  )
  expect_warning(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)

  expect_error(
    tbl <-
      trial %>%
      select(age, grade, trt) %>%
      tbl_summary(
        by = trt,
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c("{mean} ({sd})", "{min} - {max}"),
      ) %>%
      add_stat_label(label = age ~ c("Mean (SD)", "Min - Max")),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())
  expect_equal(
    tbl %>%
      modify_column_unhide(everything()) %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::filter(variable == "age", row_type == "level") %>%
      dplyr::pull(label),
    c("Mean (SD)", "Min - Max")
  )
})

test_that("no errors/warnings with standard use for continuous2", {
  tbl <- mtcars %>% tbl_summary(by = am, type = all_continuous() ~ "continuous2")
  expect_snapshot(tbl %>% add_stat_label() %>% as.data.frame())
  expect_warning(tbl %>% add_stat_label(), NA)

  expect_error(tbl00 <- tbl %>% add_stat_label() %>% add_p(), NA)
  expect_snapshot(tbl00 %>% as.data.frame())
  expect_warning(tbl %>% add_stat_label() %>% add_p(), NA)

  expect_snapshot(tbl %>% add_overall() %>% add_stat_label() %>% as.data.frame())
  expect_warning(tbl %>% add_overall() %>% add_stat_label(), NA)

  expect_snapshot(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)") %>% as.data.frame())
  expect_warning(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
})


test_that("no errors/warnings with standard use for tbl_svysummary", {
  skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))
  tbl <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt)

  expect_snapshot(tbl %>% add_stat_label() %>% as.data.frame())
  expect_warning(tbl %>% add_stat_label(), NA)

  expect_snapshot(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)") %>% as.data.frame())
  expect_warning(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
})

test_that("no errors/warnings with standard use for tbl_svysummary with continuous2", {
  skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))
  tbl <- trial %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
    tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2")

  expect_snapshot(tbl %>% add_stat_label() %>% as.data.frame())
  expect_warning(tbl %>% add_stat_label(), NA)

  expect_snapshot(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)") %>% as.data.frame())
  expect_warning(tbl %>% add_stat_label(location = "column", label = all_categorical() ~ "no. (%)"), NA)
})

test_that("add_stat_label() with tbl_merge()", {
  tbl0 <-
    trial %>%
    select(age, response, trt) %>%
    tbl_summary(by = trt, missing = "no") %>%
    add_stat_label()

  expect_error(
    tbl1 <- tbl_merge(list(tbl0, tbl0)),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_equal(
    as_tibble(tbl1, col_labels = FALSE) %>% dplyr::pull(label),
    c("Age, Median (IQR)", "Tumor Response, n (%)")
  )
})
