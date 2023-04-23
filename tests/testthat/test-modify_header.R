skip_on_cran()
skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))

tbl_summary_noby <- trial %>% tbl_summary()
tbl_summary_by <- trial %>% tbl_summary(by = trt)
tbl_svysummary_by <-
  survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) %>%
  tbl_svysummary(by = Survived)

test_that("input checks", {
  expect_snapshot(
    tbl_summary_noby %>% modify_header(stat_0 = "test") %>% as.data.frame()
  )

  expect_snapshot(
    tbl_summary_noby %>% modify_header(stat_0 ~ "test") %>% as.data.frame()
  )

  expect_error(
    tbl_summary_noby %>% modify_header(),
    NA
  )

  expect_snapshot(tbl_summary_noby %>% modify_header(stat_0 = "N = {n}") %>% as.data.frame())
  expect_message(tbl_summary_noby %>% modify_header(stat_1 = "N = {n}"))
  expect_equal(
    tbl_summary_by %>%
      add_overall() %>%
      modify_header(all_stat_cols() ~ "{level}, N = {n}/{N} ({style_percent(p)}%)") %>%
      purrr::pluck("table_styling", "header") %>%
      dplyr::filter(startsWith(column, "stat_")) %>%
      dplyr::pull("label"),
    c("Overall, N = 200/200 (100%)", "Drug A, N = 98/200 (49%)", "Drug B, N = 102/200 (51%)")
  )

  expect_equal(
    tbl_svysummary_by %>%
      add_overall() %>%
      modify_header(all_stat_cols() ~ "{level}, N = {n}/{N} ({style_percent(p)}%)") %>%
      purrr::pluck("table_styling", "header") %>%
      dplyr::filter(startsWith(column, "stat_")) %>%
      dplyr::pull("label"),
    c("Overall, N = 2201/2201 (100%)", "No, N = 1490/2201 (68%)", "Yes, N = 711/2201 (32%)")
  )

  # this is erring on R 3.6 only WTF??!
  # expect_error(
  #   tbl_summary_noby %>% modify_header(label = c("test", "test2"))
  # )
})

test_that("checking glue inserts to headers", {
  expect_error(
    tbl1 <-
      tbl_summary_by %>%
      modify_header(
        list(
          all_stat_cols() ~ "{level} ({n}/{N}; {style_percent(p)}%)",
          label ~ "Variable (N = {N})"
        )
      ),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_equal(
    tbl1$table_styling$header %>% dplyr::filter(hide == FALSE) %>% dplyr::pull(label),
    c("Variable (N = 200)", "Drug A (98/200; 49%)", "Drug B (102/200; 51%)")
  )

  expect_error(
    tbl2 <-
      tbl_svysummary_by %>%
      modify_header(
        list(
          all_stat_cols() ~ "{level} ({n}/{N}; {style_percent(p)}%): Unweighted {n_unweighted}/{N_unweighted}; {style_percent(p_unweighted)}%",
          label ~ "Variable (N = {N}: Unweighted {N_unweighted})"
        )
      ),
    NA
  )
  expect_snapshot(tbl2 %>% as.data.frame())

  expect_equal(
    tbl2$table_styling$header %>% dplyr::filter(hide == FALSE) %>% dplyr::pull(label),
    c(
      "Variable (N = 2201: Unweighted 32)",
      "No (1490/2201; 68%): Unweighted 16/32; 50%",
      "Yes (711/2201; 32%): Unweighted 16/32; 50%"
    )
  )

  expect_error(
    tbl3 <-
      lm(mpg ~ hp, mtcars) %>%
      tbl_regression() %>%
      modify_header(label ~ "Variable (N = {N})"),
    NA
  )
  expect_snapshot(tbl3 %>% as.data.frame())
  expect_equal(
    tbl3$table_styling$header %>% dplyr::filter(column == "label") %>% dplyr::pull(label),
    c("Variable (N = 32)")
  )
})

test_that("deprecated argument still works", {
  lifecycle::expect_defunct(
    trial %>%
      tbl_summary(by = trt, include = age) %>%
      modify_header(stat_by = "{level}")
  )
})
