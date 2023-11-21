skip_on_cran()

table1 <- trial %>%
  tbl_summary(by = trt) %>%
  add_p()

test_that("no errors/warnings with standard use after tbl_summary() and add_p()", {
  expect_snapshot(add_q(table1) %>% as.data.frame())
  expect_warning(add_q(table1), NA)
})


test_that("expect error if no p value in table 1", {
  table1 <- trial %>% tbl_summary(by = trt)

  expect_error(
    add_q(table1),
    NULL
  )
})


test_that("no errors/warnings with standard use after tbl_uvregression() and add_global_p()", {
  skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))
  uni_reg <- trial %>%
    tbl_uvregression(
      method = lm,
      y = age
    ) %>%
    add_global_p()

  expect_snapshot(add_q(uni_reg) %>% as.data.frame())
  expect_warning(add_q(uni_reg), NA)
})

test_that("add_q creates errors when non-function in input", {
  expect_error(
    add_q(uni_reg, pvalue_fun = mtcars),
    NULL
  )
})



test_that("Checking q-values against p.adjust", {
  q_check1 <-
    lm(marker ~ stage + age + grade + trt, trial) %>%
    tbl_regression() %>%
    add_q()
  q_check2 <-
    lm(marker ~ stage + age + grade + trt, trial) %>%
    tbl_regression() %>%
    add_q(method = "bonferroni")
  expect_equal(
    q_check1$table_body$q.value %>% purrr::discard(is.na),
    q_check1$table_body$p.value %>% purrr::discard(is.na) %>% stats::p.adjust(method = "fdr")
  )
  expect_equal(
    q_check2$table_body$q.value %>% purrr::discard(is.na),
    q_check2$table_body$p.value %>% purrr::discard(is.na) %>% stats::p.adjust(method = "bonferroni")
  )
})


test_that("add_q messaging checks", {
  expect_message(
    add_q(table1),
    NULL
  )

  expect_message(
    add_q(table1, quiet = TRUE),
    NA
  )
})
