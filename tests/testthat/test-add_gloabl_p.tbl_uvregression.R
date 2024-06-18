skip_on_cran()
test_that("no errors/warnings with standard use after tbl_uvregression", {
  tbl <- trial %>% tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  expect_silent(
    res <- tbl %>% add_global_p()
  )

  expect_snapshot(res %>% as.data.frame())

  # two model terms, two p values
  expect_equal(
    sum(!is.na(res$table_body$p.value)),
    2
  )
})

test_that("testing include argument", {
  tbl <- trial %>% tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  res1 <- tbl %>% add_global_p(include = age)

  # one expected p-value for age
  expect_equal(
    sum(!is.na(res1$table_body$p.value)),
    1
  )
})

test_that("testing type and keep argument", {
  tbl <- trial %>% tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  res2 <- tbl %>% add_global_p(type = "II", keep = TRUE)

  # 4 expected p-values, 2 for each variable, 2 for each level of grade
  expect_equal(
    sum(!is.na(res2$table_body$p.value)),
    4
  )
})

test_that("testing anova_fun argument", {
  tbl <- trial %>% tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  res3 <- tbl %>% add_global_p(anova_fun = cardx::ard_aod_wald_test)

  # two model terms, two p values
  expect_equal(
    sum(!is.na(res3$table_body$p.value)),
    2
  )
})
