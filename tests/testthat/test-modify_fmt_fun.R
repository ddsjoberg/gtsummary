skip_on_cran()

test_that("fmt_fun works", {
  expect_error(
    tbl1 <-
      lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      modify_fmt_fun(
        update = p.value ~ function(x) style_pvalue(x, digits = 3),
        rows = variable == "grade"
      ),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())
})
