test_that("fmt_fun works", {
  expect_error(
    tbl1 <-
      lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      modify_fmt_fun(
        columns = p.value,
        rows = variable == 'grade',
        fmt_fun = function(x) style_pvalue(x, digits = 3)
      ),
    NA
  )
})
