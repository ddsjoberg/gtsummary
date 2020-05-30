context("test-add_stat")

my_ttest <- function(data, variable, by, ...) {
  t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
}

my_ttest2 <- function(data, variable, by, ...) {
  tt <- t.test(data[[variable]] ~ as.factor(data[[by]]))

  # returning test statistic and pvalue
  stringr::str_glue(
    "t={style_sigfig(tt$statistic)}, {style_pvalue(tt$p.value, prepend_p = TRUE)}"
  )
}

tbl <- trial %>%
  select(trt, age, marker) %>%
  tbl_summary(by = trt, missing = "no")

test_that("no errors/warnings with standard use", {


  expect_error(
    test1 <- tbl %>%
      add_p(test = everything() ~ t.test) %>%
      # replicating result of `add_p()` with `add_stat()`
      add_stat(
        fns = everything() ~ my_ttest, # all variables compared with with t-test
        fmt_fun = style_pvalue,        # format result with style_pvalue()
        header = "**My p-value**"      # new column header
      ),
    NA
  )

  # checking the pvalues match
  expect_equal(
    test1$table_body$p.value %>% rlang::set_names(c("age", "marker")),
    test1$table_body$add_stat_1
  )



  expect_error(
    tbl %>%
      add_stat(
        fns = everything() ~ my_ttest2,    # all variables will be compared by t-test
        fmt_fun = NULL, # fn returns and chr, so no formatting function needed
        header = "**Treatment Comparison**",       # column header
        footnote = "T-test statistic and p-value"  # footnote
      ),
    NA
  )
})

test_that("expect errors", {
  expect_message(
    tbl %>%
      add_stat(
        fns = everything() ~ mean,    # all variables will be compared by t-test
        fmt_fun = NULL, # fn returns and chr, so no formatting function needed
        header = "**Treatment Comparison**",       # column header
        footnote = "T-test statistic and p-value"  # footnote
      ),
    "*"
  )

  expect_error(
    tbl %>%
      add_stat(
        fns = everything() ~ my_ttest2,    # all variables will be compared by t-test
        fmt_fun = "string", # fn returns and chr, so no formatting function needed
        header = "**Treatment Comparison**",       # column header
        footnote = "T-test statistic and p-value"  # footnote
      ),
    "*"
  )

  expect_error(
    tbl %>%
      add_stat(
        fns = everything() ~ my_ttest2,    # all variables will be compared by t-test
        fmt_fun = NULL, # fn returns and chr, so no formatting function needed
        header = c("**Treatment Comparison**", "**Treatment Comparison**"),       # column header
        footnote = "T-test statistic and p-value"  # footnote
      ),
    "*"
  )

  expect_error(
    tbl %>%
      add_stat(
        fns = everything() ~ my_ttest2,    # all variables will be compared by t-test
        fmt_fun = NULL, # fn returns and chr, so no formatting function needed
        header = "**Treatment Comparison**",       # column header
        footnote = c("T-test statistic and p-value", "T-test statistic and p-value")  # footnote
      ),
    "*"
  )

  expect_error(
    tbl %>%
      add_stat(
        fns = everything() ~ my_ttest2,    # all variables will be compared by t-test
        fmt_fun = NULL, # fn returns and chr, so no formatting function needed
        header = "**Treatment Comparison**",       # column header
        new_col_name = mean,
        footnote = "T-test statistic and p-value"  # footnote
      ),
    "*"
  )

  expect_error(
    mtcars %>%
      add_stat(
        fns = everything() ~ my_ttest2,    # all variables will be compared by t-test
        fmt_fun = NULL, # fn returns and chr, so no formatting function needed
        header = "**Treatment Comparison**",       # column header
        footnote = "T-test statistic and p-value"  # footnote
      ),
    "*"
  )
})
