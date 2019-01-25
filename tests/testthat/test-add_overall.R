context("test-add_overall")
library(survival)

test_that("no errors/warnings with standard use", {
  expect_error(mtcars %>% fmt_table1(by = "am") %>% add_overall(), NA)
  expect_warning(mtcars %>% fmt_table1(by = "am") %>% add_overall(), NA)

  expect_error(iris %>% fmt_table1(by = "Species") %>% add_overall(), NA)
  expect_warning(iris %>% fmt_table1(by = "Species") %>% add_overall(), NA)
})

test_that("no errors/warnings with missing data", {
  expect_error(lung %>% fmt_table1(by = "sex") %>% add_overall(), NA)
  expect_warning(lung %>% fmt_table1(by = "sex") %>% add_overall(), NA)
})
