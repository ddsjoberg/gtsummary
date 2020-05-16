context("test-add_overall")
library(survival)

test_that("no errors/warnings with standard use", {
  expect_error(mtcars %>% tbl_summary(by = am) %>% add_overall(), NA)
  expect_warning(mtcars %>% tbl_summary(by = am) %>% add_overall(), NA)

  expect_error(mtcars %>% tbl_summary(by = am) %>% add_overall(last = TRUE), NA)
  expect_warning(mtcars %>% tbl_summary(by = am) %>% add_overall(last = TRUE), NA)

  expect_error(iris %>% tbl_summary(by = Species) %>% add_overall(), NA)
  expect_warning(iris %>% tbl_summary(by = Species) %>% add_overall(), NA)
})

test_that("no errors/warnings with missing data", {
  expect_error(lung %>% tbl_summary(by = sex) %>% add_overall(), NA)
  expect_warning(lung %>% tbl_summary(by = sex) %>% add_overall(), NA)

  expect_error(lung %>% tbl_summary(by = sex) %>% add_overall(last = TRUE), NA)
  expect_warning(lung %>% tbl_summary(by = sex) %>% add_overall(last = TRUE), NA)
})

test_that("no errors/warnings with missing data in by variable", {
  expect_error(trial %>% tbl_summary(by = response) %>% add_overall(), NA)

})
