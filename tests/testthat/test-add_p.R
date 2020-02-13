context("test-add_p")

test_that("add_p creates output without error/warning", {
  expect_error(
    tbl_summary(mtcars, by = am) %>% add_p(),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = am) %>% add_p(),
    NA
  )

  expect_error(
    tbl_summary(trial, by = trt) %>%
      add_p(test = everything() ~ "lme4", group = response),
    NA
  )
})

test_that("add_p creates errors when non-function in input", {
  expect_error(
    tbl_summary(mtcars, by = am) %>%
      add_p(pvalue_fun = mtcars),
    "*"
  )
})


test_that("add_p works well", {
  expect_error(
    tbl_summary(mtcars, by = am) %>%
      add_p(test = list(
        vars(mpg) ~ "t.test",
        disp ~ "aov",
        cyl ~ "chisq.test.no.correct"
      )),
    NA
  )
})

test_that("add_p defaults to clustered data with `group=` arg", {
  expect_error(
    add_p_lme4 <-
      tbl_summary(trial[c("trt","death","age", "stage")], by = death) %>%
      add_p(group = trt),
    NA
  )
  expect_equal(
    add_p_lme4$meta_data$stat_test,
    c("lme4", "lme4")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define function for unit test involving the method footnote when using
# custom p-value in add_p
my_mcnemar <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::mcnemar.test(data[[variable]], data[[by]])$p.value
  result$test <- paste0("McNemar's test",' "this is my method"')
  result
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("add_p custom p-value method footnote does not require double escape chars for quotes" {
  expect_error(
    trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ "my_mcnemar"),
    NA
  )
})









