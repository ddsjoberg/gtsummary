context("test-add_p.tbl_summary")

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
    trial %>%
      tbl_summary(by = trt) %>%
      add_p(),
    NA
  )

  expect_warning(
    trial %>%
      tbl_summary(by = trt) %>%
      add_p(),
    NA
  )

  expect_message(
    trial %>%
      tbl_summary(by = trt) %>%
      add_p(),
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

  expect_error(
    tbl_summary(mtcars, by = am) %>%
      add_p(test = list(
        vars(mpg) ~ t.test,
        disp ~ aov
      )),
    NA
  )
})

test_that("add_p with custom p-value function", {
  my_mcnemar <- function(data, variable, by, ...) {
    result <- list()
    result$p <- stats::mcnemar.test(data[[variable]], data[[by]])$p.value
    result$test <- "McNemar's test"
    result
  }

  expect_error(
    trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ "my_mcnemar"),
    NA
  )

  expect_error(
    trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ my_mcnemar),
    NA
  )
})


# test-add_p.tbl_cross----------------------------------------------------------
context("test-add_p.tbl_cross")

test_that("add_p.tbl_cross", {
  expect_error(
    trial %>% tbl_cross(response, death) %>% add_p(),
    NA
  )
  expect_error(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(),
    NA
  )
  expect_error(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(source_note = TRUE),
    NA
  )
  expect_error(
    mtcars %>%
      tbl_cross(gear, carb) %>%
      add_p(test = "fisher.test"),
    NA
  )
})








