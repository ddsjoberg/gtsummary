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








