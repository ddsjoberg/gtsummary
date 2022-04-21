skip_on_cran()

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
        fns = everything() ~ my_ttest # all variables compared with with t-test
      ),
    NA
  )

  # checking the pvalues match
  expect_equal(
    test1$table_body$p.value,
    test1$table_body$add_stat_1
  )


  expect_error(
    tbl %>%
      add_stat(
        fns = everything() ~ my_ttest2, # all variables will be compared by t-test
        fmt_fun = NULL # fn returns and chr, so no formatting function needed
      ),
    NA
  )
})

test_that("expect errors", {
  expect_message(
    tbl %>%
      add_stat(
        fns = everything() ~ mean # all variables will be compared by t-test
      ),
    NULL
  )

  expect_error(
    mtcars %>%
      add_stat(
        fns = everything() ~ my_ttest2 # all variables will be compared by t-test
      ),
    NULL
  )

  return_three_10s <- function(...) rep_len(10, 3)
  expect_error(
    trial %>%
      select(grade) %>%
      tbl_summary() %>%
      add_stat(
        fns = everything() ~ return_three_10s,
        location = all_categorical() ~ "level"
      ),
    NA
  )

  expect_error(
    trial %>%
      select(age) %>%
      tbl_summary(
        type = age ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{min}", "{max}")
      ) %>%
      add_stat(
        fns = everything() ~ return_three_10s,
        location = everything() ~ "level"
      ),
    NA
  )

  skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))
  return_two_10s <- function(...) rep_len(10, 2)
  expect_error(
    survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) %>%
      tbl_svysummary(include = Sex) %>%
      add_stat(
        fns = everything() ~ return_two_10s,
        location = all_categorical() ~ "level"
      ),
    NA
  )

  return_two_by_two_10s <- function(...) tibble::tibble(one = rep_len(10, 2), two = rep_len(10, 2))
  expect_error(
    trial %>%
      select(age) %>%
      tbl_summary(
        type = age ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{sd}")
      ) %>%
      add_stat(
        fns = everything() ~ return_two_by_two_10s,
        location = everything() ~ "level"
      ),
    NA
  )

  return_one_by_two_10s <- function(...) tibble::tibble(one = rep_len(10, 1), two = rep_len(10, 1))
  expect_error(
    trial %>%
      select(age) %>%
      tbl_summary(
        type = age ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{sd}"),
        missing = "always"
      ) %>%
      add_stat(
        fns = everything() ~ return_one_by_two_10s,
        location = everything() ~ "missing"
      ),
    NA
  )
})


test_that("tbl_continuous()", {
  tt <-
    trial %>%
    tbl_continuous(
      age,
      include = grade,
      by = trt
    )

  add_stat_test1 <- function(data, variable, by, ...) {
    tibble::tibble(test_col = "Ugh")
  }

  add_stat_test2 <- function(data, variable, by, ...) {
    tibble::tibble(test_col = rep_len("Ugh", 3))
  }

  expect_error(
    tt %>%
      add_stat(fns = everything() ~ add_stat_test1),
    NA
  )
  expect_error(
    tt %>%
      add_stat(fns = everything() ~ add_stat_test2, location = everything() ~ "level"),
    NA
  )
})
