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

test_that("add_stat() works with fns that return a scalar", {
  expect_error(
    test1 <- tbl |>
      add_p(test = everything() ~ t.test) |>
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
    tbl |>
      add_stat(
        fns = everything() ~ my_ttest2, # all variables will be compared by t-test
      ),
    NA
  )
})

test_that("add_stat() works with fns that returns a vector", {
  return_three_10s <- function(...) rep_len(10, 3)
  expect_equal(
    trial |>
      tbl_summary(include = grade) |>
      add_stat(
        fns = everything() ~ return_three_10s,
        location = all_categorical() ~ "level"
      ) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull(add_stat_1),
    c(NA, "10.0", "10.0", "10.0")
  )

  expect_equal(
    trial |>
      tbl_summary(
        include = age,
        type = age ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{min}", "{max}")
      ) %>%
      add_stat(
        fns = everything() ~ return_three_10s,
        location = everything() ~ "level"
      ) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull(add_stat_1),
    c(NA, "10.0", "10.0", "10.0", NA)
  )
})

test_that("add_stat() works with fns that returns a tibble", {
  return_two_by_two_10s <- function(...) dplyr::tibble(one = rep_len(10, 2), two = rep_len(10, 2))
  expect_equal(
    trial |>
      tbl_summary(
        include = age,
        type = age ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{sd}")
      ) |>
      add_stat(
        fns = everything() ~ return_two_by_two_10s,
        location = everything() ~ "level"
      ) |>
      as_tibble(col_label = FALSE) |>
      dplyr::select(one, two),
    dplyr::bind_rows(
      dplyr::tibble(one = NA, two = NA),
      return_two_by_two_10s(),
      dplyr::tibble(one = NA, two = NA)
    ) |>
      dplyr::mutate(across(everything(), styfn_sigfig(digits = 3)))
  )

  return_one_by_two_10s <- function(...) dplyr::tibble(one = rep_len(10, 1), two = rep_len(10, 1))
  expect_equal(
    trial |>
      tbl_summary(
        include = age,
        type = age ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{sd}"),
        missing = "always"
      ) %>%
      add_stat(
        fns = everything() ~ return_one_by_two_10s,
        location = everything() ~ "missing"
      ) |>
      as_tibble(col_label = FALSE) |>
      dplyr::select(one, two),
    dplyr::bind_rows(
      dplyr::tibble(one = c(NA, NA, NA), two = c(NA, NA, NA)),
      return_one_by_two_10s(),
    ) |>
      dplyr::mutate(across(everything(), styfn_sigfig(digits = 3)))
  )
})

test_that("add_stat(fns) messaging", {
  # pass a function, but it does not accept the correct arguments
  expect_snapshot(
    result <- tbl |> add_stat(fns = everything() ~ mean)
  )

  return_two_by_two_10s <- function(...) dplyr::tibble(one = rep_len(10, 2), two = rep_len(10, 2))
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(
        include = age,
        type = age ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{sd}")
      ) |>
      add_stat(
        fns = everything() ~ return_two_by_two_10s,
        location = everything() ~ "label"
      )
  )

  return_two_by_two_10s <- function(...) dplyr::tibble(one = rep_len(10, 2), two = rep_len(10, 2))
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(
        include = age,
        type = age ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{sd}")
      ) |>
      add_stat(
        fns = everything() ~ return_two_by_two_10s,
        location = everything() ~ "level"
      ) |>
      add_stat(
        fns = everything() ~ return_two_by_two_10s,
        location = everything() ~ "level"
      )
  )
})

test_that("add_stat(x) messaging", {
  # function only accepts certain gtsummary tables
  expect_snapshot(
    error = TRUE,
    mtcars |>
      add_stat(fns = everything() ~ my_ttest2)
  )
})

# TODO: Add `tbl_svysummary()` tests

# TODO: Add `tbl_continuous()` tests
