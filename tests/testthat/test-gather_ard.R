skip_on_cran()
skip_if_pkg_not_installed(c("broom.helpers", "car", "parameters"))

test_that("gather_ard(x) works with `tbl_*()` functions", {
  # tbl_summary()
  expect_false(
    tbl_summary(trial, by = trt, include = c(age, grade)) |>
      add_difference() |>
      gather_ard() |>
      is_empty()
  )

  # tbl_continuous()
  expect_false(
    tbl_continuous(
      data = trial,
      variable = age,
      by = trt,
      include = grade
    ) |>
      gather_ard() |>
      is_empty()
  )

  # tbl_regression()
  expect_silent(
    lst_ard <- glm(response ~ age + grade, trial, family = binomial()) |>
      tbl_regression(exponentiate = TRUE) |>
      add_global_p() |>
      gather_ard()
  )
  expect_equal(
    names(lst_ard),
    c("tbl_regression", "add_global_p")
  )
  expect_true(
    map(lst_ard, ~inherits(.x, "data.frame")) |>
      unlist() |>
      all()
  )

  # tbl_uvregression()
  expect_silent(
    lst_ard <-
      tbl_uvregression(
        trial,
        method = glm,
        y = response,
        method.args = list(family = binomial),
        exponentiate = TRUE,
        include = c("age", "grade")
      ) |>
      add_global_p() |>
      gather_ard()
  )
  expect_equal(
    names(lst_ard),
    c("tbl_uvregression", "add_global_p")
  )
  expect_equal(
    names(lst_ard[["tbl_uvregression"]]),
    c("age", "grade")
  )
  expect_equal(
    names(lst_ard[["add_global_p"]]),
    c("age", "grade")
  )
  expect_true(
    map(lst_ard[["tbl_uvregression"]], ~inherits(.x, "data.frame")) |>
      unlist() |>
      all()
  )
  expect_true(
    map(lst_ard[["add_global_p"]], ~inherits(.x, "data.frame")) |>
      unlist() |>
      all()
  )

  # tbl_stack()
  expect_false({
    t1 <-
      glm(response ~ trt, trial, family = binomial) %>%
      tbl_regression(
        exponentiate = TRUE,
        label = list(trt ~ "Treatment (unadjusted)")
      )

    t2 <-
      glm(response ~ trt + grade + stage + marker, trial, family = binomial) %>%
      tbl_regression(
        include = "trt",
        exponentiate = TRUE,
        label = list(trt ~ "Treatment (adjusted)")
      )

    tbl_stack(list(t1, t2)) |>
      gather_ard() |>
      is_empty()
  })

  # tbl_merge()
  expect_false({
    glm(response ~ trt + grade + age, trial, family = binomial) %>%
      tbl_regression(exponentiate = TRUE) %>%
      list(., .) |>
      tbl_merge() |>
      gather_ard() |>
      is_empty()
  })

  # tbl_strata()
  expect_false(
    trial |>
      select(age, grade, stage, trt) |>
      mutate(grade = paste("Grade", grade)) |>
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x |>
          tbl_summary(by = trt, missing = "no") |>
          add_n(),
        .header = "**{strata}**, N = {n}"
      ) |>
      gather_ard() |>
      is_empty()
  )

  expect_false(
    trial |>
      select(age, grade, stage, trt) |>
      mutate(grade = paste("Grade", grade)) |>
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x |>
          tbl_summary(by = trt, missing = "no") |>
          add_n(),
        .combine_with = "tbl_stack"
      ) |>
      gather_ard() |>
      is_empty()
  )
})

test_that("gather_ard(x) messaging", {
  expect_snapshot(
    as_gtsummary(mtcars[1:2, 1:2]) |>
      gather_ard()
  )
})

test_that("gather_ard(x) works with `tbl_ard_*()` functions", {
  # tbl_ard_summary()
  expect_false(
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_tabulate(variables = "AGEGR1"),
      cards::ard_summary(variables = "AGE"),
      .attributes = TRUE,
      .missing = TRUE
    ) |>
      tbl_ard_summary() |>
      gather_ard() |>
      is_empty()
  )

  # tbl_ard_continuous()
  expect_false(
    cards::bind_ard(
      # the primary ARD with the results
      cards::ard_summary(trial, by = grade, variables = age),
      # add missing and attributes ARD
      cards::ard_missing(trial, by = grade, variables = age),
      cards::ard_attributes(trial, variables = c(grade, age))
    ) |>
      tbl_ard_continuous(variable = "age", include = "grade") |>
      gather_ard() |>
      is_empty()
  )

  # tbl_ard_wide_summary()
  expect_false(
    cards::ard_stack(
      trial,
      cards::ard_summary(variables = age),
      .missing = TRUE,
      .attributes = TRUE
    ) |>
      tbl_ard_wide_summary() |>
      gather_ard() |>
      is_empty()
  )
})

test_that("gather_ard(x) works with `tbl_strata_nested_stack()` function", {
  expect_length(
    tbl_strata_nested_stack(
      trial,
      strata = trt,
      .tbl_fun = ~ .x |>
        tbl_summary(include = c(age, grade), missing = "no") |>
        modify_header(all_stat_cols() ~ "**Summary Statistics**")
    ) |>
      gather_ard(),
    2L
  )
})

test_that("gather_ard() has labels for merged/stacked tbls", {
  expect_equal(
    trial |>
      select(grade, response, stage, trt) |>
      tbl_strata(
        strata = grade,
        .tbl_fun = ~ .x |>
          tbl_summary(by = trt) |>
          add_p()
      ) |>
      gather_ard() |>
      names(),
    c("grade=I", "grade=II", "grade=III")
  )

  expect_equal(
    tbl_strata_nested_stack(
      trial,
      strata = trt,
      ~ .x |>
        tbl_summary(include = c(age, grade), missing = "no") |>
        modify_header(all_stat_cols() ~ "**Summary Statistics**")
    ) |>
      gather_ard() |>
      names(),
    c("trt=\"Drug A\"", "trt=\"Drug B\"")
  )
})


