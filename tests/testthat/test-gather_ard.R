skip_on_cran()
skip_if_not(is_pkg_installed("cardx"))

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
  expect_false(
    glm(response ~ age + grade, trial, family = binomial()) |>
      tbl_regression(exponentiate = TRUE) |>
      gather_ard() |>
      is_empty()
  )

  # tbl_uvregression()
  expect_false(
    tbl_uvregression(
      trial,
      method = glm,
      y = response,
      method.args = list(family = binomial),
      exponentiate = TRUE,
      include = c("age", "grade")
    ) |>
      gather_ard() |>
      is_empty()
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

test_that("gather_ard(x) works with `tbl_ard_*()` functions", {
  # tbl_ard_summary()
  expect_false(
    cards::ard_stack(
      data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"),
      cards::ard_continuous(variables = "AGE"),
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
      cards::ard_continuous(trial, by = grade, variables = age),
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
      cards::ard_continuous(variables = age),
      .missing = TRUE,
      .attributes = TRUE
    ) |>
      tbl_ard_wide_summary() |>
      gather_ard() |>
      is_empty()
  )
})
