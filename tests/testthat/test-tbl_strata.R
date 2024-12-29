skip_on_cran()

test_that("tbl_strata works with standard use", {
  # one stratifier ----
  expect_silent(
    tbl <- trial |>
      select(grade, response, stage, trt) |>
      mutate(grade = paste("Grade", grade)) |>
      tbl_strata(
        strata = grade,
        .tbl_fun = ~ .x |>
          tbl_summary(by = trt) |>
          add_p()
      )
  )

  # df_strata correct
  expect_equal(nrow(tbl$df_strata), 3)
  expect_equal(names(tbl$df_strata), c("strata_1", "header"))

  # correct subtables
  expect_equal(length(tbl$tbls), 3)

  # two stratifiers ----
  expect_silent(
    tbl <- trial |>
      select(age, grade, stage, trt) |>
      mutate(
        grade = paste("Grade", grade),
        stage = paste("Stage", stage)
      ) |>
      tbl_strata(
        strata = c(grade, stage),
        .tbl_fun = ~ .x |>
          tbl_summary(by = trt) |>
          add_p(test = all_continuous() ~ "t.test")
      )
  )

  # df_strata correct
  expect_equal(nrow(tbl$df_strata), 12)
  expect_equal(names(tbl$df_strata), c("strata_1", "strata_2", "header"))

  # correct subtables
  expect_equal(length(tbl$tbls), 12)
})

test_that("tbl_strata(.sep) works as expected", {
  # default .sep
  tbl <- trial |>
    select(age, grade, stage, trt) |>
    mutate(
      grade = paste("Grade", grade),
      stage = paste("Stage", stage)
    ) |>
    tbl_strata(
      strata = c(grade, stage),
      .tbl_fun = ~ .x |>
        tbl_summary(by = trt) |>
        add_p(test = all_continuous() ~ "t.test")
    )

  expect_true(all(grepl(", ", tbl$df_strata$header)))

  # custom .sep
  tbl <- trial |>
    select(age, grade, stage, trt) |>
    mutate(
      grade = paste("Grade", grade),
      stage = paste("Stage", stage)
    ) |>
    tbl_strata(
      strata = c(grade, stage),
      .tbl_fun = ~ .x |>
        tbl_summary(by = trt) |>
        add_p(test = all_continuous() ~ "t.test"),
      .sep = "--"
    )

  expect_true(all(grepl("--", tbl$df_strata$header)))
})

test_that("tbl_strata(.combine_with) works as expected", {
  # non-default .combine_with (tbl_stack)
  expect_message(
    tbl <- trial |>
      select(grade, stage, trt) |>
      mutate(grade = paste("Grade", grade)) |>
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x |>
          tbl_summary(by = trt) |>
          add_p(),
        .combine_with = "tbl_stack"
      )
  )

  # df_strata correct
  expect_equal(nrow(tbl$df_strata), 3)
  expect_equal(names(tbl$df_strata), c("strata_1", "header"))

  # correct subtables
  expect_equal(length(tbl$tbls), 3)
  expect_true("groupname_col" %in% names(tbl$table_body))
  expect_true(all(c("Grade I", "Grade II", "Grade III") %in% tbl$table_body$groupname_col))
})

test_that("tbl_strata(.header) works as expected", {
  # default .header
  tbl <- trial |>
    select(age, grade, stage, trt) |>
    mutate(
      grade = paste("Grade", grade),
      stage = paste("Stage", stage)
    ) |>
    tbl_strata(
      strata = c(grade, stage),
      .tbl_fun = ~ .x |>
        tbl_summary(by = trt) |>
        add_p(test = all_continuous() ~ "t.test")
    )

  expect_true(all(grepl("\\*\\*Grade .* Stage .*\\*\\*", tbl$df_strata$header)))

  # custom .header
  tbl <- trial |>
    select(age, grade, stage, trt) |>
    mutate(
      grade = paste("Grade", grade),
      stage = paste("Stage", stage)
    ) |>
    tbl_strata(
      strata = c(grade, stage),
      .tbl_fun = ~ .x |>
        tbl_summary(by = trt) |>
        add_p(test = all_continuous() ~ "t.test"),
      .header = "_{strata}_"
    )

  expect_true(all(grepl("_Grade .* Stage .*_", tbl$df_strata$header)))

  # custom .header with .combine_with = "tbl_stack"
  expect_message(
    tbl <- trial |>
      select(grade, stage, trt) |>
      mutate(grade = paste("Grade", grade)) |>
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x |>
          tbl_summary(by = trt) |>
          add_p(),
        .combine_with = "tbl_stack",
        .header = "_{strata}_"
      )
  )

  expect_true(all(grepl("_Grade .*_", tbl$df_strata$header)))
  expect_true(all(grepl("_Grade .*_", tbl$table_body$groupname_col)))
})

test_that("tbl_strata(.combine_with) works as expected", {
  # non-default .combine_with (tbl_stack)
  expect_message(
    tbl <- trial |>
      select(grade, stage, trt) |>
      mutate(grade = paste("Grade", grade)) |>
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x |>
          tbl_summary(by = trt) |>
          add_p(),
        .combine_with = "tbl_stack"
      )
  )

  # df_strata correct
  expect_equal(nrow(tbl$df_strata), 3)
  expect_equal(names(tbl$df_strata), c("strata_1", "header"))

  # correct subtables
  expect_equal(length(tbl$tbls), 3)
  expect_true("groupname_col" %in% names(tbl$table_body))
  expect_true(all(c("Grade I", "Grade II", "Grade III") %in% tbl$table_body$groupname_col))
})


test_that("tbl_strata(.combine_args) works as expected", {
  expect_silent(
    tbl <- trial |>
      select(grade, stage, trt) |>
      tbl_strata(
        strata = trt,
        .tbl_fun =
          ~ .x |>
          tbl_summary(),
        .combine_args = list(tab_spanner = FALSE)
      )
  )

  # no spanning header added
  expect_true(tbl$table_styling$spanning_header$spanning_header |> is_empty())
})

test_that("tbl_strata2 works with standard use", {
  expect_silent(
    tbl <- trial |>
      select(grade, response) |>
      tbl_strata2(
        strata = grade,
        .tbl_fun =
          ~ .x |>
          tbl_summary(
            include = response,
            label = list(response = .y),
            missing = "no"
          )
      )
  )

  # df_strata correct
  expect_equal(nrow(tbl$df_strata), 3)
  expect_equal(names(tbl$df_strata), c("strata_1", "header"))

  # correct subtables
  expect_equal(length(tbl$tbls), 3)
})

test_that("tbl_strata(.stack_group_header) produces defunct warning", {
  lifecycle::expect_defunct(
    trial |>
      select(grade, stage, trt) |>
      mutate(grade = paste("Grade", grade)) |>
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x |>
          tbl_summary(by = trt) |>
          add_p(),
        .combine_with = "tbl_stack",
        .stack_group_header = TRUE
      )
  )
})

test_that("tbl_strata(.quiet) produces deprecation warning", {
  lifecycle::expect_deprecated(
    trial |>
      select(grade, response, stage, trt) |>
      mutate(grade = paste("Grade", grade)) |>
      tbl_strata(
        strata = grade,
        .tbl_fun = ~ .x |> tbl_summary(),
        .quiet = TRUE
      )
  )
})

test_that("tbl_strata2(.quiet) produces deprecation warning", {
  lifecycle::expect_deprecated(
    trial |>
      select(grade, response, stage, trt) |>
      mutate(grade = paste("Grade", grade)) |>
      tbl_strata2(
        strata = grade,
        .tbl_fun = ~ .x |> tbl_summary(),
        .quiet = TRUE
      )
  )
})

test_that("tbl_strata works with survey objects", {
  skip_if_not(is_pkg_installed("survey"))

  svy_obj <- survey::svydesign(~1, data = trial, weights = ~1)

  expect_silent(
    svy_obj |>
      tbl_strata(
        strata = grade,
        ~ tbl_svysummary(.x, by = trt, include = c(stage, trt), percent = "cell")
      )
  )

  # error when multiple strata variables selected
  expect_error(
    svy_obj |>
      tbl_strata(
        strata = c(grade, trt),
        ~ tbl_svysummary(.x, by = trt, include = c(stage, trt), percent = "cell")
      )
  )
})
