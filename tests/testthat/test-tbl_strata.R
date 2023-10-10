skip_on_cran()

test_that("no errors with standard use", {
  expect_error(
    tbl <-
      trial %>%
      select(age, grade, stage, trt) %>%
      mutate(grade = paste("Grade", grade)) %>%
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x %>%
            tbl_summary(by = trt) %>%
            add_p()
      ),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())

  expect_snapshot(
    trial %>%
      select(age, grade, stage, trt) %>%
      mutate(
        grade = paste("Grade", grade),
        stage = paste("Stage", stage)
      ) %>%
      tbl_strata(
        strata = c(grade, stage),
        .tbl_fun =
          ~ .x %>%
            tbl_summary(by = trt) %>%
            add_p(test = all_continuous() ~ "t.test")
      ) %>%
      as.data.frame()
  )

  expect_error(
    tbl <-
      trial %>%
      select(age, grade, stage, trt) %>%
      mutate(grade = paste("Grade", grade)) %>%
      tbl_strata(
        strata = grade,
        .tbl_fun = tbl_summary,
        by = trt,
        missing = "no"
      ),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())

  expect_error(
    tbl <-
      trial %>%
      select(age, grade, stage, trt) %>%
      mutate(grade = paste("Grade", grade)) %>%
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x %>%
            tbl_summary(by = trt) %>%
            add_p(),
        .combine_with = "tbl_stack"
      ),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())

  expect_snapshot(
    trial %>%
      select(age, grade, stage, trt) %>%
      mutate(grade = paste("Grade", grade)) %>%
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x %>%
            tbl_uvregression(
              y = age,
              method = lm
            )
      ) %>%
      as.data.frame()
  )

  expect_snapshot(
    trial %>%
      select(grade, stage, trt) %>%
      mutate(grade = paste("Grade", grade)) %>%
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x %>%
            tbl_cross() %>%
            add_p()
      ) %>%
      as.data.frame()
  )

  expect_snapshot(
    trial %>%
      select(grade, stage, trt) %>%
      mutate(grade = paste("Grade", grade)) %>%
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x %>%
            tbl_cross() %>%
            add_p()
      ) %>%
      as.data.frame()
  )

  expect_snapshot(
    trial %>%
      select(grade, response) %>%
      tbl_strata2(
        strata = grade,
        .tbl_fun =
          ~ .x %>%
            tbl_summary(
              include = response,
              label = list(response = .y),
              missing = "no"
            )
      ) %>%
      as.data.frame()
  )

  skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))
  expect_snapshot(
    survey::svydesign(~1, data = trial, weights = ~1) %>%
      tbl_strata(
        strata = grade,
        ~ tbl_svysummary(.x, by = trt, include = c(stage, trt), percent = "cell") %>%
          modify_header(all_stat_cols() ~ "**{level}**"),
        .combine_with = "tbl_stack"
      ) %>%
      as.data.frame()
  )

  # check .combine_args works, no spanning header anywhere
  expect_error(
    tbl <-
      trial %>%
      select(grade, stage, trt) %>%
      tbl_strata(
        strata = trt,
        .tbl_fun =
          ~ .x %>%
            tbl_summary(),
        .combine_args = list(tab_spanner = FALSE)
      ),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())
  expect_true(all(is.na(tbl$table_styling$header$spanning_header)))

  lifecycle::expect_defunct(
    trial %>%
      select(age, trt, grade) %>%
      tbl_strata(
        strata = trt,
        .tbl_fun =
          ~ .x %>%
            tbl_summary(by = grade, missing = "no") %>%
            add_n(),
        .stack_group_header = TRUE,
        .combine_with = "tbl_stack"
      )
  )
})
