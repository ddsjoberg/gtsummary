test_that("no errors with standard use", {
  expect_error(
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

  expect_error(
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
      ),
    NA
  )

  expect_error(
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

  expect_error(
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

  expect_error(
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
      ),
    NA
  )

  expect_error(
    trial %>%
      select(grade, stage, trt) %>%
      mutate(grade = paste("Grade", grade)) %>%
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x %>%
            tbl_cross() %>%
            add_p()
      ),
    NA
  )

  expect_error(
    trial %>%
      select(grade, stage, trt) %>%
      mutate(grade = paste("Grade", grade)) %>%
      tbl_strata(
        strata = grade,
        .tbl_fun =
          ~ .x %>%
            tbl_cross() %>%
            add_p()
      ),
    NA
  )

  expect_error(
    survey::svydesign(~1, data = trial, weights = ~1) %>%
      tbl_strata(
        strata = grade,
        ~ tbl_svysummary(.x, by = trt, include = c(stage, trt), percent = "cell") %>%
          modify_header(all_stat_cols() ~ "**{level}**"),
        .combine_with = "tbl_stack"
      ),
    NA
  )
})
