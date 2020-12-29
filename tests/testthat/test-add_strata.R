context("test-combine_terms")
testthat::skip_on_cran()

test_that("no errors/warnings with standard use", {
  gg1 <-
    trial %>%
    select(age, grade, trt, stage) %>%
    mutate(grade = paste("Grade", grade)) %>%
    tbl_summary(by = trt, include = -grade, missing = "no") %>%
    add_strata(
      strata = grade,
      additional_fn = ~modify_header(.x, all_stat_cols() ~ "**{level}**")
    )

  gg2 <-
    trial %>%
    select(age, grade, trt, stage) %>%
    mutate(grade = paste("Grade", grade)) %>%
    tbl_summary(by = trt, include = -grade, missing = "no") %>%
    add_strata(
      strata = grade,
      additional_fn = ~modify_header(.x, all_stat_cols() ~ "**{level}**")
    )

  gg3 <-
    trial %>%
    select(age, grade, trt, stage) %>%
    mutate(grade = paste("Grade", grade)) %>%
    tbl_summary(by = trt, include = -grade, missing = "no") %>%
    add_strata(
      strata = grade,
      include_unstratafied = TRUE
    )


  expect_error(gg1, NA)
  expect_warning(gg1, NA)
  expect_error(gg2, NA)
  expect_warning(gg2, NA)
  expect_error(gg3, NA)
  expect_warning(gg3, NA)
})

test_that("unobserved levels appear in resulting table", {
  gg4 <-
    trial %>%
    select(age, grade, trt, stage) %>%
    mutate(stage = ifelse(grade == "I" & stage == "T1", NA, stage)) %>%
    tbl_summary(by = trt, include = -grade, missing = "no") %>%
    add_strata(strata = grade)
  expect_equal(
    gg4$table_body %>% filter(variable == "stage", label == "1") %>% select(stat_1_1, stat_2_1) %>% unlist(),
    c(stat_1_1 = "0 (0%)", stat_2_1 = "0 (0%)")
  )
})


