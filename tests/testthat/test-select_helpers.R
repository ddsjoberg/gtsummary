skip_on_cran()

test_that("test-select helpers", {
  expect_equal(
    .select_to_varnames(select = vars(hp, mpg), data = mtcars),
    dplyr::select(mtcars, hp, mpg) %>% colnames()
  )

  expect_equal(
    .select_to_varnames(select = mpg, data = mtcars),
    dplyr::select(mtcars, mpg) %>% colnames()
  )

  expect_equal(
    .select_to_varnames(select = "mpg", data = mtcars),
    dplyr::select(mtcars, "mpg") %>% colnames()
  )

  expect_equal(
    .select_to_varnames(select = c("hp", "mpg"), data = mtcars),
    dplyr::select(mtcars, c("hp", "mpg")) %>% colnames()
  )

  expect_equal(
    .select_to_varnames(select = c(hp, mpg), data = mtcars),
    dplyr::select(mtcars, c(hp, mpg)) %>% colnames()
  )

  expect_equal(
    .select_to_varnames(select = NULL, data = mtcars),
    NULL
  )

  expect_equal(
    .select_to_varnames(select = vars(dplyr::everything(), -mpg), data = mtcars),
    dplyr::select(mtcars, dplyr::everything(), -mpg) %>% colnames()
  )

  # checking the all_*() function label the types accurately
  expect_true(
    tbl_summary(
      trial,
      label = list(
        all_continuous() ~ "continuous",
        all_categorical() ~ "categorical",
        all_dichotomous() ~ "dichotomous"
      )
    ) %>%
      purrr::pluck("meta_data") %>%
      {
        .$summary_type == .$var_label
      } %>%
      all()
  )

  stage_variable <- "stage"
  expect_equal(
    purrr::map_chr(
      c("trt", "grade", stage_variable),
      ~ tbl_summary(trial, by = all_of(.x)) %>%
        purrr::pluck("by")
    ),
    c("trt", "grade", stage_variable)
  )

  expect_snapshot(
    tbl_summary(trial, statistic = all_continuous() ~ "{mean}") %>% as.data.frame()
  )

  expect_snapshot(
    tbl_summary(trial, statistic = all_categorical() ~ "{n}") %>% as.data.frame()
  )

  expect_snapshot(
    tbl_summary(trial, statistic = all_dichotomous() ~ "{n}") %>% as.data.frame()
  )

  expect_snapshot(
    tbl_summary(trial, statistic = all_categorical(dichotomous = FALSE) ~ "{n}") %>% as.data.frame()
  )

  expect_snapshot(
    tbl_summary(trial, by = trt, include = c(stage, response, trt)) %>%
      add_p(
        test = everything() ~ "fisher.test",
        test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE)
      ) %>%
      as.data.frame()
  )

  df <-
    data.frame(
      stat = NA,
      stat_0 = NA,
      stat_0_1 = NA,
      stat_0_11 = NA,
      stat_1 = NA,
      stat_11 = NA,
      stat_1_1 = NA,
      stat_11_1 = NA
    )

  expect_equal(
    select(df, all_stat_cols()) %>% names(),
    c("stat_0", "stat_1", "stat_11", "stat_0_1", "stat_0_11", "stat_1_1", "stat_11_1")
  )
  expect_equal(
    select(df, all_stat_cols(FALSE)) %>% names(),
    c("stat_1", "stat_11", "stat_1_1", "stat_11_1")
  )

  expect_equal(
    trial[c("age", "marker")] %>%
      tbl_summary(
        type = marker ~ "continuous2",
        label = all_continuous() ~ "YASS"
      ) %>%
      purrr::pluck("meta_data", "var_label"),
    c("YASS", "YASS")
  )

  expect_equal(
    trial[c("age", "marker")] %>%
      tbl_summary(
        type = marker ~ "continuous2",
        label = all_continuous(FALSE) ~ "YASS"
      ) %>%
      purrr::pluck("meta_data", "var_label"),
    c("YASS", "Marker Level (ng/mL)")
  )

  expect_equal(
    trial[c("age", "marker")] %>%
      tbl_summary(
        type = marker ~ "continuous2",
        label = all_continuous2() ~ "YASS"
      ) %>%
      purrr::pluck("meta_data", "var_label"),
    c("Age", "YASS")
  )
})
