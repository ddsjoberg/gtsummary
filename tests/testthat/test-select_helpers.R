test_that("test-select helpers", {
  expect_equal(
    var_input_to_string(mtcars, select_input = vars(hp, mpg)),
    dplyr::select(mtcars, hp, mpg) %>% colnames()
  )

  expect_equal(
    var_input_to_string(mtcars, select_input = mpg),
    dplyr::select(mtcars, mpg) %>% colnames()
  )

  expect_equal(
    var_input_to_string(mtcars, select_input = "mpg"),
    dplyr::select(mtcars, "mpg") %>% colnames()
  )

  expect_equal(
    var_input_to_string(mtcars, select_input = "mpg"),
    dplyr::select(mtcars, "mpg") %>% colnames()
  )

  expect_equal(
    var_input_to_string(mtcars, select_input = c("hp", "mpg")),
    dplyr::select(mtcars, c("hp", "mpg")) %>% colnames()
  )

  expect_equal(
    var_input_to_string(mtcars, select_input = c(hp, mpg)),
    dplyr::select(mtcars, c(hp, mpg)) %>% colnames()
  )

  expect_equal(
    var_input_to_string(mtcars, select_input = NULL),
    NULL
  )

  expect_equal(
    var_input_to_string(mtcars, select_input = vars(dplyr::everything(), -mpg)),
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
      {.$summary_type == .$var_label} %>%
      all()
  )
  expect_true(
    tbl_summary(
      trial,
      label = list(
        all_character() ~ "character",
        all_factor() ~ "factor",
        all_numeric() ~ "numeric",
        all_integer() ~ "integer"
      )
    ) %>%
      purrr::pluck("meta_data") %>%
      {.$class == .$var_label} %>%
      all()
  )

  stage_variable = "stage"
  expect_equal(
    purrr::map_chr(
      c("trt", "grade", stage_variable),
      ~tbl_summary(trial, by = .x) %>%
        purrr::pluck("by")),
    c("trt", "grade", stage_variable)
  )

  # var_input_to_string(mtcars, select_input = vars(everything(), -mpg)
  expect_error(
    tbl_summary(trial, type = all_character() ~ "categorical"), NA
  )

  expect_error(
    tbl_summary(trial, type = all_double() ~ "continuous"), NA
  )

  expect_error(
    tbl_summary(trial, type = all_factor() ~ "categorical"), NA
  )

  expect_error(
    tbl_summary(trial, type = all_integer() ~ "categorical"), NA
  )

  expect_error(
    tbl_summary(trial, statistic = all_continuous() ~ "{mean}"), NA
  )

  expect_error(
    tbl_summary(trial, statistic = all_categorical() ~ "{n}"), NA
  )

  expect_error(
    tbl_summary(trial, statistic = all_dichotomous() ~ "{n}"), NA
  )

  expect_error(
    tbl_summary(trial, statistic = all_categorical(dichotomous = FALSE) ~ "{n}"), NA
  )
})
