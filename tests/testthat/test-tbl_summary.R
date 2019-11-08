context("test-tbl_summary")

test_that("tbl_summary creates output without error/warning (no by var)", {
  expect_error(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x, sort = list(all_categorical() ~ "frequency"))),
    NA
  )
  expect_warning(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x)),
    NA
  )
})


test_that("tbl_summary creates output without error/warning (with by var)", {
  expect_error(
    tbl_summary(mtcars, by = am),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = am),
    NA
  )
})

test_that("tbl_summary allows for named list input", {
  expect_error(
    tbl_summary(mtcars, by = am, label = list(mpg = "New mpg", cyl = "New cyl")),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = am, label = list(mpg = "New mpg", cyl = "New cyl")),
    NA
  )
})


test_that("tbl_summary throws errors/messages with bad 'sort = ' specifications", {
  expect_error(
    tbl_summary(mtcars, sort = list(all_categorical() ~ c("frequency", "two"))),
    "*"
  )
  expect_error(
    tbl_summary(mtcars, sort = list(all_categorical() ~ "freq5555uency")),
    "*"
  )
})

test_that("tbl_summary value argument works properly", {
  expect_error(
    tbl_summary(trial, value = "grade" ~ "III"),
    NA
  )
})


test_that("tbl_summary returns errors with bad inputs", {
  expect_error(
    tbl_summary(tibble::tibble()),
    "*"
  )
  expect_error(
    tbl_summary(tibble::tibble(t = integer())),
    "*"
  )
  expect_error(
    tbl_summary(list(test = 5)),
    "*"
  )
  expect_error(
    tbl_summary(trial, by = THIS_IS_NOT_A_VARIABLE),
    "*"
  )
  expect_message(
    tbl_summary(trial, by = response), # should get message about missing data
    "*"
  )
  expect_error(
    tbl_summary(trial, type = response),
    "*"
  )
  expect_error(
    tbl_summary(trial, value = "0"),
    "*"
  )
  expect_error(
    tbl_summary(trial, label = "Age"),
    "*"
  )
  expect_error(
    tbl_summary(trial, statistic = "{mean}"),
    "*"
  )
  expect_error(
    tbl_summary(trial, digits = 0),
    "*"
  )
  expect_error(
    tbl_summary(trial, sort = list("grade" ~ "frequ55555ency")),
    "*"
  )
})


test_that("tbl_summary-testing tidyselect parsing", {
  trial2 <- trial
  trial2$`bad trt` <- trial2$trt
  trial2$`bad grade` <- trial2$grade

  expect_error(
    big_test <-
      tbl_summary(
        data = trial2,
        by = `bad trt`,
        type = vars(response, death) ~ "categorical",
        statistic = list(all_continuous() ~ "{min} {max}",
                         c("grade", "stage") ~ "{n}"),
        label = list(age = "Patient Age", vars(stage) ~ "Patient Stage",
                     vars(`bad grade`) ~ "Crazy Grade"),
        digits = list(vars(age) ~ c(2, 3), marker = c(2, 3)),
        value = list(vars(`bad grade`) ~ "III", "stage" ~ "T1"),
        missing = "no"
      ),
    NA
  )

  # checking missing
  expect_equal(
    big_test$table_body %>%
      dplyr::filter(.data$row_type %in% c("missing")) %>%
      nrow()
    ,
    0
  )

  # checking value
  expect_equal(
    big_test$meta_data %>%
      dplyr::filter(.data$variable %in% c("bad grade", "stage")) %>%
      dplyr::pull(.data$summary_type) %>%
      unique()
    ,
    "dichotomous"
  )

  expect_equal(
    big_test$meta_data %>%
      dplyr::filter(.data$variable %in% c("bad grade", "stage")) %>%
      dplyr::pull(.data$dichotomous_value) %>%
      purrr::every(purrr::negate(is.null))
    ,
    TRUE
  )

  # checking digits
  expect_equal(
    big_test$table_body %>%
      dplyr::filter(.data$variable %in% c("age", "marker")) %>%
      dplyr::pull(.data$stat_1) %>%
      stringr::word(1) %>%
      stringr::word(2, sep = stringr::fixed(".")) %>%
      nchar() %>%
      unique()
    ,
    2
  )

  expect_equal(
    big_test$table_body %>%
      filter(.data$variable %in% c("age", "marker")) %>%
      pull(.data$stat_1) %>%
      word(2) %>%
      word(2, sep = fixed(".")) %>%
      nchar() %>%
      unique()
    ,
    3
  )

  # checking label
  expect_equal(
    big_test$meta_data %>%
      dplyr::filter(.data$variable %in% c("age", "stage", "bad grade")) %>%
      dplyr::pull(.data$var_label),
    c("Patient Age", "Patient Stage", "Crazy Grade")
  )

  # checking type
  expect_equal(
    big_test$meta_data %>%
      dplyr::filter(.data$variable %in% c("response", "death")) %>%
      dplyr::pull(.data$summary_type),
    c("categorical", "categorical")
  )

  # checking statistic
  expect_equal(
    big_test$meta_data[c("summary_type", "stat_display")] %>%
      dplyr::filter(.data$summary_type %in% c("continuous")) %>%
      dplyr::distinct() %>%
      dplyr::pull(.data$stat_display),
    c("{min} {max}")
  )
  expect_equal(
    big_test$meta_data[c("variable", "stat_display")] %>%
      dplyr::filter(.data$variable %in% c("grade", "stage")) %>%
      dplyr::pull(.data$stat_display) %>%
      unique(),
    c("{n}")
  )
})
