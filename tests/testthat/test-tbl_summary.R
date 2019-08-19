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

test_that("tbl_summary-inputs with backticks", {
  trial2 <- trial[c("trt", "age")]
  trial2$`bad trt` <- trial2$trt

  expect_error(
    trial2 %>% tbl_summary(by = `bad trt`),
    NA
  )

 expect_equal(
   trial2 %>%
     tbl_summary(label = vars(`bad trt`) ~ "OMG") %>%
     purrr::pluck("meta_data") %>%
     dplyr::filter(.data$variable == "bad trt") %>%
     dplyr::pull(.data$var_label),
   "OMG"
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
  expect_error(
    tbl_summary(trial, by = response),
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
