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

test_that("tbl_summary works in character inputs for `by=`", {
  my_by_variable <- "trt"

  expect_error(
    tbl_summary(trial, by = my_by_variable),
    NA
  )
  expect_error(
    tbl_summary(trial, by = "trt"),
    NA
  )
  expect_error(
    purrr::map(
      c("trt", "grade", "stage"),
      ~tbl_summary(trial, by = .x)
    ),
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
  expect_error(
    tbl_summary(trial, by = c("trt", "grade")),
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
        statistic = list(
          all_continuous() ~ "{min} {max}",
          c("grade", "stage") ~ "{n}"
        ),
        label = list(
          age ~ "Patient Age", vars(stage) ~ "Patient Stage",
          vars(`bad grade`) ~ "Crazy Grade"
        ),
        digits = list(vars(age) ~ c(2, 3), marker ~ c(2, 3)),
        value = list(`bad grade` = "III", "stage" = "T1"),
        missing = "no"
      ),
    NA
  )

  # checking missing
  expect_equal(
    big_test$table_body %>%
      dplyr::filter(.data$row_type %in% c("missing")) %>%
      nrow(),
    0
  )

  # checking value
  expect_equal(
    big_test$meta_data %>%
      dplyr::filter(.data$variable %in% c("bad grade", "stage")) %>%
      dplyr::pull(.data$summary_type) %>%
      unique(),
    "dichotomous"
  )

  expect_equal(
    big_test$meta_data %>%
      dplyr::filter(.data$variable %in% c("bad grade", "stage")) %>%
      dplyr::pull(.data$dichotomous_value) %>%
      purrr::every(purrr::negate(is.null)),
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
      unique(),
    2
  )

  expect_equal(
    big_test$table_body %>%
      filter(.data$variable %in% c("age", "marker")) %>%
      pull(.data$stat_1) %>%
      word(2) %>%
      word(2, sep = fixed(".")) %>%
      nchar() %>%
      unique(),
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

test_that("tbl_summary-order of output columns", {
  expect_equal(
    trial %>%
      dplyr::mutate(
        grade =
          dplyr::case_when(grade != "III" ~ grade),
        grade_str =
          dplyr::case_when(
            is.na(grade) ~ "Missing Grade",
            grade == "I" ~ "First Grade",
            grade == "II" ~ "Second Grade"
          )
      ) %>%
      dplyr::select(grade, grade_str) %>%
      tbl_summary(by = grade_str) %>%
      purrr::pluck("table_body") %>%
      names() %>% {
        .[startsWith(., "stat_")]
      },
    paste0("stat_", 1:3)
  )
})

test_that("tbl_summary-all_categorical() use with `type=`", {
  # no variables should be dichotomous
  expect_true(
    !"dichotomous" %in%
      (tbl_summary(trial, type = all_dichotomous() ~ "categorical") %>%
         purrr::pluck("meta_data") %>%
         dplyr::pull(summary_type))
  )
})


test_that("tbl_summary-difftime does not cause error", {

  expect_error(
    dplyr::storms %>%
      dplyr::mutate(
        date = ISOdate(year, month, day),
        date_diff = difftime(dplyr::lag(date, 5), date, units = "days")
      ) %>%
      tbl_summary(),
    NA
  )
})


test_that("tbl_summary-all missing data does not cause error", {
  df_missing <-
    tibble(
      my_by_var = c(1,1,2,2),
      fct = rep(NA, 4) %>% factor(levels = c("lion", "tiger", "bear")),
      lgl = NA,
      chr = NA_character_,
      int = NA_integer_,
      dbl = NA_real_
    )

  expect_error(
    all_missing_no_by <- tbl_summary(df_missing %>% dplyr::select(-my_by_var)),
    NA
  )

  expect_error(
    all_missing_by <- tbl_summary(df_missing, by = my_by_var),
    NA
  )

  # making categorical, variables that cannot be summarized as categorical
  expect_error(
    tbl_summary(df_missing, by = my_by_var, type = vars(int, dbl) ~ "categorical"),
    NA
  )

  expect_equal(
    all_missing_no_by$table_body %>%
      filter(variable == "fct", row_type == "level") %>%
      pull(stat_0),
    c("0 (NA%)", "0 (NA%)", "0 (NA%)")
  )

  expect_equal(
    all_missing_no_by$table_body %>%
      filter(variable %in% c("lgl", "chr"), row_type == "label") %>%
      pull(stat_0),
    c("0 (NA%)", "0 (NA%)")
  )

  expect_equal(
    all_missing_no_by$table_body %>%
      filter(variable %in% c("int", "dbl"), row_type == "label") %>%
      pull(stat_0),
    c("NA (NA, NA)", "NA (NA, NA)")
  )

  expect_equal(
    all_missing_by$table_body %>%
      filter(variable == "fct", row_type == "level") %>%
      select(starts_with("stat_")),
    tibble(stat_1 = c("0 (NA%)", "0 (NA%)", "0 (NA%)"), stat_2 = stat_1)
  )

  expect_equal(
    all_missing_by$table_body %>%
      filter(variable %in% c("lgl", "chr"), row_type == "label") %>%
      select(starts_with("stat_")),
    tibble(stat_1 = c("0 (NA%)", "0 (NA%)"), stat_2 = stat_1)
  )

  expect_equal(
    all_missing_by$table_body %>%
      filter(variable %in% c("int", "dbl"), row_type == "label") %>%
      select(starts_with("stat_")),
    tibble(stat_1 = c("NA (NA, NA)", "NA (NA, NA)"), stat_2 = stat_1)
  )

  # unobserved factor level
  expect_error(
    missing_fct_by <-
      trial %>%
      mutate(response2 = factor(response) %>% forcats::fct_explicit_na()) %>%
      filter(!is.na(response)) %>%
      tbl_summary(by = response2),
    NA
  )

  expect_equal(
    missing_fct_by$table_body %>% select(starts_with("stat_")) %>% names(),
    c("stat_1", "stat_2", "stat_3")
  )
})


test_that("tbl_summary-no error when *data frame* with single column passed", {
  expect_error(
    trial["trt"] %>%
      as.data.frame() %>%
      tbl_summary(label = trt ~ "TREATMENT GROUP"),
    NA
  )
})
