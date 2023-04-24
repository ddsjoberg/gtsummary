skip_on_cran()

test_that("tbl_summary creates output without error/warning (no by var)", {
  expect_error(
    lst_tbl <- purrr::map(list(mtcars, iris), ~ tbl_summary(.x, sort = list(all_categorical() ~ "frequency"))),
    NA
  )
  expect_snapshot(purrr::map(lst_tbl, as.data.frame))
  expect_warning(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x)),
    NA
  )
})


test_that("tbl_summary creates output without error/warning (with by var)", {
  expect_snapshot(
    tbl_summary(mtcars, by = am) %>%
      as.data.frame()
  )
  expect_warning(
    tbl_summary(mtcars, by = am),
    NA
  )
})

test_that("tbl_summary allows for named list input", {
  expect_snapshot(
    tbl_summary(mtcars, by = am, label = list(mpg = "New mpg", cyl = "New cyl")) %>%
      as.data.frame()
  )
  expect_warning(
    tbl_summary(mtcars, by = am, label = list(mpg = "New mpg", cyl = "New cyl")),
    NA
  )
})


test_that("tbl_summary throws errors/messages with bad 'sort = ' specifications", {
  expect_error(
    tbl_summary(mtcars, sort = list(all_categorical() ~ c("frequency", "two"))),
    NULL
  )
  expect_error(
    tbl_summary(mtcars, sort = list(all_categorical() ~ "freq5555uency")),
    NULL
  )
})

test_that("tbl_summary value argument works properly", {
  expect_snapshot(
    tbl_summary(trial, value = "grade" ~ "III") %>%
      as.data.frame()
  )
})

test_that("tbl_summary works in character inputs for `by=`", {
  my_by_variable <- "trt"

  expect_snapshot(
    tbl_summary(trial, by = all_of(my_by_variable)) %>%
      as.data.frame()
  )
  expect_snapshot(
    tbl_summary(trial, by = "trt") %>%
      as.data.frame()
  )
  expect_snapshot(
    purrr::map(
      c("trt", "grade", "stage"),
      ~ tbl_summary(trial, by = all_of(.x)) %>% as_tibble()
    )
  )
})


test_that("tbl_summary returns errors with bad inputs", {
  expect_error(
    tbl_summary(tibble::tibble()),
    NULL
  )
  expect_error(
    tbl_summary(tibble::tibble(t = integer())),
    NULL
  )
  expect_error(
    tbl_summary(list(test = 5)),
    NULL
  )
  expect_error(
    tbl_summary(trial, by = THIS_IS_NOT_A_VARIABLE),
    NULL
  )
  expect_message(
    tbl_summary(trial, by = response), # should get message about missing data
    NULL
  )
  expect_error(
    tbl_summary(trial, type = response),
    NULL
  )
  expect_error(
    tbl_summary(trial, value = "0"),
    NULL
  )
  expect_error(
    tbl_summary(trial, label = "Age"),
    NULL
  )
  expect_error(
    tbl_summary(trial, statistic = "{mean}"),
    NULL
  )
  expect_error(
    tbl_summary(trial, digits = 0),
    NULL
  )
  expect_error(
    tbl_summary(trial, sort = list("grade" ~ "frequ55555ency")),
    NULL
  )
  expect_error(
    tbl_summary(trial, by = c("trt", "grade")),
    NULL
  )

  expect_error(
    tbl_summary(trial, statistic = everything() ~ "{mean}"),
    NULL
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
  expect_snapshot(big_test %>% as.data.frame())

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
      dplyr::pull(.data$stat_display) %>%
      unlist(),
    c("{min} {max}")
  )
  expect_equal(
    big_test$meta_data[c("variable", "stat_display")] %>%
      dplyr::filter(.data$variable %in% c("grade", "stage")) %>%
      dplyr::pull(.data$stat_display) %>%
      unique() %>%
      unlist(),
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
      select(grade, grade_str) %>%
      tbl_summary(by = grade_str) %>%
      purrr::pluck("table_body") %>%
      names() %>%
      {
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
  expect_snapshot(
    df_dplyr_storms %>%
      dplyr::mutate(
        date = ISOdate(year, month, day),
        date_diff = difftime(dplyr::lag(date, 5), date, units = "days")
      ) %>%
      tbl_summary() %>%
      as.data.frame()
  )
})


test_that("tbl_summary-all missing data does not cause error", {
  df_missing <-
    tibble(
      my_by_var = c(1, 1, 2, 2),
      fct = rep(NA, 4) %>% factor(levels = c("lion", "tiger", "bear")),
      lgl = NA,
      chr = NA_character_,
      int = NA_integer_,
      dbl = NA_real_
    )

  expect_error(
    all_missing_no_by <- tbl_summary(df_missing %>% select(-my_by_var)),
    NA
  )
  expect_snapshot(all_missing_no_by %>% as.data.frame())

  expect_error(
    all_missing_by <- tbl_summary(df_missing, by = my_by_var),
    NA
  )
  expect_snapshot(all_missing_by %>% as.data.frame())

  # making categorical, variables that cannot be summarized as categorical
  expect_snapshot(
    tbl_summary(df_missing, by = my_by_var, type = vars(int, dbl) ~ "categorical") %>%
      as.data.frame()
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
      mutate(response2 = factor(response) %>% forcats::fct_na_value_to_level(level = "(Missing)")) %>%
      filter(!is.na(response)) %>%
      tbl_summary(by = response2),
    NA
  )
  expect_snapshot(missing_fct_by %>% as.data.frame())

  expect_equal(
    missing_fct_by$table_body %>% select(starts_with("stat_")) %>% names(),
    c("stat_1", "stat_2", "stat_3")
  )
})


test_that("tbl_summary-no error when *data frame* with single column passed", {
  expect_snapshot(
    trial["trt"] %>%
      as.data.frame() %>%
      tbl_summary(label = trt ~ "TREATMENT GROUP") %>%
      as.data.frame()
  )
})


test_that("tbl_summary-no error when by variable is ordered factor", {
  expect_snapshot(
    trial %>%
      dplyr::mutate(grade = as.ordered(grade)) %>%
      tbl_summary(by = grade) %>%
      as.data.frame()
  )
})

test_that("tbl_summary- works with grouped data (it ungroups it first)", {
  expect_snapshot(
    trial %>% dplyr::group_by(response) %>%
      dplyr::select(response, death, trt) %>%
      tbl_summary(by = trt) %>%
      as.data.frame()
  )
})

test_that("tbl_summary-works with ordered factors", {
  expect_snapshot(
    trial %>%
      select(response, trt) %>%
      dplyr::mutate_at(
        vars(response, trt),
        ~ factor(., ordered = TRUE)
      ) %>%
      tbl_summary(by = trt) %>%
      as.data.frame()
  )
})


test_that("tbl_summary-complex environments check", {
  no_fun <- function() {
    grade_level <- "I"
    trial %>%
      dplyr::select(grade) %>%
      tbl_summary(
        label = grade ~ paste("Grade", grade_level)
      )
  }

  expect_error(tbl_env <- no_fun(), NA)
  expect_equal(
    tbl_env$meta_data$var_label[tbl_env$meta_data$variable == "grade"],
    "Grade I"
  )

  no_fun2 <- function() {
    label_var <- "grade"
    trial %>%
      tbl_summary(
        label = all_of(label_var) ~ "Grade, oof",
      )
  }
  expect_error(tbl_env2 <- no_fun2(), NA)
  expect_equal(
    tbl_env2$meta_data$var_label[tbl_env2$meta_data$variable == "grade"],
    "Grade, oof"
  )
})


test_that("tbl_summary creates output without error/warning for continuous2 (no by var)", {
  expect_snapshot(
    purrr::map(
      list(mtcars, iris),
      ~ tbl_summary(.x,
        type = all_continuous() ~ "continuous2",
        sort = list(all_categorical() ~ "frequency")
      ) %>%
        as_tibble()
    )
  )
  expect_warning(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x)),
    NA
  )
})


test_that("tbl_summary creates output without error/warning for continuous2 (with by var)", {
  expect_snapshot(
    tbl_summary(mtcars, by = am, type = all_continuous() ~ "continuous2") %>%
      as.data.frame()
  )
  expect_warning(
    tbl_summary(mtcars, by = am, type = all_continuous() ~ "continuous2"),
    NA
  )

  expect_error(
    tbl_summary(mtcars, by = am, statistic = all_continuous() ~ c("{median}", "{mean}")),
    "*"
  )
})


test_that("tbl_summary(digits=) tests with fn inputs", {
  expect_error(
    tbl_digits <-
      trial %>%
      select(age, marker, grade, response) %>%
      tbl_summary(
        missing = "no",
        statistic = list(
          age ~ "{mean}",
          marker ~ "{mean} {sd} {N_obs} {p_nonmiss}%",
          response ~ "{n} {N} {p}% {N_obs} {p_nonmiss}%"
        ),
        digits = list(
          age ~ function(x) format(x, digits = 2, scientific = TRUE),
          marker ~ list(style_number, 2, 1, function(x) style_percent(x, digits = 2)),
          grade ~ c(1, 1),
          response ~ list(style_number, 1, 2, 1, function(x) style_percent(x, digits = 4))
        )
      ),
    NA
  )
  expect_snapshot(tbl_digits %>% as.data.frame())

  # checking the display is correct
  expect_equal(
    tbl_digits$table_body %>% filter(variable == "age") %>% pull(stat_0),
    with(trial, glue::glue("{format(mean(age, na.rm = TRUE), digits = 2, scientific = TRUE)}")) %>% as.character(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_digits$table_body %>% filter(variable == "marker") %>% pull(stat_0),
    with(trial, glue::glue(
      "{round(mean(marker, na.rm = TRUE))} ",
      "{round(sd(marker, na.rm = TRUE), 2)} ",
      "{sprintf(length(marker),  fmt = '%#.1f')} ",
      "{sprintf(sum(!is.na(marker)) / length(marker) * 100,  fmt = '%#.2f')}%"
    )) %>% as.character(),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_digits$table_body %>% filter(variable == "grade") %>% pull(stat_0) %>% purrr::keep(~ !is.na(.)),
    c("68.0 (34.0%)", "68.0 (34.0%)", "64.0 (32.0%)"),
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_digits$table_body %>% filter(variable == "response") %>% pull(stat_0) %>% purrr::keep(~ !is.na(.)),
    "61 193.0 31.61% 200.0 96.5000%",
    ignore_attr = TRUE
  )
})


test_that("tbl_summary() continuous vars with cat summary vars only", {
  expect_error(
    tbl1 <- trial %>% select(age) %>% tbl_summary(statistic = age ~ "{N_obs}"),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())
  expect_equal(tbl1$table_body$stat_0, c("200", "11"))

  expect_error(
    tbl2 <- trial %>% select(age, trt) %>% tbl_summary(by = trt, statistic = age ~ "{N_obs}"),
    NA
  )
  expect_snapshot(tbl2 %>% as.data.frame())
  expect_equal(tbl2$meta_data$df_stats %>% pluck(1, "N_obs"), c(98, 102),
    ignore_attr = TRUE
  )
})

test_that("tbl_summary() works with date and date/time", {
  df_date <-
    data.frame(
      dates = as.Date("2021-02-20") + 1:10,
      times = as.POSIXct("2021-02-20 20:31:33 EST") + 1:10,
      group = 1:10 %% 2
    )

  expect_error(
    tbl1 <- df_date %>% tbl_summary(),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_equal(
    tbl1 %>% as_tibble() %>% select(last_col()) %>% dplyr::pull(),
    c("2021-02-21 to 2021-03-02", "2021-02-20 20:31:34 to 2021-02-20 20:31:43", "5 (50%)")
  )

  month_year <- function(x) format(x, "%B %Y")
  expect_error(
    tbl1 <- df_date %>% select(-group) %>% tbl_summary(type = everything() ~ "continuous", digits = everything() ~ month_year),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_equal(
    tbl1 %>% as_tibble() %>% select(last_col()) %>% dplyr::pull(),
    c("February 2021 to March 2021", "February 2021 to February 2021")
  )

  expect_error(
    tbl1 <- df_date %>% tbl_summary(by = group, type = everything() ~ "continuous", digits = everything() ~ month_year),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_error(
    tbl2 <- df_date %>% tbl_summary(by = group),
    NA
  )
  expect_snapshot(tbl2 %>% as.data.frame())
})


test_that("unobserved levels can be dichotomously summarized", {
  expect_equal(
    trial %>%
      dplyr::transmute(trt = forcats::fct_expand(trt, "Drug C")) %>%
      tbl_summary(value = trt ~ "Drug C") %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull(stat_0),
    "0 (0%)"
  )
})

test_that("Hmisc labelled data don't error", {
  skip_if_not(broom.helpers::.assert_package("Hmisc", pkg_search = "gtsummary", boolean = TRUE))
  hmisc_data <-
    structure(
      list(
        cd4_count = c(
          30, 97, 210, NA, 358, 242, 126,
          792, 6, 145, 22, 150, 43, 23, 39, 953, 357, 427, 367, 239, 72,
          61, 61, 438, 392, 1092, 245, 326, 42, 135, 199, 158, 17, NA,
          287, 187, 252, 477, 157, NA, NA, 362, NA, 183, 885, 109, 321,
          286, 142, 797
        ),
        unsuccessful = c(
          0, 0, 0, 1, 0, 0, 1, 1, 0, 1,
          1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0,
          0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0
        )
      ),
      row.names = c(NA, 50L),
      class = "data.frame"
    )

  # Add label to CD4 count, using Hmisc package
  Hmisc::label(hmisc_data$cd4_count) <- "CD4 count"

  expect_equal(
    hmisc_data %>%
      tbl_summary(by = unsuccessful, missing = "no") %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull("stat_1"),
    "210 (135, 358)"
  )
})

test_that("no error when by variable omitted from include", {
  expect_snapshot(
    trial %>%
      tbl_summary(by = trt, include = age) %>%
      as.data.frame()
  )
})

test_that("all column names are accepted", {
  df <- data.frame(variable = c(rep("A", 5), rep("B", 5)), value = 1:10)

  expect_snapshot(
    tbl_summary(df, by = "variable") %>%
      as.data.frame()
  )
  expect_snapshot(tbl_summary(df) %>% as.data.frame())
  expect_snapshot(
    tbl_summary(df %>% dplyr::rename(by = variable)) %>% as.data.frame()
  )
  expect_snapshot(
    tbl_summary(df %>% dplyr::rename(by = variable), by = "by") %>% as.data.frame()
  )
})

test_that("no error with factor variable with all NA and no specifed levels", {
  expect_error(
    tbl <-
      trial %>%
      dplyr::mutate(
        has_banana = factor(NA) # We don't know which patients have a banana
      ) %>%
      tbl_summary(by = trt, include = has_banana) %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl)
  expect_equal(
    tbl$stat_1,
    c("0 (NA%)", "98")
  )
})


test_that("modify_*() family works", {
  tbl0 <-
    trial %>%
    select(trt, age, grade) %>%
    tbl_summary(by = trt)
  tbl1 <-
    tbl0 %>%
    add_difference() %>%
    add_n() %>%
    add_overall() %>%
    modify_spanning_header(everything() ~ "N = {N}") %>%
    modify_header(all_stat_cols(FALSE) ~ "{level} N = {n}",
      stat_0 = "Overall N = {N}"
    ) %>%
    modify_footnote(label = "N = {N}") %>%
    modify_caption("N = {N}")
  tbl2 <-
    tbl0 %>%
    add_overall() %>%
    add_p() %>%
    add_n() %>%
    modify_spanning_header(everything() ~ "N = {N}") %>%
    modify_header(all_stat_cols(FALSE) ~ "{level} N = {n}",
      stat_0 = "Overall N = {N}"
    ) %>%
    modify_footnote(label = "N = {N}") %>%
    modify_caption("N = {N}")

  expect_false(
    any(is.na(tbl1$table_styling$header$modify_stat_N))
  )
  expect_false(
    any(is.na(tbl2$table_styling$header$modify_stat_N))
  )

  expect_true(
    all(tbl1$table_styling$header$spanning_header == "N = 200")
  )
  expect_true(
    all(tbl2$table_styling$header$spanning_header == "N = 200")
  )

  expect_equal(
    tbl1$table_styling$header %>%
      filter(startsWith(column, "stat_")) %>%
      pull(label),
    c("Overall N = 200", "Drug A N = 98", "Drug B N = 102")
  )
  expect_equal(
    tbl2$table_styling$header %>%
      filter(startsWith(column, "stat_")) %>%
      pull(label),
    c("Overall N = 200", "Drug A N = 98", "Drug B N = 102")
  )

  expect_equal(
    tbl1$table_styling$caption,
    "N = 200",
    ignore_attr = TRUE
  )
  expect_equal(
    tbl2$table_styling$caption,
    "N = 200",
    ignore_attr = TRUE
  )

  expect_equal(
    tbl_merge(list(tbl0, tbl0))$table_styling$header %>%
      filter(startsWith(column, "stat_")) %>%
      dplyr::pull(modify_stat_n),
    c(98, 102, 98, 102)
  )
})




test_that("no error when data frame contains named vector", {
  df <-
    structure(
      list(
        swallowing = structure(c(NA, 53, 100, 0, 100),
          names = c("", "", "", "", "")
        ),
        salivation = structure(c(NA, 100, 46, 62, 100),
          names = c("", "", "", "", "")
        )
      ),
      row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame")
    )

  expect_snapshot(
    tbl_summary(df, type = list(everything() ~ "continuous")) %>% as.data.frame()
  )
})
