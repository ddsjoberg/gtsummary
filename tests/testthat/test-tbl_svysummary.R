skip_on_cran()
skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))

d <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
data(api, package = "survey")
dc_light <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc, variables = ~ stype + growth + both)

test_that("tbl_svysummary creates output without error/warning (no by var)", {
  expect_snapshot(
    purrr::map(
      list(d, dc_light),
      ~ tbl_svysummary(.x, sort = list(all_categorical() ~ "frequency")) %>%
        as_tibble()
    )
  )
  expect_warning(
    purrr::map(list(d, dc_light), ~ tbl_svysummary(.x)),
    NA
  )
})


test_that("tbl_svysummary creates output without error/warning (with by var)", {
  statistics <- list(
    all_continuous() ~ "{median} {mean} {sd} {var} {min} {max} {sum} {p25} {p42} {p75} {p89} {mean.std.error} {deff}",
    all_categorical() ~ "{n} {N} {p} | {n_unweighted} {N_unweighted} {p_unweighted} {p.std.error} {deff}"
  )
  expect_snapshot(
    tbl_svysummary(dc_light, by = both, statistic = statistics) %>% as_tibble()
  )
  expect_warning(
    tbl_svysummary(dc_light, by = both, statistic = statistics),
    NA
  )
})

test_that("tbl_svysummary allows for named list input", {
  expect_snapshot(
    tbl_svysummary(d, by = Survived, label = list(Class = "New Class", Sex = "New Sex")) %>%
      as.data.frame()
  )
  expect_warning(
    tbl_svysummary(d, by = Survived, label = list(Class = "New Class", Sex = "New Sex")),
    NA
  )
})


test_that("tbl_svysummary throws errors/messages with bad 'sort = ' specifications", {
  expect_error(
    tbl_svysummary(d, sort = list(all_categorical() ~ c("frequency", "two"))),
    NULL
  )
  expect_error(
    tbl_svysummary(d, sort = list(all_categorical() ~ "freq5555uency")),
    NULL
  )
})

test_that("tbl_svysummary value argument works properly", {
  expect_snapshot(
    tbl_svysummary(d, value = "Class" ~ "1st") %>%
      as.data.frame()
  )
})

test_that("tbl_svysummary works in character inputs for `by=`", {
  my_by_variable <- "Survived"

  expect_snapshot(
    tbl_svysummary(d, by = all_of(my_by_variable)) %>%
      as.data.frame()
  )
  expect_snapshot(
    tbl_svysummary(d, by = "Survived") %>%
      as.data.frame()
  )
  expect_snapshot(
    purrr::map(
      c("Survived", "Class", "Sex", "Age"),
      ~ tbl_svysummary(d, by = all_of(.x)) %>% as_tibble()
    )
  )
})


test_that("tbl_svysummary returns errors with bad inputs", {
  expect_error(
    tbl_svysummary(survey::svydesign(ids = ~1, data = tibble(), weights = ~1)),
    NULL
  )
  expect_error(
    tbl_svysummary(survey::svydesign(ids = ~1, data = tibble(t = integer()), weights = ~1)),
    NULL
  )
  expect_error(
    tbl_svysummary(trial),
    NULL
  )
  expect_error(
    tbl_svysummary(d, by = THIS_IS_NOT_A_VARIABLE),
    NULL
  )
  d$variables$Survived[5:8] <- NA
  expect_message(
    tbl_svysummary(d, by = Survived), # should get message about missing data
    NULL
  )
  expect_error(
    tbl_svysummary(d, type = Survived),
    NULL
  )
  expect_error(
    tbl_svysummary(d, value = "0"),
    NULL
  )
  expect_error(
    tbl_svysummary(d, label = "Age"),
    NULL
  )
  expect_error(
    tbl_svysummary(d, statistic = "{mean}"),
    NULL
  )
  expect_error(
    tbl_svysummary(d, digits = 0),
    NULL
  )
  expect_error(
    tbl_svysummary(d, sort = list("Class" ~ "frequ55555ency")),
    NULL
  )
  expect_error(
    tbl_svysummary(d, by = c("Class", "Survived")),
    NULL
  )

  expect_error(
    tbl_svysummary(d, statistic = everything() ~ "{mean}"),
    NULL
  )
})


test_that("tbl_svysummary-testing tidyselect parsing", {
  trial2 <- trial
  trial2$`bad trt` <- trial2$trt
  trial2$`bad grade` <- trial2$grade

  expect_error(
    big_test <-
      tbl_svysummary(
        data = survey::svydesign(ids = ~1, data = trial2, weights = ~1),
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
      dplyr::pull(.data$stat_1) %>%
      stringr::word(2) %>%
      stringr::word(2, sep = fixed(".")) %>%
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

test_that("tbl_svysummary-order of output columns", {
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
      survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
      tbl_svysummary(by = grade_str) %>%
      purrr::pluck("table_body") %>%
      names() %>%
      {
        .[startsWith(., "stat_")]
      },
    paste0("stat_", 1:3)
  )
})

test_that("tbl_svysummary-all_categorical() use with `type=`", {
  # no variables should be dichotomous
  expect_true(
    !"dichotomous" %in%
      (survey::svydesign(data = trial, ids = ~1, weights = ~1) %>%
        tbl_svysummary(type = all_dichotomous() ~ "categorical") %>%
        purrr::pluck("meta_data") %>%
        dplyr::pull(summary_type))
  )
})


test_that("tbl_svysummary-difftime does not cause error", {
  expect_snapshot(
    df_dplyr_storms %>%
      dplyr::mutate(
        date = ISOdate(year, month, day),
        date_diff = difftime(dplyr::lag(date, 5), date, units = "days")
      ) %>%
      survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
      tbl_svysummary() %>%
      as_tibble()
  )
})


test_that("tbl_svysummary-all missing data does not cause error", {
  design_missing <-
    tibble(
      my_by_var = c(1, 1, 2, 2),
      fct = rep(NA, 4) %>% factor(levels = c("lion", "tiger", "bear")),
      lgl = NA,
      chr = NA_character_,
      int = NA_integer_,
      dbl = NA_real_
    ) %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1)

  expect_error(
    all_missing_no_by <- tbl_svysummary(design_missing, include = -my_by_var),
    NA
  )
  expect_snapshot(all_missing_no_by %>% as.data.frame())

  expect_error(
    all_missing_by <- tbl_svysummary(design_missing, by = my_by_var),
    NA
  )
  expect_snapshot(all_missing_by %>% as.data.frame())

  # making categorical, variables that cannot be summarized as categorical
  expect_snapshot(
    tbl_svysummary(design_missing, by = my_by_var, type = c(int, dbl) ~ "categorical") %>%
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
      survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
      tbl_svysummary(by = response2),
    NA
  )

  expect_equal(
    missing_fct_by$table_body %>% select(starts_with("stat_")) %>% names(),
    c("stat_1", "stat_2", "stat_3")
  )
})


test_that("tbl_svysummary-no error when *data* with single column passed", {
  expect_snapshot(
    trial["trt"] %>%
      as.data.frame() %>%
      survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
      tbl_svysummary(label = trt ~ "TREATMENT GROUP") %>%
      as.data.frame()
  )
})

test_that("tbl_svysummary-no error when by variable is ordered factor", {
  expect_snapshot(
    trial %>%
      dplyr::mutate(grade = as.ordered(grade)) %>%
      survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
      tbl_svysummary(by = grade) %>%
      as.data.frame()
  )
})

test_that("tbl_svysummary-provides similar results than tbl_summary for simple weights", {
  d1 <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  d2 <- as.data.frame(Titanic) %>% tidyr::uncount(Freq)

  # col percentages
  t1 <- d1 %>% tbl_svysummary(include = c(Class, Sex, Age, Survived))
  t2 <- d2 %>% tbl_summary(include = c(Class, Sex, Age, Survived))
  expect_equal(t1$table_body, t2$table_body)
  expect_equal(
    t1$table_styling %>% purrr::list_modify(header = NULL),
    t2$table_styling %>% purrr::list_modify(header = NULL)
  )
  expect_equal(
    t1$table_styling$header %>% select(-contains("unweighted")),
    t2$table_styling$header %>% select(-contains("unweighted"))
  )

  # row percentages
  t1 <- d1 %>%
    tbl_svysummary(include = c(Class, Sex), by = Survived, percent = "row") %>%
    add_overall()
  t2 <- d2 %>%
    tbl_summary(include = c(Class, Sex), by = Survived, percent = "row") %>%
    add_overall()
  expect_equal(t1$table_body, t2$table_body)
  expect_equal(
    t1$table_styling %>% purrr::list_modify(header = NULL),
    t2$table_styling %>% purrr::list_modify(header = NULL)
  )
  expect_equal(
    tmp <- t1$table_styling$header %>% select(-contains("unweighted")),
    t2$table_styling$header %>% select(all_of(names(tmp))) # order of variables can differ
  )

  # cell percentages
  t1 <- d1 %>%
    tbl_svysummary(include = c(Class, Sex), by = Survived, percent = "cell") %>%
    add_overall()
  t2 <- d2 %>%
    tbl_summary(include = c(Class, Sex), by = Survived, percent = "cell") %>%
    add_overall()
  expect_equal(t1$table_body, t2$table_body)
  expect_equal(
    t1$table_styling %>% purrr::list_modify(header = NULL),
    t2$table_styling %>% purrr::list_modify(header = NULL)
  )
  expect_equal(
    tmp <- t1$table_styling$header %>% select(-contains("unweighted")),
    t2$table_styling$header %>% select(all_of(names(tmp))) # order of variables can differ
  )

  statistic <- list(all_continuous() ~ "{mean}", all_categorical() ~ "{n} ({p}%)")
  t1 <- survey::svydesign(~1, data = trial, weights = ~1) %>%
    tbl_svysummary(by = trt, statistic = statistic)
  t2 <- trial %>%
    tbl_summary(by = trt, statistic = statistic)
  expect_equal(t1$table_body, t2$table_body)
  expect_equal(
    t1$table_styling %>% purrr::list_modify(header = NULL),
    t2$table_styling %>% purrr::list_modify(header = NULL)
  )
  expect_equal(
    t1$table_styling$header %>% select(-contains("unweighted")) %>% select(all_of(names(.) %>% sort())),
    t2$table_styling$header %>% select(-contains("unweighted")) %>% select(all_of(names(.) %>% sort()))
  )
})

test_that("tbl_svysummary-calculates unweighted N with continuous variables and {N_obs_unweighted}", {
  t1 <-
    as_tibble(Titanic) %>%
    mutate(age = round(as.numeric(list(runif(1, min = 5, max = 100))))) %>%
    dplyr::ungroup() %>%
    survey::svydesign(data = ., ids = ~1, weights = ~n) %>%
    tbl_svysummary(
      by = Sex,
      include = -n,
      statistic = list(all_continuous() ~ "{N_obs_unweighted}")
    )

  expect_equal(
    t1$meta_data$df_stats[[1]] %>%
      dplyr::pull("N_obs_unweighted") %>%
      dplyr::first(),
    as_tibble(Titanic) %>%
      dplyr::group_by(Sex) %>%
      dplyr::count() %>%
      dplyr::pull(n) %>%
      dplyr::first(),
    ignore_attr = TRUE
  )
})




test_that("tbl_summary(digits=) tests with fn inputs", {
  expect_error(
    tbl_digits <-
      survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) %>%
      tbl_svysummary(
        statistic = all_continuous() ~ "{min} {max}",
        digits = all_continuous() ~ style_sigfig,
        include = c(emer)
      ),
    NA
  )
  expect_snapshot(tbl_digits %>% as.data.frame())

  # checking the display is correct
  expect_equal(
    tbl_digits$table_body %>% filter(variable == "emer") %>% pull(stat_0),
    "0.00 49"
  )
})


test_that("tbl_svysummary() works with date and date/time", {
  df_date <-
    data.frame(
      dates = as.Date("2021-02-20") + 1:10,
      times = as.POSIXct("2021-02-20 20:31:33 EST") + 1:10,
      group = 1:10 %% 2
    ) %>%
    {
      survey::svydesign(~1, data = ., weights = ~1)
    }

  expect_error(
    tbl1 <- df_date %>% tbl_svysummary(),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_equal(
    tbl1 %>% as_tibble() %>% select(last_col()) %>% dplyr::pull(),
    c("2021-02-21 to 2021-03-02", "2021-02-20 20:31:34 to 2021-02-20 20:31:43", "5 (50%)")
  )

  month_year <- function(x) format(x, "%B %Y")
  expect_error(
    tbl1 <- df_date %>% tbl_svysummary(type = everything() ~ "continuous", digits = everything() ~ month_year, include = -group),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_equal(
    tbl1 %>% as_tibble() %>% select(last_col()) %>% dplyr::pull(),
    c("February 2021 to March 2021", "February 2021 to February 2021")
  )

  numeric_to_month_year <- function(x) as.Date(x, origin = "1970-01-01") %>% format("%B %Y")
  numeric_to_month_year2 <- function(x) as.POSIXct(x, origin = "1970-01-01 00:00:00") %>% format("%B %Y")
  expect_error(
    tbl1 <-
      df_date %>%
      tbl_svysummary(
        by = group,
        type = everything() ~ "continuous",
        digits = list(
          dates ~ numeric_to_month_year,
          times ~ numeric_to_month_year2
        )
      ),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_error(
    tbl2 <- df_date %>% tbl_svysummary(by = group),
    NA
  )


  # checking that formatting the unweighted proportion works
  data(api, package = "survey")
  dstrat <-
    survey::svydesign(
      id = ~1, strata = ~stype,
      weights = ~pw, data = apistrat, fpc = ~fpc,
      variables = apistrat[, c("pcttest", "growth", "awards")]
    )
  expect_equal(
    tbl_svysummary(
      data = dstrat,
      include = awards,
      type = awards ~ "categorical",
      statistic = all_categorical() ~ "{p_unweighted}%",
      digits = all_categorical() ~ 2
    ) %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull(),
    c(NA, "43.50%", "56.50%")
  )

  expect_equal(
    tbl_svysummary(
      data = dstrat,
      include = awards,
      type = awards ~ "categorical",
      statistic = all_categorical() ~ "{p_unweighted}%"
    ) %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull(),
    c(NA, "44%", "57%")
  )
})

test_that("tbl_svysummary() works with 0/1 variables", {
  expect_snapshot(
    survey::svydesign(data = trial, ids = ~1, weights = ~1) %>%
      tbl_svysummary(include = response) %>%
      as.data.frame()
  )
})

test_that("tbl_svysummary() works with a factor having only one levem", {
  d <- tibble(fct = factor(c("a", "a", "a", "a", "a"))) %>%
    survey::svydesign(data = ., ids = ~1, weights = ~1)

  expect_error(
    res <- d %>% tbl_svysummary(),
    NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_equal(
    res$table_body$stat_0,
    c(NA, "5 (100%)")
  )
})
