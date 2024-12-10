skip_on_cran()

test_that("tbl_custom_summary() basics", {
  mean_age <- function(data, ...) {
    dplyr::tibble(mean_age = mean(data$age, na.rm = TRUE))
  }

  expect_error(
    tbl1 <-
      trial |>
      tbl_custom_summary(
        include = c("grade", "response", "marker"),
        by = "trt",
        stat_fns = ~mean_age,
        statistic = ~"{mean_age}",
        digits = ~1
      ) |>
      add_overall(last = TRUE) |>
      modify_footnote_header("Mean age", columns = all_stat_cols()) |>
      modify_column_unhide(everything()) |>
      as.data.frame(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl1)

  expect_equal(
    tbl1 |>
      dplyr::filter(variable == "grade", row_type == "level") |>
      select(all_stat_cols(F)),
    trial |>
      select(age, grade, trt) %>%
      dplyr::filter(complete.cases(.)) |>
      dplyr::group_by(trt, grade) |>
      dplyr::summarise(mean_age = mean(age), .groups = "drop") |>
      tidyr::pivot_wider(id_cols = grade, names_from = trt, values_from = mean_age) |>
      select(-1) |>
      set_names(c("stat_1", "stat_2")) |>
      dplyr::mutate_all(~ style_number(., 1)) |>
      as.data.frame()
  )

  expect_equal(
    tbl1 |>
      dplyr::filter(variable == "response", row_type == "label") |>
      dplyr::select(all_stat_cols(F)) |>
      unlist() |>
      unname(),
    trial |>
      dplyr::select(age, response, trt) %>%
      dplyr::filter(complete.cases(.)) |>
      dplyr::group_by(trt, response) |>
      dplyr::summarise(mean_age = mean(age), .groups = "drop") |>
      dplyr::filter(response == 1) |>
      dplyr::pull(mean_age) |>
      style_number(digits = 1)
  )

  expect_equal(
    tbl1$stat_0,
    c(NA, "46.2", "47.5", "48.1", "49.8", "7", "47.0", "10")
  )

  diff_to_great_mean <- function(data, full_data, ...) {
    mean <- mean(data$marker, na.rm = TRUE)
    great_mean <- mean(full_data$marker, na.rm = TRUE)
    diff <- mean - great_mean
    dplyr::tibble(
      mean = mean,
      great_mean = great_mean,
      diff = diff,
      level = ifelse(diff > 0, "high", "low")
    )
  }

  expect_error(
    tbl <-
      trial |>
      tbl_custom_summary(
        include = c("grade", "stage"),
        by = "trt",
        stat_fns = everything() ~ diff_to_great_mean,
        statistic = everything() ~ "{mean} ({level}, diff: {diff})",
        digits = everything() ~ list(1, as.character, 1),
        overall_row = TRUE,
        overall_row_last = TRUE,
        overall_row_label = "All grades & stages"
      ) |>
      add_overall() |>
      add_n() |>
      bold_labels() |>
      italicize_levels() |>
      as.data.frame(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl)
  expect_equal(
    tbl$label,
    c(
      "__Grade__", "_I_", "_II_", "_III_", "__T Stage__", "_T1_",
      "_T2_", "_T3_", "_T4_", "__All grades & stages__"
    )
  )
  expect_equal(
    tbl$n,
    c("200", NA, NA, NA, "200", NA, NA, NA, NA, "200")
  )
  expect_equal(
    tbl$stat_2,
    c(
      NA, "1.0 (high, diff: 0.1)", "0.5 (low, diff: -0.4)", "1.0 (high, diff: 0.1)",
      NA, "0.7 (low, diff: -0.3)", "1.0 (high, diff: 0.1)", "0.9 (high, diff: 0.0)",
      "0.7 (low, diff: -0.2)", "0.8 (low, diff: -0.1)"
    )
  )

  # using data[[variable]]

  mean_ci <- function(data, variable, ...) {
    test <- t.test(data[[variable]])
    dplyr::tibble(
      mean = test$estimate,
      conf.low = test$conf.int[1],
      conf.high = test$conf.int[2]
    )
  }

  expect_error(
    tbl <- trial |>
      tbl_custom_summary(
        include = c("marker", "ttdeath"),
        by = "trt",
        stat_fns = everything() ~ mean_ci,
        statistic = everything() ~ "{mean} [{conf.low}; {conf.high}]"
      ) |>
      as.data.frame(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl)
  expect_equal(
    tbl$stat_1,
    c("1.02 [0.83; 1.20]", "6", "20.2 [19.2; 21.2]")
  )

  expect_error(
    trial |>
      tbl_custom_summary(
        include = c("marker", "ttdeath"),
        by = "trt",
        stat_fns = mean_ci, # incorrect, right syntax is '~ mean_ci'
        statistic = everything() ~ "{mean} [{conf.low}; {conf.high}]"
      )
  )
})

test_that("tbl_custom_summary() manage factor levels with no observation", {
  expect_error(
    tbl <- trial |>
      mutate(grade = factor(grade, levels = union(levels(grade), "IV"))) |>
      tbl_custom_summary(
        include = "grade",
        by = "trt",
        stat_fns = everything() ~ \(data, ...) list(median = median(data[["age"]], na.rm = TRUE)),
        statistic = everything() ~ "{median}",
        overall_row = TRUE
      ) |>
      as.data.frame(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl)

  expect_equal(
    c(tbl$stat_1[tbl$label == "IV"], tbl$stat_2[tbl$label == "IV"]),
    c("NA", "NA")
  )
})

test_that("tbl_custom_summary() helpers work as expected", {
  skip_on_cran()

  # ratio_summary
  expect_error(
    tbl <- trial |>
      tbl_custom_summary(
        include = c("stage"),
        by = "trt",
        stat_fns = everything() ~ ratio_summary("response", "ttdeath"),
        statistic = everything() ~ "{ratio} [{conf.low}; {conf.high}] ({num}/{denom})",
        digits = everything() ~ c(3, 2, 2, 0, 0)
      ) |>
      as.data.frame(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl)

  expect_equal(
    tbl$stat_1,
    c(
      NA, "0.012 [0.00; 0.02] (7/583)", "0.011 [0.00; 0.02] (6/528)",
      "0.019 [0.01; 0.04] (8/426)", "0.016 [0.01; 0.03] (7/445)"
    )
  )

  # proportion_summary
  expect_error(
    tbl <- Titanic |>
      as.data.frame() |>
      tbl_custom_summary(
        include = c("Age", "Class"),
        by = "Sex",
        stat_fns = everything() ~ proportion_summary("Survived", "Yes", weights = "Freq"),
        statistic = everything() ~ "{prop}% ({n}/{N}) [{conf.low}-{conf.high}]",
        digits = everything() ~ list(function(x) {
          style_percent(x, digits = 1)
        }, 0, 0, style_percent, style_percent)
      ) |>
      as.data.frame(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl)

  expect_equal(
    tbl$stat_1,
    c(
      NA, "45.3% (29/64) [33-58]", "20.3% (338/1,667) [18-22]", NA,
      "34.4% (62/180) [28-42]", "14.0% (25/179) [9.4-20]", "17.3% (88/510) [14-21]",
      "22.3% (192/862) [20-25]"
    )
  )
})


test_that("character/date summaries do not cause error", {
  diff_to_great_mean <- function(data, full_data, ...) {
    mean <- mean(data$marker, na.rm = TRUE)
    great_mean <- mean(full_data$marker, na.rm = TRUE)
    diff <- mean - great_mean
    dplyr::tibble(
      mean = mean,
      great_mean = great_mean,
      diff = diff,
      level = ifelse(diff > 0, "high", "low"),
      date = as.Date("2016-06-15")
    )
  }

  expect_error(
    tbl <-
      trial |>
      tbl_custom_summary(
        include = c("stage"),
        by = "trt",
        stat_fns = ~diff_to_great_mean,
        statistic = ~"{mean} ({level}) [{date}]"
      ),
    NA
  )
  expect_snapshot(tbl |> as.data.frame())

  # by variable can be named "variable"
  expect_equal(
    trial |>
      select(age, variable = trt) |>
      tbl_custom_summary(
        by = "variable",
        stat_fns = ~ \(data, ...) dplyr::tibble(mean = mean(data[["age"]], na.rm = TRUE), sd = sd(data[["age"]], na.rm = TRUE)),
        statistic = ~"{mean} ({sd})"
      ) |>
      as.data.frame(),
    trial |>
      select(age, trt) |>
      tbl_custom_summary(
        by = "trt",
        stat_fns = ~ \(data, ...) dplyr::tibble(mean = mean(data[["age"]], na.rm = TRUE), sd = sd(data[["age"]], na.rm = TRUE)),
        statistic = ~"{mean} ({sd})"
      ) |>
      as.data.frame()
  )
})

test_that("full_data contains all observations including missing values", {
  fn <- function(data, full_data, variable, ...) {
    dplyr::tibble(
      Nobs = nrow(data),
      Ntot = nrow(full_data)
    )
  }

  res <-
    tbl_custom_summary(
      trial,
      stat_fns = ~fn,
      statistic = ~"{Nobs}/{Ntot}",
      include = age
    ) |>
    as.data.frame()

  expect_snapshot(res)
  expect_equal(res[[2]][1], "189/200")
})

test_that("character statistic get default fmt_fn, as.character()", {
  diff_to_great_mean <- function(data, full_data, ...) {
    mean <- mean(data$marker, na.rm = TRUE)
    great_mean <- mean(full_data$marker, na.rm = TRUE)
    diff <- mean - great_mean
    dplyr::tibble(
      mean = mean,
      great_mean = great_mean,
      diff = diff,
      level = ifelse(diff > 0, "high", "low")
    )
  }

  expect_equal(
    trial |>
      tbl_custom_summary(
        include = c("grade", "stage"),
        by = "trt",
        stat_fns = ~diff_to_great_mean,
        statistic = ~"{mean} ({level}, diff: {diff})",
        digits = ~list(level = as.character),
        overall_row = TRUE
      ) |>
      getElement("cards") |>
      getElement(1L) |>
      dplyr::filter(stat_name %in% "level") |>
      dplyr::pull(fmt_fn) |>
      unique(),
    list(as.character)
  )
})
