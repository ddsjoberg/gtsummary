skip_on_cran()

trial2 <- rbind(trial, trial)

test_that("update_header_stats() works with tables with a `by` variable", {
  expect_message(
    tbl <- tbl_summary(trial, by = trt, include = age) |>
      update_header_stats(card = cards::ard_tabulate(trial2, variables = trt)),
    "must update the headers"
  )

  header <- tbl$table_styling$header
  expect_equal(header$modify_stat_level[header$column %in% c("stat_1", "stat_2")], c("Drug A", "Drug B"))
  expect_equal(header$modify_stat_n[header$column %in% c("stat_1", "stat_2")], c(196, 204))
  expect_equal(header$modify_stat_p[header$column %in% c("stat_1", "stat_2")], c(0.49, 0.51))
  # the total N is updated for every column
  expect_true(all(header$modify_stat_N == 400))
  expect_equal(names(tbl$call_list), c("tbl_summary", "update_header_stats"))

  # the updated stats are placed in the headers
  expect_equal(
    tbl |>
      modify_header(all_stat_cols() ~ "{level}, N = {n}/{N}") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(.data$column %in% c("stat_1", "stat_2")) |>
      dplyr::pull("label"),
    c("Drug A, N = 196/400", "Drug B, N = 204/400")
  )
})

test_that("update_header_stats() works with tables with a `by` variable and overall column", {
  tbl <- tbl_summary(trial, by = trt, include = age) |>
    add_overall() |>
    update_header_stats(card = cards::ard_tabulate(trial2, variables = trt)) |>
    suppressMessages()

  header <- tbl$table_styling$header
  expect_equal(header$modify_stat_n[header$column %in% c("stat_0", "stat_1", "stat_2")], c(400, 196, 204))
  expect_equal(header$modify_stat_p[header$column %in% c("stat_0", "stat_1", "stat_2")], c(1, 0.49, 0.51))
  expect_true(all(header$modify_stat_N == 400))
})

test_that("update_header_stats() works with tables without a `by` variable", {
  tbl <- tbl_summary(trial, include = age) |>
    update_header_stats(card = cards::ard_total_n(trial2)) |>
    suppressMessages()

  header <- tbl$table_styling$header
  expect_true(all(header$modify_stat_N == 400))
  expect_true(all(header$modify_stat_n == 400))
  expect_true(all(header$modify_stat_p == 1))
  expect_true(all(header$modify_stat_level == "Overall"))

  expect_equal(
    tbl |>
      modify_header(stat_0 ~ "N = {N}") |>
      getElement("table_styling") |>
      getElement("header") |>
      dplyr::filter(.data$column == "stat_0") |>
      dplyr::pull("label"),
    "N = 400"
  )
})

test_that("update_header_stats() works with survey tables", {
  skip_if_not(is_pkg_installed("survey"))
  svy <- survey::svydesign(~1, data = trial, weights = ~1)
  svy2 <- survey::svydesign(~1, data = trial2, weights = ~1)

  tbl <- tbl_svysummary(svy, by = trt, include = age) |>
    update_header_stats(card = cards::ard_tabulate(svy2, variables = trt)) |>
    suppressMessages()
  header <- tbl$table_styling$header
  expect_equal(header$modify_stat_n_unweighted[header$column %in% c("stat_1", "stat_2")], c(196, 204))
  expect_true(all(header$modify_stat_N_unweighted == 400))

  tbl <- tbl_svysummary(svy, include = age) |>
    update_header_stats(card = cards::ard_total_n(svy2)) |>
    suppressMessages()
  header <- tbl$table_styling$header
  expect_true(all(header$modify_stat_n_unweighted == 400))
  expect_true(all(header$modify_stat_p_unweighted == 1))
})

test_that("update_header_stats() messaging", {
  tbl_by <- tbl_summary(trial, by = trt, include = age)
  tbl_overall <- tbl_summary(trial, include = age)

  # card must be a card object
  expect_snapshot(error = TRUE, update_header_stats(tbl_by, card = trial))

  # no group columns
  expect_snapshot(
    error = TRUE,
    update_header_stats(tbl_by, card = cards::ard_tabulate(trial, by = grade, variables = trt))
  )

  # only a single variable
  expect_snapshot(
    error = TRUE,
    update_header_stats(tbl_by, card = cards::ard_tabulate(trial, variables = c(trt, grade)))
  )

  # number of levels must match the number of stat columns
  expect_snapshot(
    error = TRUE,
    update_header_stats(tbl_by, card = cards::ard_tabulate(trial, variables = grade))
  )

  # wrong ARD type for the table
  expect_snapshot(error = TRUE, update_header_stats(tbl_by, card = cards::ard_total_n(trial)))
  expect_snapshot(
    error = TRUE,
    update_header_stats(tbl_overall, card = cards::ard_tabulate(trial, variables = trt))
  )

  # table must have stat columns
  expect_snapshot(
    error = TRUE,
    tbl_regression(lm(age ~ trt, trial)) |>
      update_header_stats(card = cards::ard_total_n(trial))
  )
})
