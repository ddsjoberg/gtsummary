skip_on_cran()

test_that("tbl_survfit(times) works", {
  expect_silent(
    trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        times = 12
      )
  )
})

test_that("tbl_survfit(probs) works", {
  expect_silent(
    trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        probs = 0.5
      )
  )
})

test_that("tbl_survfit works with integer times values", {
  expect_silent(
    trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        times = c(6L, 12L)
      )
  )
})

test_that("Using both times and probs errors correctly", {
  expect_error(
    trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        times = 12,
        probs = 0.5
      ),
    regexp = "Specify one and only one of arguments `times` and `probs`."
  )
})

test_that("Double check results with ard_survival_survfit", {
  check_res <- function(x) {
    y <- tbl_survfit(
      trial,
      include = trt,
      y = "Surv(ttdeath, death)",
      times = 12,
      statistic = paste0("{", x, "}")
    )
    return(y)
  }

  check_comp <- function(x) {
    compare = survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial) |>
      cardx::ard_survival_survfit(times = 12) %>%
      as.data.frame |>
      dplyr::filter(stat_name == x) |>
      dplyr::pull(stat) |>
      unlist()
    return(compare)
  }


  expect_equal(
    check_res("estimate")$table_body$stat_1,
    c(NA, paste0(round(check_comp("estimate")*100), "%"))
  )

  expect_equal(
    check_res("conf.low")$table_body$stat_1,
    c(NA, paste0(round(check_comp("conf.low")*100), "%"))
  )

  expect_equal(
    check_res("conf.high")$table_body$stat_1,
    c(NA, paste0(round(check_comp("conf.high")*100), "%"))
  )
})

test_that("tbl_survfit(statistic) works", {
  expect_silent(
    tbl <- trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        times = 12,
        statistic = "{estimate} [{conf.low} - {conf.high}]"
      )
  )
  expect_equal(
    tbl$table_body$stat_1,
    c(NA, "91% [85% - 97%]", "86% [80% - 93%]")
  )
})

test_that("tbl_survfit(label) works", {
  expect_silent(
    tbl1 <- trial |>
      tbl_survfit(
        include = c(trt, grade),
        y = "Surv(ttdeath, death)",
        times = 12,
        label = list(trt = "TREATMENT", grade = "GRADE")
      )
  )
  expect_equal(
    tbl1$table_body |> dplyr::filter(row_type == "label") |> dplyr::pull(label),
    c(trt = "TREATMENT", grade = "GRADE")
  )
})

test_that("tbl_survfit(label_header) works", {
  expect_silent(
    tbl2 <- trial |>
      tbl_survfit(
        include = c(trt, grade),
        y = "Surv(ttdeath, death)",
        times = 12,
        label_header = "Time ({time})"
      )
  )
  expect_equal(
    tbl2$table_styling$header |> dplyr::filter(column == "stat_1") |> dplyr::pull(label),
    "Time (12)"
  )
})


test_that("tbl_survfit(estimate_fun) works", {
  test_perc <- function(x) {
    y <- paste0(round(x, 2)*100, "%%")
  }

  expect_silent(
    tbl3 <- trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        times = 12,
        estimate_fun = test_perc
      )
  )
  expect_equal(
    tbl3$table_body$stat_1,
    c(NA, "91%% (85%%, 97%%)", "86%% (80%%, 93%%)")
  )
})

test_that("tbl_survfit(missing) works", {
  expect_silent(
    tbl4 <- trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        probs = 0.5,
        missing = "???"
      )
  )
  expect_equal(
    tbl4$table_body$stat_1,
    c(NA, "24 (21, ???)", "21 (18, ???)")
  )
})

test_that("tbl_survfit(type) works", {
  expect_silent(
    tbl5 <- trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        time = 12,
        type = "risk"
      )
  )
  expect_equal(
    tbl5$table_body$stat_1,
    c(NA, "9.2% (3.3%, 15%)", "14% (6.8%, 20%)")
  )

  expect_silent(
    tbl6 <- trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        time = 12,
        type = "cumhaz"
      )
  )
  expect_equal(
    tbl6$table_body$stat_1,
    c(NA, "9.6% (3.3%, 16%)", "15% (7.0%, 23%)")
  )
})

test_that("tbl_survfit(type with prob) errors properly", {
  expect_error(
    trial |>
      tbl_survfit(
        include = trt,
        y = "Surv(ttdeath, death)",
        prob = 0.5,
        type = "risk"
      ),
    regexp = "Cannot use `type` argument when `probs` argument specifed."
  )
})
