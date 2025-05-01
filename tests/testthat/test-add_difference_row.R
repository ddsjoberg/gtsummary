test_that("add_difference_row() works", {
  expect_silent(
    tbl <- trial |>
      tbl_summary(by = grade, include = c(age, response), missing = "no") |>
      add_difference_row(
        reference = "I",
        statistic = everything() ~ c("{estimate}", "{conf.low}, {conf.high}", "{p.value}")
      )
  )
  expect_snapshot(as.data.frame(as.data.frame(tbl)))

  # check results in ARD
  # Age I vs III
  expect_equal(
    tbl$cards$add_difference_row$age$`'I' vs. 'III'` |>
      dplyr::select(-c("stat_fmt", "fmt_fn")),
    trial |>
      dplyr::filter(grade != "II") |>
      cardx::ard_stats_t_test(variables = "age", by = "grade") |>
      dplyr::select(-"fmt_fn")
  )
  # Response I vs III
  expect_equal(
    tbl$cards$add_difference_row$response$`'I' vs. 'III'` |>
      dplyr::select(-c("stat_fmt", "fmt_fn")),
    trial |>
      dplyr::filter(grade != "II") |>
      cardx::ard_stats_prop_test(variables = "response", by = "grade") |>
      dplyr::select(-"fmt_fn")
  )
})
