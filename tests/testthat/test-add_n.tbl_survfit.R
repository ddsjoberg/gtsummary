test_that("add_n.tbl_survfit() works", {
  tbl <- trial |>
    tbl_survfit(
    include = trt,
    y = "Surv(ttdeath, death)",
    times = 12
  )

  # total N added to table is accurate
  expect_silent(
    res <- tbl |> add_n()
  )

  expect_equal(
    as.data.frame(res, col_label = FALSE)$N,
    c('200', NA, NA)
  )

  # stacked survfits works
  fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
  fit2 <- survfit(Surv(ttdeath, death) ~ grade, trial)

  expect_silent(
    res1 <- list(fit1, fit2) |>
      tbl_survfit(times = c(12, 24)) |>
      add_n()
  )
  expect_equal(
    as.data.frame(res1, col_label = FALSE)$N,
    c('200', '200', NA, NA, NA)
  )


  # add_n.tbl_survfit does not accept additional arguments
  expect_error(
    res2 <- tbl |> add_n(statistic = "{N_nonmiss} / {N_obs}"),
    regexp = "`...` must be empty"
  )

  # mess with the call object, trigger `safe_survfit_eval` function
  trial2 <- NA
  fit1$call$data <- trial2

  expect_error(
    res3 <- list(fit1, fit2) |>
      tbl_survfit(times = c(12, 24)) |>
      add_n(),
    regexp = "error may be a due to the construction of the original"
  )
})
