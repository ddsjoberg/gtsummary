test_that("add_nevent.tbl_survfit() works", {
  tbl <- trial |>
    tbl_survfit(
      include = trt,
      y = "Surv(ttdeath, death)",
      times = 12
    )

  # total N events added to table is accurate
  expect_silent(
    res <- tbl |> add_nevent()
  )

  expect_equal(
    as.data.frame(res, col_label = FALSE)$nevent,
    c("112", NA, NA)
  )

  # stacked fits work
  fit1 <- survival::survfit(survival::Surv(ttdeath, death) ~ 1, trial)
  fit2 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)

  expect_silent(
    res1 <- list(fit1, fit2) |>
      tbl_survfit(times = c(12, 24)) |>
      add_nevent()
  )
  expect_equal(
    as.data.frame(res1, col_label = FALSE)$nevent,
    c("112", "112", NA, NA)
  )


  # add_nevent.tbl_survfit does not accept additional arguments (yet)
  expect_error(
    res2 <- tbl |> add_nevent(location = "level"),
    regexp = "`...` must be empty"
  )

  # mess with the tbl_survfit object to trigger error
  res3 <- list(fit1, fit2) |>
    tbl_survfit(times = c(12, 24))
  class(res3$inputs$x[[1]]) <- NULL
  expect_error(
    res3 |> add_nevent(),
    regexp = "objects must have class"
  )
})
