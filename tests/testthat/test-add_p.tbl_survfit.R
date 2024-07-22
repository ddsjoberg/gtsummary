test_that("add_p.tbl_survfit() works", {
  tbl <- trial |>
    tbl_survfit(
      include = trt,
      y = "survival::Surv(ttdeath, death)",
      times = 12
    )

  # p-value added to table
  expect_snapshot(
    tbl |> add_p() |> as.data.frame()
  )
})

test_that("add_p.tbl_survfit(test) works", {
  tbl1 <- trial |>
    tbl_survfit(
      include = trt,
      y = "survival::Surv(ttdeath, death)",
      times = 12
    ) |>
    add_p(test = "petopeto_gehanwilcoxon")

    compare <- cardx::ard_survival_survdiff(survival::Surv(ttdeath, death) ~ trt, trial, rho = 1)

  expect_equal(
    tbl1$table_body$p.value[1],
    compare |> dplyr::filter(stat_name == "p.value") |> dplyr::pull(stat) %>% unlist()
  )
})

test_that("add_p.tbl_survfit(pvalue_fun) works", {
  s_ns <- function(x) ifelse(x < 0.05, "S", "N.S.")
  tbl <- trial |>
    tbl_survfit(
      include = trt,
      y = "survival::Surv(ttdeath, death)",
      times = 12
    )

  expect_snapshot(
    tbl |>
      add_p(pvalue_fun = s_ns) |>
      as.data.frame()
  )
})
