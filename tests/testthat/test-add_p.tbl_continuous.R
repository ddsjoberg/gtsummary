test_that("add_p.tbl_continuous() works", {
  expect_silent(
    tbl <- trial |>
      tbl_continuous(variable = age, by = trt, include = grade) |>
      add_p()
  )

  compare <- summary(lm(age ~ trt + grade, trial))

  expect_equal(
    tbl$table_body$p.value[1],
    1 - pf(compare$fstatistic[1], compare$fstatistic[2], compare$fstatistic[3])
  )
})

test_that("add_p.tbl_continuous(pvalue_fun) works", {
  s_ns <- function(x) ifelse(x < 0.05, "S", "N.S.")
  expect_snapshot(
    trial |>
      tbl_continuous(variable = age, by = trt, include = c(grade, stage)) |>
      add_p(pvalue_fun = s_ns)
  )
})

test_that("add_p.tbl_continuous(include) works", {
  expect_silent(
    tbl1 <- trial |>
      tbl_continuous(variable = age, by = trt, include = c(grade, stage)) |>
      add_p(include = grade)
  )

  # there should **not** be a p-value calculated for stage
  named_NA <- NA_real_
  names(named_NA) <- ""

  expect_equal(
    tbl1$table_body |> dplyr::filter(label == "T Stage") |> dplyr::pull(p.value),
    named_NA
  )
})
