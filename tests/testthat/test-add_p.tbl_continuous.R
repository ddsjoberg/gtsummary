skip_if_not(is_pkg_installed("cardx", reference_pkg = "gtsummary"))

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

test_that("add_p.tbl_continuous(test) works", {
  expect_silent(
    tbl1 <- trial |>
      tbl_continuous(variable = age, include = grade) |>
      add_p(grade ~ 'kruskal.test')
  )

  compare <- stats::kruskal.test(age ~ grade, trial)

  expect_equal(
    tbl1$table_body$p.value[1],
    compare$p.value
  )
})

test_that("add_p.tbl_continuous(pvalue_fun) works", {
  s_ns <- function(x) ifelse(x < 0.05, "S", "N.S.")
  expect_snapshot(
    trial |>
      tbl_continuous(variable = age, by = trt, include = c(grade, stage)) |>
      add_p(pvalue_fun = s_ns) |>
      as.data.frame()
  )
})

test_that("add_p.tbl_continuous(include) works", {
  expect_silent(
    tbl2 <- trial |>
      tbl_continuous(variable = age, by = trt, include = c(grade, stage)) |>
      add_p(include = grade)
  )

  expect_equal(
    tbl2$table_body |> dplyr::filter(label == "T Stage") |> dplyr::pull(p.value),
    NA_real_,
    ignore_attr = TRUE
  )
})


test_that("add_p.tbl_continuous(test.args) works", {
  expect_silent(
    tbl3 <- trial |>
      tbl_continuous(variable = age, include = trt) |>
      add_p(test = trt ~ "t.test", test.args = all_tests("t.test") ~ list(var.equal = TRUE))
  )

  compare <- t.test(age ~ trt, trial, var.equal = TRUE)

  expect_equal(
    tbl3$table_body$p.value[1],
    compare$p.value
  )
})

test_that("add_p.tbl_continuous(group) works", {
  skip_if_not(is_pkg_installed("lme4", reference_pkg = "gtsummary"))

  expect_silent(
    tbl4 <- trial |>
      tbl_continuous(
        variable = age,
        include = trt
      ) |>
      add_p(test = ~"lme4", group = stage)
  )

  compare <- cardx::ard_stats_anova(
    x = trial |>  tidyr::drop_na(any_of(c("trt", "age", "grade"))),
    formulas = list(
      glue::glue("as.factor(trt) ~ 1 + (1 | stage)") |> stats::as.formula(),
      glue::glue("as.factor(trt) ~ age + (1 | stage)") |> stats::as.formula()
    ),
    method = "glmer",
    method.args = list(family = stats::binomial),
    package = "lme4",
    method_text = "random intercept logistic regression"
  ) |>
    dplyr::filter(stat_name == "p.value") |>
    dplyr::pull(stat)

  expect_equal(
    tbl4$table_body$p.value[1],
    unlist(compare)
  )
})
