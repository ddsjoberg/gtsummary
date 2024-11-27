skip_on_cran()
skip_if_not(is_pkg_installed(c("broom.helpers", "car", "aod", "cardx", "parameters")))


test_that("add_global_p.tbl_regression works with standard use", {
  tbl <- glm(response ~ age + grade, trial, family = binomial()) |>
    tbl_regression()

  expect_silent(res <- tbl |> add_global_p())

  # 1 p-value per model term
  expect_equal(sum(!is.na(res$table_body$p.value)), 2)
  expect_equal(
    res |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(p.value),
    c("0.092", "0.9", NA, NA, NA)
  )

  # p-values calculated are correct
  expect_equal(
    res$table_body$p.value[1:2],
    (car::Anova(glm(response ~ age + grade, trial, family = binomial()), type = "III"))$`Pr(>Chisq)`
  )
})

test_that("add_global_p.tbl_regression(include) works as expected", {
  tbl <- glm(response ~ age + grade + marker, trial, family = binomial()) |>
    tbl_regression()

  expect_silent(res <- tbl |> add_global_p(include = c(age, marker)))

  # 1 p-value per model term in include
  expect_equal(sum(!is.na(res$table_body$p.value)), 2)
})

test_that("add_global_p.tbl_regression(keep) works as expected", {
  tbl <- glm(response ~ age + grade + marker, trial, family = binomial()) |>
    tbl_regression()

  expect_silent(res <- tbl |> add_global_p(keep = TRUE))

  # 1 p-value per non-empty row
  expect_equal(sum(!is.na(res$table_body$p.value)), 5)

  # p-values calculated are correct
  expect_equal(
    res |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(p.value),
    c("0.10", ">0.9", NA, ">0.9", ">0.9", "0.14")
  )
})

test_that("add_global_p.tbl_regression(anova_fun) works as expected", {
  tbl <- glm(response ~ age + grade, trial, family = binomial()) |>
    tbl_regression()

  expect_silent(res <- tbl |> add_global_p(anova_fun = cardx::ard_aod_wald_test))

  # 1 p-value per model term
  expect_equal(sum(!is.na(res$table_body$p.value)), 2)

  # p-values calculated are correct when anova_fun = aod_wald_test
  expect_equal(
    res$table_body$p.value[1:2],
    cardx::ard_aod_wald_test(glm(response ~ age + grade, trial, family = binomial())) |>
      dplyr::filter(variable %in% c("age", "grade") & stat_name == "p.value") |>
      dplyr::pull(stat) |>
      unlist()
  )
})

test_that("add_global_p.tbl_regression(type) works as expected", {
  tbl <- glm(response ~ age + grade, trial, family = binomial()) |>
    tbl_regression()

  expect_silent(res <- tbl |> add_global_p(type = 2))

  # 1 p-value per model term
  expect_equal(sum(!is.na(res$table_body$p.value)), 2)

  # p-values calculated are correct
  expect_equal(
    res$table_body$p.value[1:2],
    (car::Anova(glm(response ~ age + grade, trial, family = binomial()), type = "III"))$`Pr(>Chisq)`
  )
})

test_that("add_global_p.tbl_regression works when table is modified to hide p-values via tidy_fun", {
  tbl <- glm(response ~ age + grade, trial, family = binomial()) |>
    tbl_regression(
      include = c("age", "grade"),
      tidy_fun = \(x, ...) broom::tidy(x, ...) |> dplyr::select(-p.value)
    )

  expect_silent(res <- tbl |> add_global_p())

  # p-value column is added to the table
  expect_true(
    "p.value" %in% (res |> as_tibble(col_labels = FALSE) |> names())
  )

  # 1 p-value per model term
  expect_equal(sum(!is.na(res$table_body$p.value)), 2)
})

test_that("add_global_p.tbl_regression(quiet) causes deprecation error", {
  tbl <- glm(response ~ age + grade + marker, trial, family = binomial()) |>
    tbl_regression()

  lifecycle::expect_deprecated(
    tbl |> add_global_p(quiet = TRUE),
  )
})

test_that("add_global_p.tbl_regression returns an error for unsupported anova_fun input", {
  tbl <- glm(response ~ age + grade + marker, trial, family = binomial()) |>
    tbl_regression()

  not_anova <- function(x) x + 1

  expect_snapshot(
    res <- tbl |> add_global_p(anova_fun = not_anova),
    error = TRUE
  )
})
