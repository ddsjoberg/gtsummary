skip_on_cran()
t1_summary <- trial |>
  select(trt, age, death) |>
  tbl_summary()

t2_regression <- glm(response ~ age + grade, trial, family = binomial(link = "logit")) |>
  tbl_regression(exponentiate = TRUE)

t3_uvregression <- tbl_uvregression(
  trial |> dplyr::select(response, age, grade),
  method = glm,
  y = response,
  method.args = list(family = binomial),
  exponentiate = TRUE
)

t4_regression_merged <- tbl_merge(list(
  lm(mpg ~ factor(cyl), mtcars) |> tbl_regression(),
  lm(mpg ~ factor(cyl), mtcars |> dplyr::filter(cyl != 4)) |> tbl_regression()
))

test_that("as_tibble works with standard use", {
  expect_silent(res <- as_tibble(t1_summary))
  expect_silent(as_tibble(t1_summary, return_calls = TRUE))
  expect_snapshot(res |> as.data.frame())

  expect_silent(res <- as_tibble(t2_regression))
  expect_snapshot(res |> as.data.frame())

  expect_silent(res <- as_tibble(t3_uvregression))
  expect_snapshot(res |> as.data.frame())
})

test_that("as_tibble(col_labels) works", {
  # col_labels = FALSE
  expect_equal(
    as_tibble(t2_regression, col_labels = FALSE) |> names(),
    c("label", "estimate", "conf.low", "p.value")
  )

  # col_labels = TRUE
  expect_equal(
    as_tibble(t2_regression) |> names(),
    c("**Characteristic**", "**OR**", "**95% CI**", "**p-value**")
  )
})

test_that("as_tibble works with bold/italics", {
  t1_summary <- t1_summary |>
    bold_labels() |>
    italicize_levels()

  t2_regression <- t2_regression |>
    bold_labels() |>
    italicize_levels()

  t3_uvregression <- t3_uvregression |>
    bold_levels() |>
    italicize_labels()

  expect_silent(res <- as_tibble(t1_summary))
  expect_equal(
    names(res),
    c("**Characteristic**", "**N = 200**")
  )
  expect_equal(
    res$`**Characteristic**`,
    c("__Chemotherapy Treatment__", "_Drug A_", "_Drug B_", "__Age__", "_Unknown_", "__Patient Died__")
  )

  expect_silent(res <- as_tibble(t2_regression))
  expect_equal(
    names(res),
    c("**Characteristic**", "**OR**", "**95% CI**", "**p-value**")
  )
  expect_equal(
    res$`**Characteristic**`,
    c("__Age__", "__Grade__", "_I_", "_II_", "_III_")
  )

  expect_silent(res <- as_tibble(t3_uvregression))
  expect_equal(
    names(res),
    c("**Characteristic**", "**N**", "**OR**", "**95% CI**", "**p-value**")
  )
  expect_equal(
    res$`**Characteristic**`,
    c("_Age_", "_Grade_", "__I__", "__II__", "__III__")
  )
})

test_that("as_tibble works with bold_p()", {
  t2_regression <- t2_regression |> bold_p(t = 0.10)

  bold_p_data <- data.frame(
    x = c(rep("YES", 10), rep("NO", 10)),
    y = c(rep("YES", 9), rep("NO", 11)),
    a = c(rep(c(rep("YES", 5), rep("NO", 5)))),
    b = c(rep("YES", 4), rep("NO", 16))
  )

  expect_equal(
    bold_p_data |>
      tbl_summary(by = x) |>
      add_p() |>
      bold_labels() |>
      bold_p() |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(p.value),
    c("__<0.001__", ">0.9", "0.087")
  )

  expect_equal(
    as_tibble(t2_regression, col_labels = FALSE)$p.value,
    c("__0.10__", NA, NA, "0.7", ">0.9")
  )
})

test_that("as_tibble works with formatting functions", {
  t2_regression_modify_fmt <- t2_regression |>
    modify_fmt_fun(
      p.value ~ function(x) style_pvalue(x, digits = 3),
      rows = variable == "grade"
    ) |>
    modify_fmt_fun(
      estimate ~ function(x) style_ratio(x, digits = 4, decimal.mark = ",")
    )

  expect_silent(res <- as_tibble(t2_regression_modify_fmt, col_labels = FALSE))

  # formatted cells
  expect_equal(
    res$p.value,
    c("0.10", NA, NA, "0.688", "0.972")
  )

  # formatted column
  expect_equal(
    res$estimate,
    c("1,0191", NA, NA, "0,8535", "1,0136")
  )

  t3_uvregression_modify_fmt <- t3_uvregression |>
    modify_fmt_fun(
      stat_n ~ function(x) style_number(x, digits = 2),
      rows = variable == "age"
    ) |>
    modify_fmt_fun(
      stat_n ~ label_style_number(digits = 4),
      rows = variable == "grade"
    ) |>
    modify_fmt_fun(
      c(conf.low, conf.high) ~ label_style_sigfig(digits = 3)
    )

  expect_silent(res <- as_tibble(t3_uvregression_modify_fmt, col_labels = FALSE))

  # formatted cell
  expect_equal(
    res$stat_n,
    c("183.00", "193.0000", NA, NA, NA)
  )

  # formatted column
  expect_equal(
    res$conf.low,
    c("0.997, 1.04", NA, NA, "0.446, 2.00", "0.524, 2.29")
  )
})

test_that("as_tibble works with tbl_merge", {
  expect_silent(res <- as_tibble(t4_regression_merged, col_labels = FALSE))
  expect_equal(
    names(res),
    c("label", "estimate_1", "conf.low_1", "p.value_1", "estimate_2", "conf.low_2", "p.value_2")
  )
})

test_that("as_tibble(fmt_missing) works", {
  expect_silent(
    res <- t4_regression_merged |> as_tibble(fmt_missing = TRUE, col_labels = FALSE)
  )

  # fmt_missing = TRUE, default missing_symbol
  expect_equal(
    res$estimate_1,
    c(NA_character_, "—", "-6.9", "-12")
  )
  expect_equal(
    res$estimate_2,
    c(NA_character_, NA_character_, "—", "-4.6")
  )

  # fmt_missing = FALSE
  expect_equal(
    t4_regression_merged |>
      as_tibble(fmt_missing = FALSE, col_labels = FALSE) |>
      dplyr::pull(estimate_1),
    c(NA_character_, NA_character_, "-6.9", "-12")
  )

  # fmt_missing = TRUE, custom missing_symbol
  expect_equal(
    trial |>
      select(age) |>
      tbl_summary() |>
      modify_table_body(
        ~ .x |> mutate(stat_0 = NA_character_)
      ) |>
      modify_table_styling(stat_0, rows = !is.na(label), missing_symbol = "n / a") |>
      as_tibble(fmt_missing = TRUE, col_labels = FALSE) |>
      dplyr::pull(stat_0),
    c("n / a", "n / a")
  )
})

test_that("as_tibble works with grouped columns", {
  tbl <- tbl_summary(trial, include = age, missing = "no")
  tbl <- tbl_stack(list(tbl, tbl), group_header = c("T1", "T2"))

  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(groupname_col),
    c("T1", "T2")
  )
})

test_that("as.data.frame works as expected", {
  expect_equal(
    t1_summary |> as_tibble() |> as.data.frame(),
    t1_summary |> as.data.frame()
  )
})
