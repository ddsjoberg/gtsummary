skip_on_cran()
skip_if_not(is_pkg_installed("knitr", reference_pkg = "gtsummary"))

my_tbl_summary <- trial |>
  select(trt, age, death) |>
  tbl_summary()
my_tbl_regression <- lm(marker ~ age, trial) |> tbl_regression()

test_that("as_kable works with standard use", {
  # include argument does not produce warnings
  expect_silent(my_tbl_summary |> as_kable(include = tibble))

  # no errors replacing default kable argument value
  expect_silent(my_tbl_summary |> as_kable(col.names = NULL))

  expect_silent(kbl_summary <- my_tbl_summary |> as_kable())

  # correct number of rows
  expect_equal(length(kbl_summary), 8)

  # test snapshot
  expect_snapshot(kbl_summary)
})

test_that("as_kable(return_calls) works as expected", {
  # no warnings produced
  expect_silent(kbl <- my_tbl_summary |> as_kable(return_calls = TRUE))

  # correct elements are returned
  expect_equal(
    names(kbl),
    c("tibble", "fmt", "cols_merge", "tab_style_bold", "tab_style_italic",
      "fmt_missing", "cols_hide", "remove_line_breaks", "kable")
  )
})

test_that("as_kable produces column header labels correctly", {
  expect_silent(kbl <- my_tbl_regression |> as_kable())

  expect_equal(
    kbl[1],
    "|**Characteristic** | **Beta** | **95% CI**  | **p-value** |"
  )

  tbl <- my_tbl_regression |>
    modify_column_hide(p.value)
  kbl <- tbl |> as_kable()

  expect_equal(
    kbl[1],
    "|**Characteristic** | **Beta** | **95% CI**  |"
  )
})

test_that("as_kable works with bold/italics", {
  tbl <- my_tbl_summary |>
    bold_labels() |>
    italicize_levels()
  kbl <- tbl |> as_kable()

  # bold labels
  expect_equal(
    kbl[3],
    "|__Chemotherapy Treatment__ |             |"
  )
  expect_equal(
    kbl[8],
    "|__Patient Died__           |  112 (56%)  |"
  )

  # italicized labels
  expect_equal(
    kbl[4],
    "|_Drug A_                   |  98 (49%)   |"
  )
  expect_equal(
    kbl[7],
    "|_Unknown_                  |     11      |"
  )

  tbl <- tbl |>
    modify_table_styling(columns = label, undo_text_format = "bold")
  kbl <- tbl |> as_kable()

  # formatting removed
  expect_equal(
    kbl[3],
    "|Chemotherapy Treatment |             |"
  )
})

test_that("as_kable removes line breaks from table", {
  tbl <- trial |>
    select(trt, age, death) |>
    tbl_summary(label = list(age = "Pt \nAge")) |>
    modify_header(label = "_Test \n Columns_")
  kbl <- tbl |> as_kable()

  expect_equal(
    kbl[1],
    "|_Test Columns_         | **N = 200** |"
  )
  expect_equal(
    kbl[6],
    "|Pt Age                 | 47 (38, 57) |"
  )
})

test_that("as_kable works with tbl_cross", {
  tbl <- tbl_cross(trial, grade, trt)

  expect_silent(kbl_cross <- tbl |> as_kable())
  expect_snapshot(kbl_cross)
})

test_that("as_kable works with tbl_uvregression", {
  tbl <- trial |> tbl_uvregression(method = lm, y = age)

  expect_silent(kbl_uvregression <- tbl |> as_kable())
  expect_snapshot(kbl_uvregression)
})

test_that("as_kable works with tbl_survfit", {
  skip_if_not(is_pkg_installed("survival", reference_pkg = "gtsummary"))

  fit1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)
  tbl <- tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months")

  expect_silent(kbl_survfit <- tbl |> as_kable())
  expect_snapshot(kbl_survfit)
})

test_that("as_kable works with tbl_merge", {
  skip_if_not(is_pkg_installed("survival", reference_pkg = "gtsummary"))

  t1 <- glm(response ~ trt + grade + age, trial, family = binomial) |>
    tbl_regression(exponentiate = TRUE)
  t2 <- survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) |>
    tbl_regression(exponentiate = TRUE)

  tbl_merge_ex1 <- tbl_merge(
    tbls = list(t1, t2),
    tab_spanner = c("**Tumor Response**", "**Time to Death**")
  )

  expect_silent(kbl_merge <- tbl_merge_ex1 |> as_kable())
  expect_snapshot(kbl_merge)
})

test_that("as_kable works with tbl_stack", {
  t1 <- trial |>
    dplyr::filter(trt == "Drug A") |>
    select(age, response, death) |>
    tbl_summary() |>
    modify_header(stat_0 ~ "**Statistic**")

  t2 <- trial |>
    dplyr::filter(trt == "Drug B") |>
    select(age, response, death) |>
    tbl_summary()

  tbl_stack_ex1 <- tbl_stack(
    tbls = list(t1, t2),
    group_header = c("Drug A", "Drug B"),
    quiet = TRUE
  )

  expect_silent(kbl_stack <- tbl_stack_ex1 |> as_kable())
  expect_snapshot(kbl_stack)
})

test_that("as_kable passes missing symbols correctly", {
  tbl <- my_tbl_summary |>
    modify_table_body(~ .x |> mutate(stat_0 = NA_character_)) |>
    modify_table_styling(stat_0, rows = !is.na(label), missing_symbol = "n / a")
  kbl <- tbl |> as_kable()

  expect_true(
    kbl[3:8] |>
      sapply(grepl, pattern = "n / a") |>
      all()
  )
})

test_that("as_kable passes table column alignment correctly", {
  expect_silent(kbl <- my_tbl_regression |> as_kable(return_calls = TRUE))

  # default alignment
  expect_true("c(\"l\", \"c\", \"c\", \"c\")" %in% as.character(kbl$kable))

  tbl <- my_tbl_regression |>
    modify_table_styling(columns = "estimate", align = "right")
  kbl <- tbl |> as_kable(return_calls = TRUE)

  # customized alignment
  expect_true("c(\"l\", \"r\", \"c\", \"c\")" %in% as.character(kbl$kable))
})

test_that("as_kable applies formatting functions correctly", {
  tbl <- glm(response ~ age + grade, trial, family = binomial(link = "logit")) |>
    tbl_regression(exponentiate = TRUE) |>
    modify_fmt_fun(
      p.value ~ function(x) style_pvalue(x, digits = 3),
      rows = variable == "grade"
    ) |>
    modify_fmt_fun(
      estimate ~ function(x) style_ratio(x, digits = 4, decimal.mark = ",")
    )
  kbl <- tbl |> as_kable()

  # formatted cells/columns
  expect_equal(
    kbl[3],
    "|Age                | 1,0191 | 1.00, 1.04 |    0.10     |"
  )
  expect_equal(
    kbl[7],
    "|III                | 1,0136 | 0.47, 2.16 |    0.972    |"
  )

  tbl2 <- tbl_uvregression(
    trial |> dplyr::select(response, age, grade),
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |>
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
  kbl2 <- tbl2 |> as_kable()

  # formatted cells/columns
  expect_equal(
    kbl2[3],
    "|Age                |  183.00  |  1.02  | 0.997, 1.04 |    0.10     |"
  )
  expect_equal(
    kbl2[4],
    "|Grade              | 193.0000 |        |             |             |"
  )
  expect_equal(
    kbl2[7],
    "|III                |          |  1.10  | 0.524, 2.29 |     0.8     |"
  )
})

test_that("as_kable passes column merging correctly", {
  tbl <- my_tbl_regression |>
    modify_column_merge(
      pattern = "{estimate} (pval {p.value})",
      rows = !is.na(estimate)
    )
  kbl <- tbl |> as_kable()

  expect_equal(
    kbl[3],
    "|Age                | 0.00 (pval >0.9) | -0.01, 0.01 |"
  )
})
