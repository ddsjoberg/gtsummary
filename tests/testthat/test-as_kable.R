skip_on_cran()
skip_if_not(is_pkg_installed("knitr", reference_pkg = "gtsummary"))

my_tbl_summary <- trial |>
  select(trt, age, death) |>
  tbl_summary()
my_tbl_regression <- lm(marker ~ age, trial) |> tbl_regression()

test_that("as_kable works with standard use", {
  # return_calls argument does not produce warnings
  expect_silent(kbl <- my_tbl_summary |> as_kable(return_calls = TRUE))

  # include argument does not produce warnings
  expect_silent(my_tbl_summary |> as_kable(include = tibble))

  # correct elements are returned
  expect_equal(
    names(kbl),
    c("tibble", "fmt", "cols_merge", "tab_style_bold", "tab_style_italic",
      "fmt_missing", "cols_hide", "remove_line_breaks", "kable")
  )

  expect_silent(kbl <- my_tbl_summary |> as_kable())

  # correct number of rows
  expect_equal(length(kbl), 8)

  # test snapshot
  expect_snapshot(kbl)
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

test_that("as_kable removes line breaks from table", {
  tbl <- trial |>
    select(trt, age, death) |>
    tbl_summary(label = list(age = "Pt \nAge")) |>
    modify_header(label = "_Test \n Columns_")
  expect_silent(kbl <- tbl |> as_kable())

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

test_that("tbl_cross", {
  expect_error(tbl <- tbl_cross(trial, grade, trt) |> as_kable(format = "pipe"), NA)
  expect_warning(tbl_summary(trial) |> as_kable(), NA)
  expect_snapshot(tbl)
})

test_that("tbl_regression", {
  expect_error(tbl <- lm(marker ~ age, trial) |> tbl_regression() |> as_kable(format = "pipe"), NA)
  expect_warning(lm(marker ~ age, trial) |> tbl_regression() |> as_kable(), NA)
  expect_snapshot(tbl)

  expect_snapshot(
    with_gtsummary_theme(
      x = theme_gtsummary_journal("qjecon"),
      lm(age ~ marker + response, data = trial) |>
        tbl_regression() |>
        as_kable()
    )
  )
})

test_that("as_gt applies formatting functions correctly", {
  tbl <- glm(response ~ age + grade, trial, family = binomial(link = "logit")) |>
    tbl_regression(exponentiate = TRUE) |>
    modify_fmt_fun(
      p.value ~ function(x) style_pvalue(x, digits = 3),
      rows = variable == "grade"
    ) |>
    modify_fmt_fun(
      estimate ~ function(x) style_ratio(x, digits = 4, decimal.mark = ",")
    )
  gt_tbl <- tbl |> as_gt()

  # formatted cells
  expect_equal(
    gt_tbl$`_formats`[[12]]$func$default(gt_tbl$`_data`$p.value),
    c("0.096", NA, NA, "0.688", "0.972")
  )

  # formatted column
  expect_equal(
    gt_tbl$`_formats`[[13]]$func$default(gt_tbl$`_data`$estimate),
    c("1,0191", NA, NA, "0,8535", "1,0136")
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
  gt_tbl2 <- tbl2 |> as_gt()

  # formatted cell
  expect_equal(
    gt_tbl2$`_formats`[[22]]$func$default(gt_tbl2$`_data`$stat_n),
    c("183.0000", "193.0000", NA, NA, NA)
  )

  # formatted column
  expect_equal(
    gt_tbl2$`_data`$conf.low,
    c("0.997, 1.04", NA, NA, "0.446, 2.00", "0.524, 2.29")
  )
})

test_that("tbl_uvregression", {
  expect_error(tbl <- trial |> tbl_uvregression(method = lm, y = age) |> as_kable(format = "pipe"), NA)
  expect_warning(trial |> tbl_uvregression(method = lm, y = age) |> as_kable(), NA)
  expect_snapshot(tbl)
})

test_that("tbl_survfit", {
  skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))
  fit1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)

  expect_error(tbl <- tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months") |> as_kable(format = "pipe"), NA)
  expect_warning(tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months") |> as_kable(), NA)
  expect_snapshot(tbl)
})


test_that("tbl_merge/tbl_stack", {
  skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))

  t1 <-
    glm(response ~ trt + grade + age, trial, family = binomial) |>
    tbl_regression(exponentiate = TRUE)
  t2 <-
    survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) |>
    tbl_regression(exponentiate = TRUE)
  tbl_merge_ex1 <-
    tbl_merge(
      tbls = list(t1, t2),
      tab_spanner = c("**Tumor Response**", "**Time to Death**")
    )

  tbl_stack_ex1 <-
    tbl_stack(
      tbls = list(t1, t2),
      group_header = c("**Tumor Response**", "**Time to Death**")
    )

  expect_error(tbl <- tbl_merge_ex1 |> as_kable(format = "pipe"), NA)
  expect_warning(tbl_merge_ex1 |> as_kable(), NA)
  expect_snapshot(tbl)

  expect_error(tbl <- tbl_stack_ex1 |> as_kable(format = "pipe"), NA)
  expect_warning(tbl_stack_ex1 |> as_kable(), NA)
  expect_snapshot(tbl)
})

test_that("No errors replacing default arg values", {
  expect_error(
    trial |>
      tbl_summary(
        by = trt,
        include = c(age, grade),
        missing = "no"
      ) |>
      as_kable(col.names = NULL),
    NA
  )
})
