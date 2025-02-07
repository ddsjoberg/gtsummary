skip_on_cran()
skip_if_not(is_pkg_installed(c("cardx", "survival")))

t1_summary <- trial |>
  dplyr::filter(trt == "Drug A") |>
  select(age, response, death) |>
  tbl_summary() |>
  modify_header(stat_0 ~ "**Statistic**")

t2_summary <- trial |>
  dplyr::filter(trt == "Drug B") |>
  select(age, response, death) |>
  tbl_summary() |>
  modify_header(stat_0 ~ "**Statistic**")

t1_regression <- glm(response ~ trt, trial, family = binomial) |>
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (unadjusted)")
  )

t2_regression <- glm(response ~ trt + grade + stage + marker, trial, family = binomial) |>
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (adjusted)")
  )

t1_survival <- survival::coxph(survival::Surv(ttdeath, death) ~ trt, trial) |>
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (unadjusted)")
  )

t2_survival <- survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + stage + marker, trial) |>
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (adjusted)")
  )

merge_row1 <- tbl_merge(list(t1_regression, t1_survival), tab_spanner = c("Tumor Response", "Death"))
merge_row2 <- tbl_merge(list(t2_regression, t2_survival))

test_that("tbl_stack works with standard use", {
  expect_silent(
    tbl <- tbl_stack(list(t1_summary, t2_summary), group_header = c("Drug A", "Drug B"))
  )

  # no error if list input is named
  expect_silent(
    tbl <- tbl_stack(list(t1 = t1_summary, t2 = t2_summary), group_header = c("Drug A", "Drug B"))
  )

  # correct number of rows after stacking
  expect_equal(
    nrow(tbl$table_body),
    nrow(t1_summary$table_body) + nrow(t2_summary$table_body)
  )

  # correct ordering of stacked tables
  expect_equal(tbl$tbls$t1$table_body, t1_summary$table_body)
  expect_equal(tbl$tbls$t2$table_body, t2_summary$table_body)

  # correct group headers
  expect_equal(
    tbl$table_body$groupname_col |> unique(),
    c("Drug A", "Drug B")
  )
  expect_equal(
    tbl$table_body$tbl_id1,
    c(rep(1, 5), rep(2, 5))
  )

  # footnotes stacked correctly
  expect_equal(
    tbl$table_styling$footnote,
    rbind(t1_summary$table_styling$footnote, t2_summary$table_styling$footnote)
  )

  # indentation values stacked correctly
  expect_equal(
    tbl$table_styling$indent |>
      dplyr::select(-rows),
    rbind(t1_summary$table_styling$indent, t2_summary$table_styling$indent) |>
      dplyr::select(-rows)
  )
})

test_that("tbl_stack(quiet) works", {
  t1 <- t1_summary |>
    modify_header(stat_0 ~ "**Statistic**")

  t2 <- t2_summary |>
    modify_header(stat_0 ~ "Replaced label")

  expect_silent(tbl <- tbl_stack(list(t1, t2), quiet = TRUE))
  expect_equal(
    tbl |> as_tibble() |> names(),
    c("**Characteristic**", "**Statistic**")
  )
})

test_that("tbl_stack works with no group header", {
  expect_silent(tbl <- tbl_stack(list(t1_summary, t2_summary)))

  expect_true(!"groupname_col" %in% names(tbl$table_body))
  expect_equal(
    tbl$table_body$tbl_id1,
    c(rep(1, 5), rep(2, 5))
  )
})

test_that("tbl_stack works with a single table", {
  expect_silent(tbl <- tbl_stack(list(t1_summary)))

  # tbls has length 1
  expect_equal(length(tbl$tbls), 1)

  # table_body unchanged except for added id column
  expect_equal(
    tbl$table_body,
    t1_summary$table_body |>
      dplyr::mutate(tbl_id1 = 1L, .before = variable)
  )
})

test_that("tbl_stack works with tbl_regression objects", {
  expect_silent(
    tbl <- tbl_stack(list(t1_regression, t2_regression), group_header = c("Group 1", "Group 2"))
  )

  # correct number of rows after stacking
  expect_equal(
    nrow(tbl$table_body),
    nrow(t1_regression$table_body) + nrow(t2_regression$table_body)
  )

  # correct ordering of stacked tables
  expect_equal(tbl$tbls[[1]]$table_body, t1_regression$table_body)
  expect_equal(tbl$tbls[[2]]$table_body, t2_regression$table_body)

  # correct group headers
  expect_equal(
    tbl$table_body$groupname_col |> unique(),
    c("Group 1", "Group 2")
  )
  expect_equal(
    tbl$table_body$tbl_id1,
    c(rep(1, 3), rep(2, 3))
  )

  # footnote abbreviations stacked correctly
  expect_equal(
    tbl$table_styling$footnote_abbrev,
    rbind(t1_regression$table_styling$footnote_abbrev, t2_regression$table_styling$footnote_abbrev)
  )
})

test_that("tbl_stack works with tbl_merge objects", {
  expect_silent(
    tbl <- tbl_stack(list(merge_row1, merge_row2), quiet = TRUE)
  )

  # correct number of rows after stacking
  expect_equal(
    nrow(tbl$table_body),
    nrow(merge_row1$table_body) + nrow(merge_row2$table_body)
  )

  # correct ordering of stacked tables
  expect_equal(tbl$tbls[[1]]$table_body, merge_row1$table_body)
  expect_equal(tbl$tbls[[2]]$table_body, merge_row2$table_body)
})

test_that("tbl_stack works with tbl_merge objects", {
  expect_silent(
    tbl <- tbl_stack(list(merge_row1, merge_row2), quiet = TRUE)
  )

  # correct number of rows after stacking
  expect_equal(
    nrow(tbl$table_body),
    nrow(merge_row1$table_body) + nrow(merge_row2$table_body)
  )

  # correct ordering of stacked tables
  expect_equal(tbl$tbls[[1]]$table_body, merge_row1$table_body)
  expect_equal(tbl$tbls[[2]]$table_body, merge_row2$table_body)
})

test_that("tbl_stack works with formatting functions", {
  t1 <- t1_regression |>
    modify_fmt_fun(
      p.value ~ function(x) style_pvalue(x, digits = 3),
      rows = variable == "trt"
    ) |>
    modify_fmt_fun(
      estimate ~ function(x) style_ratio(x, digits = 4, decimal.mark = ",")
    )

  t2 <- t2_regression |>
    modify_fmt_fun(
      estimate ~ function(x) style_ratio(x, digits = 2, decimal.mark = "*")
    )

  expect_silent(tbl <- tbl_stack(list(t1, t2)))

  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(estimate),
    c(NA, NA, "1,2148", NA, NA, "1*48")
  )
  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(p.value),
    c(NA, NA, "0.531", NA, NA, "0.2")
  )
})

test_that("tbl_stack works with missing symbols", {
  t1 <- t1_summary |>
    modify_table_body(
      ~ .x |> mutate(stat_0 = NA_character_)
    ) |>
    modify_missing_symbol(stat_0, rows = !is.na(label), symbol = "n / a")

  t2 <- t2_summary |>
    modify_table_body(
      ~ .x |> mutate(stat_0 = NA_character_)
    )

  expect_silent(tbl <- tbl_stack(list(t1, t2)))
  expect_equal(
    tbl$table_styling$fmt_missing |>
      dplyr::select(-rows),
    rbind(t1$table_styling$fmt_missing, t2$table_styling$fmt_missing) |>
      dplyr::select(-rows)
  )

  # missing symbol in first table only
  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE, fmt_missing = TRUE) |>
      dplyr::pull(stat_0),
    c(rep("n / a", 5), rep(NA, 5))
  )

  t2 <- t2 |>
    modify_missing_symbol(stat_0, rows = !is.na(label), symbol = "miss")

  expect_silent(tbl <- tbl_stack(list(t1, t2)))

  # different missing symbol in each stacked table
  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE, fmt_missing = TRUE) |>
      dplyr::pull(stat_0),
    c(rep("n / a", 5), rep("miss", 5))
  )
  expect_equal(
    tbl$table_styling$fmt_missing |>
      dplyr::select(-rows),
    rbind(t2$table_styling$fmt_missing, t1$table_styling$fmt_missing) |>
      dplyr::select(-rows)
  )
})

test_that("tbl_stack returns expected message when unique column names are present", {
  t1 <- t1_summary |>
    modify_header(stat_0 ~ "**Statistic**")

  t2 <- t2_summary |>
    modify_header(stat_0 ~ "Replaced label")

  expect_message(
    tbl_stack(list(t1, t2)),
    "Column headers among stacked tables differ"
  )
})

test_that("tbl_stack() can stack tbl that have been previosly stacked", {
  t1 <- trial |> tbl_summary(include = age, missing = "no")
  t2 <- tbl_stack(list(t1, t1))

  expect_silent(t3 <- tbl_stack(list(t1, t2)))

  t4 <- glm(response ~ trt + grade + age, trial, family = binomial) %>%
    tbl_regression(exponentiate = TRUE)
  t5 <- survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
    tbl_regression(exponentiate = TRUE)

  expect_error(t6 <- tbl_stack(tbls = list(t4, t5)), NA)
})

test_that("tbl_stack throws expected errors", {
  # input must be a list
  expect_snapshot(
    tbl_stack(t1_summary, t2_summary),
    error = TRUE
  )

  # must pass acceptable objects
  expect_snapshot(
    tbl_stack(list(mtcars)),
    error = TRUE
  )

  # correct group header length
  expect_snapshot(
    tbl_stack(tbls = list(t1_summary, t2_summary), group_header = c("Table")),
    error = TRUE
  )
})
