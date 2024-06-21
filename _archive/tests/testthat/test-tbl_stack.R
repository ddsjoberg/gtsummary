skip_on_cran()
gtsummary:::is_pkg_installed(c("cardx", "survival"), reference_pkg = "gtsummary")

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

  # correct number of rows after stacking
  expect_equal(
    nrow(tbl$table_body),
    nrow(t1_summary$table_body) + nrow(t2_summary$table_body)
  )

  # correct ordering of stacked tables
  expect_equal(tbl$tbls[[1]]$table_body, t1_summary$table_body)
  expect_equal(tbl$tbls[[2]]$table_body, t2_summary$table_body)

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
      estimate ~ function(x) style_ratio(x, digits = 2, decimal.mark = "..")
    )

  expect_silent(tbl <- tbl_stack(list(t1, t2)))

  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(estimate),
    c(NA, NA, "1,2148", NA, NA, "1..48")
  )
  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(p.value),
    c(NA, NA, "0.531", NA, NA, "0.2")
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
    c(rep(1, 5), rep(2, 5))
  )

  # footnotes stacked correctly
  expect_equal(
    tbl$table_styling$footnote,
    rbind(t1_regression$table_styling$footnote, t2_regression$table_styling$footnote)
  )

  # indentation values stacked correctly
  expect_equal(
    tbl$table_styling$indent |>
      dplyr::select(-rows),
    rbind(t1_regression$table_styling$indent, t2_regression$table_styling$indent) |>
      dplyr::select(-rows)
  )
})

test_that("tbl_stack works with tbl_merge objects", {
  expect_silent(
    tbl <- tbl_stack(list(row1, row2))
  )
})

test_that("tbl_stack works with tbl_summary objects", {
  yy <- tbl_summary(trial, by = response) |>
    add_p() |>
    add_q()
  tt <- tbl_summary(trial, by = trt) |>
    add_p() |>
    add_q()

  expect_error(
    zz <- tbl_stack(list(yy, tt)),
    NA
  )
  expect_silent(tbl <- zz |> as.data.frame())

  # no error if the list is named
  lst_summary <- list(yy, tt) |> set_names("one", "two")
  expect_snapshot(
    tbl_stack(lst_summary, group_header = c("Group 1", "Group 2")) |>
      as.data.frame()
  )

  # complex row-specific formatting is maintained
  tbl <-
    trial |>
    select(age, response, trt) |>
    tbl_summary(
      by = trt,
      missing = "no"
    ) |>
    add_difference()

  expect_equal(
    tbl_stack(
      list(
        tbl,
        tbl |> modify_fmt_fun(p.value ~ purrr::partial(style_sigfig, digits = 3))
      )
    ) |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(p.value),
    c("0.8", "0.6", "0.834", "0.637")
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
