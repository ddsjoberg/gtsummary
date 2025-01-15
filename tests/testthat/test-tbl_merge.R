skip_on_cran()
skip_if_not(is_pkg_installed("survival", ref = "survival"))

# univariate regression models
t0 <-
  trial |>
  dplyr::select(response, trt, grade, age) |>
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
  )
# MVA logistic regression
t1 <-
  glm(response ~ trt + grade + age, trial, family = binomial) |>
  tbl_regression(exponentiate = TRUE)

# MVA cox regression
t2 <-
  survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) |>
  tbl_regression(
    exponentiate = TRUE
  )

# tbl_stack adjusted model
covars <- c("trt", "age")

# get model covariates adjusted by stage and grade
adj_mods <-
  map(
    covars, ~
      survival::coxph(
        as.formula(
          paste("survival::Surv(ttdeath, death) ~ grade + ", .x)
        ),
        trial
      ) |>
      tbl_regression(
        include = all_of(.x),
        exponentiate = TRUE
      )
  )

# now get stage and grade models adjusted for each other
adj_mods[["grade_mod"]] <-
  survival::coxph(
    as.formula(paste("survival::Surv(ttdeath, death) ~ grade")),
    trial
  ) |>
  tbl_regression(exponentiate = TRUE)

# stack all your adjusted models
t3 <- tbl_stack(adj_mods)

test_that("tbl_merge works with standard use", {
  # apply tbl_merge
  expect_silent(
    t4 <- tbl_merge(
      tbls = list(t0, t1, t2, t3),
      tab_spanner = c("UVA Tumor Response", "MVA Tumor Response", "MVA Time to Death", "TTD Adjusted for grade")
    )
  )

  # same number of rows after merge
  expect_true(nrow(t0$table_body) == nrow(t4$table_body))
  expect_true(nrow(t1$table_body) == nrow(t4$table_body))
  expect_true(nrow(t2$table_body) == nrow(t4$table_body))
  expect_true(nrow(t3$table_body) == nrow(t4$table_body))

  # correct spanning headers
  expect_equal(
    t4$table_styling$spanning_header |>
      dplyr::filter(column %in% t4$table_styling$header$column[!t4$table_styling$header$hide]) |>
      dplyr::pull(spanning_header) |>
      rev(),
    c(rep("UVA Tumor Response", 4),
      rep("MVA Tumor Response", 3),
      rep("MVA Time to Death", 3),
      rep("TTD Adjusted for grade", 3))
  )
})

test_that("tbl_merge orders columns correctly", {
  t4 <- tbl_merge(
    tbls = list(t0, t1, t2, t3),
    tab_spanner = c("UVA Tumor Response", "MVA Tumor Response", "MVA Time to Death", "TTD Adjusted for grade")
  )

  # table body is constructed in correct order
  expect_equal(t4$tbls[[1]]$table_body, t0$table_body)
  expect_equal(t4$tbls[[2]]$table_body, t1$table_body)
  expect_equal(t4$tbls[[3]]$table_body, t2$table_body)
  expect_equal(t4$tbls[[4]]$table_body, t3$table_body)
  expect_equal(
    t4 |> as_tibble(col_labels = FALSE),
    t4 |>
      as_tibble(col_labels = FALSE) |>
      select(label, ends_with("_1"), ends_with("_2"), ends_with("_3"), ends_with("_4"))
  )
})

test_that("tbl_merge works with no spanning header", {
  expect_silent(tbl <- tbl_merge(list(t0, t1, t2, t3), tab_spanner = FALSE))

  expect_true(
    tbl$table_styling$spanning_header$spanning_header |> is_empty()
  )
})

test_that("tbl_merge works with a single table", {
  t5 <- trial |>
    dplyr::select(age, grade, response) |>
    tbl_summary(missing = "no") |>
    add_n()

  expect_silent(tbl <- tbl_merge(list(t5)))

  # tbls has length 1
  expect_equal(length(tbl$tbls), 1)

  # table_body unchanged except for labels
  expect_equal(
    tbl |> as_tibble(col_labels = FALSE),
    t5 |>
      as_tibble(col_labels = FALSE) |>
      dplyr::rename(
        n_1 = n,
        stat_0_1 = stat_0,
      )
  )

  # correct spanning header
  expect_equal(
    tbl$table_styling$spanning_header$spanning_header,
    rep("**Table 1**", 3)
  )
})

test_that("tbl_merge works with more complex merge", {
  suppressMessages(theme_gtsummary_journal("jama"))

  t1 <- trial[c("age", "grade", "response")] |>
    tbl_summary(
      missing = "no",
      type = list(where(is.numeric) ~ "continuous2"),
      include = c(age, response)
    ) |>
    add_n() |>
    modify_header(stat_0 ~ "**Summary Statistics**")


  t2 <- tbl_uvregression(
    trial[c("ttdeath", "death", "age", "grade", "response")],
    method = survival::coxph,
    y = survival::Surv(ttdeath, death),
    exponentiate = TRUE,
    hide_n = TRUE,
    include = c(age, response)
  )

  expect_snapshot(
    tbl_merge(tbls = list(t1, t2)) |>
      modify_spanning_header(everything() ~ NA) |>
      as.data.frame()
  )

  reset_gtsummary_theme()
})

test_that("tbl_merge returns expected message when nonunique columns present", {
  expect_message(
    tbl_merge(list(tbl_stack(list(t1, t1)))),
    "do not uniquely identify rows for each table.*merge may fail or result in a malformed table"
  )
})

test_that("tbl_merge throws expected errors", {
  # input must be a list
  expect_snapshot(
    tbl_merge(t1),
    error = TRUE
  )

  # tables must be gtsummary tables
  expect_snapshot(
    tbl_merge(list(mtcars)),
    error = TRUE
  )

  # correct spanning header type
  expect_snapshot(
    tbl_merge(tbls = list(t0, t1), tab_spanner = 1),
    error = TRUE
  )

  # correct spanning header length
  expect_snapshot(
    tbl_merge(tbls = list(t0, t1), tab_spanner = c("Table")),
    error = TRUE
  )

  tbl <-
    .create_gtsummary_object(table_body = head(mtcars)) |>
    modify_column_unhide(everything())

  # gtsummary tables must be correctly formatted
  expect_snapshot(
    tbl_merge(list(tbl, tbl)),
    error = TRUE
  )
})

test_that("tbl_merge() merges mixed-type from .$table_styling$header$modify_* columns", {
  skip_if_not(is_pkg_installed(c("survey", "cardx", "broom", "withr")))
  withr::local_seed(123)

  num_rows <- 10
  toy_data <-
    data.frame(
      item1 = sample(c("Never", "Sometimes", "All the time"), num_rows, replace = TRUE),
      gender = sample(c("Male", "Female"), num_rows, replace = TRUE),
      weight = rnorm(num_rows, mean = 0, sd = 5) |> abs()
    )
  # Create a survey object
  toy_dataw <- survey::svydesign(data = toy_data, weights = ~weight, ids = ~1)
  t1 <- tbl_summary(toy_data, by = gender)
  t2 <- tbl_svysummary(toy_dataw, by = gender)
  t3 <- tbl_summary(toy_data, include = item1)
  t4 <- tbl_svysummary(toy_dataw, include = item1)

  # confirm these columns are a mix of integers and non-integers
  expect_true(t1$table_styling$header$modify_stat_N |> is("integer"))
  expect_true(t3$table_styling$header$modify_stat_N |> is("integer"))
  expect_false(t2$table_styling$header$modify_stat_N |> is("integer"))
  expect_false(t4$table_styling$header$modify_stat_N |> is("integer"))

  # no errors when mixing these variables during the merge
  expect_silent(tbl_merge(tbls = list(t1, t2)))
  expect_silent(tbl_merge(tbls = list(t3, t4)))
})

test_that("tbl_merge(merge_vars)", {
  # no errors when merging
  expect_silent(
    tbl <-
      as_gtsummary(mtcars[duplicated(mtcars$mpg),c("mpg", "cyl")]) %>%
      list(., .) |>
      tbl_merge(merge_vars = "mpg")
  )

  # check the headers are correct
  expect_equal(
    tbl$table_styling$header,
    dplyr::tribble(
      ~column, ~hide,   ~align, ~interpret_label, ~label,
      "mpg",   FALSE, "center",         "gt::md",  "mpg",
      "cyl_1", FALSE, "center",         "gt::md",  "cyl",
      "cyl_2", FALSE, "center",         "gt::md",  "cyl"
    )
  )

  # check the spanning headers are correct
  expect_equal(
    tbl$table_styling$spanning_header,
    dplyr::tribble(
      ~level, ~column, ~spanning_header, ~text_interpret, ~remove,
      1L,     "cyl_2",    "**Table 2**",        "gt::md",   FALSE,
      1L,     "cyl_1",    "**Table 1**",        "gt::md",   FALSE
    )
  )
})

test_that("tbl_merge() works with tbl_hierarchical()", {
  # check that AE table can be merged
  expect_silent(
    tbl <-
      cards::ADAE |>
      dplyr::filter(
        AESOC %in% unique(cards::ADAE$AESOC)[1:5],
        AETERM %in% unique(cards::ADAE$AETERM)[1:5]
      ) |>
      tbl_hierarchical(
        variables = c(AESOC, AETERM),
        denominator = cards::ADSL,
        id = USUBJID,
        digits = everything() ~ list(p = 1),
        overall_row = TRUE,
        label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
      )
  )
  expect_equal(
    list(tbl, tbl) |>
      tbl_merge() |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull("label"),
    tbl |>
    as.data.frame(col_labels = FALSE) |>
      dplyr::pull("label")
  )
})
