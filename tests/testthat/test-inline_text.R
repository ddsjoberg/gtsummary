skip_on_cran()
skip_if_not(is_pkg_installed("survival", reference_pkg = "gtsummary"))

# inline_text.tbl_summary tests ------------------------------------------------
test_inline1 <- trial |> tbl_summary()
test_inline2 <- trial |> tbl_summary(by = trt)
test_inline2b <- trial |>
  tbl_summary(by = trt) |>
  add_overall() |>
  add_p()

test_inline3 <-
  trial |>
  tbl_summary(by = trt, include = age, missing = "no") |>
  add_difference()

test_that("inline_text.tbl_summary", {
  expect_equal(
    trial |>
      select(age) |>
      tbl_summary() |>
      inline_text(variable = "age", column = "stat_0"),
    "47 (38, 57)"
  )

  expect_equal(
    inline_text(test_inline1, variable = "age"),
    "47 (38, 57)"
  )

  expect_equal(
    inline_text(test_inline1, variable = "stage", level = "T1"),
    "53 (27%)"
  )

  expect_equal(
    inline_text(test_inline1, variable = "stage", level = "T1", pattern = "{p}%"),
    "27%"
  )

  expect_equal(
    inline_text(test_inline1, variable = "age", pattern = "The median is {median}"),
    "The median is 47"
  )

  expect_equal(
    inline_text(test_inline2, variable = "age", column = "Drug B"),
    "48 (39, 56)"
  )

  expect_equal(
    inline_text(test_inline3, pattern = "{estimate} (95% CI {conf.low}, {conf.high})", variable = "age"),
    "-0.44 (95% CI -4.6, 3.7)"
  )

  expect_equal(
    inline_text(test_inline2, variable = "stage", level = "T1", column = "Drug B"),
    "25 (25%)"
  )

  expect_equal(
    inline_text(test_inline2b, variable = "stage", column = "p.value"),
    "p=0.9"
  )

  # add_overall checks
  expect_equal(
    inline_text(test_inline2b, variable = "age", column = "stat_1", pattern = "{median}"),
    "46"
  )
  expect_equal(
    inline_text(test_inline2b, variable = "age", column = "stat_0", pattern = "{median}"),
    "47"
  )
  expect_equal(
    inline_text(test_inline2b, variable = "stage", level = "T1", column = "stat_0", pattern = "{n}"),
    "53"
  )
  expect_equal(
    inline_text(test_inline2b, variable = "stage", level = "T1", column = "stat_1", pattern = "{n}"),
    "28"
  )

  # can still select first level after remove row type
  expect_equal(
    trial %>%
      select(grade, trt) %>%
      tbl_summary(by = trt, missing = "no") %>%
      remove_row_type() %>%
      inline_text(variable = grade, level = "I", column = "stat_1"),
    "35 (36%)"
  )

  # ensure inline_text(column) argument works with factor levels
  expect_equal(
    trial |>
      dplyr::mutate(trt = factor(trt)) |>
      tbl_summary(by = trt, include = age) |>
      inline_text(variable = "age", column = "Drug A", pattern = "{median}"),
    "46"
  )
})


test_that("inline_text.tbl_summary() messaging", {
  # dplyr made a slight change to their messaging in v1.1.4 and that is captured here
  # just adding this because some validated envs don't have the most recent version
  skip_if(packageVersion("dplyr") < package_version("1.1.4"))

  expect_snapshot(
    error = TRUE,
    inline_text(test_inline2, variable = "age", column = "Pla5cebo")
  )

  expect_snapshot(
    error = TRUE,
    inline_text(test_inline2, variable = "stage", level = "Tsdfgsdfg1", column = "Drug B")
  )

  expect_snapshot(
    error = TRUE,
    inline_text(test_inline2, variable = "st55age", level = "T1", column = "Drug B")
  )
})


# inline_text.regression tests -------------------------------------------------
test_inline3 <- lm(marker ~ age + stage, trial) |> tbl_regression()
test_inline4 <- glm(response ~ trt + age + stage, trial, family = binomial) |>
  tbl_regression(exponentiate = TRUE)

test_that("inline_text.tbl_regression()", {
  expect_equal(
    inline_text(test_inline3, variable = "age"),
    "0.00 (95% CI -0.01, 0.01; p=0.9)"
  )

  expect_equal(
    inline_text(test_inline4, variable = "stage", level = "T2"),
    "0.58 (95% CI 0.24, 1.36; p=0.2)"
  )
})


test_that("inline_text.tbl_regression() messaging", {
  # dplyr made a slight change to their messaging in v1.1.4 and that is captured here
  # just adding this because some validated envs don't have the most recent version
  skip_if(packageVersion("dplyr") < package_version("1.1.4"))

  expect_snapshot(
    error = TRUE,
    inline_text(test_inline3, variable = "stage", level = "Tsdfgsdfg1")
  )

  expect_snapshot(
    error = TRUE,
    inline_text(test_inline3, variable = "st55age")
  )
})

# inline_text.tbl_survfit() tests  ---------------------------------------------
fit1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)
fit2 <- survival::survfit(survival::Surv(ttdeath, death) ~ 1, trial)

tbl1 <-
  tbl_survfit(
    fit1,
    times = c(12, 24),
    label_header = "**{time} Month**"
  ) %>%
  add_p()

tbl2 <-
  tbl_survfit(
    fit2,
    probs = 0.5
  )

test_that("inline_text.tbl_survfit()", {
  expect_equal(
    inline_text(tbl1, time = 24, level = "Drug A"),
    "47% (38%, 58%)"
  )
  expect_equal(
    inline_text(tbl1, time = 24, level = "Drug A", pattern = "{estimate}"),
    "47%"
  )

  expect_silent(
    tbl1_pvalue <- inline_text(tbl1, column = p.value)
  )
  expect_equal(tbl1_pvalue, "p=0.2")

  expect_silent(
    tbl1_pattern <-
      inline_text(
        tbl1,
        time = 24,
        level = "Drug A",
        pattern = "{estimate}",
        estimate_fun = ~ style_percent(., digits = 3, symbol = TRUE)
      )
  )
  expect_equal(tbl1_pattern, "46.939%")

  expect_equal(
    inline_text(tbl2, prob = 0.5),
    "22 (21, —)"
  )
})

test_that("inline_text.tbl_survfit() messaging", {
  expect_snapshot(
    error = TRUE,
    inline_text(tbl1, time = 24, column = "stat_2", level = "Drug A")
  )

  expect_snapshot(
    error = TRUE,
    inline_text(tbl1, time = 10000, level = "Drug A")
  )

  expect_snapshot(
    error = TRUE,
    inline_text(tbl2, prob = 0.2, level = "Drug A")
  )
})

# inline_text.tbl_continuous() tests ------------------------------------------------
test_that("inline_text.tbl_continuous()", {
  expect_equal(
    trial |>
      tbl_continuous(
        variable = age,
        by = trt,
        include = grade
      ) |>
      inline_text(variable = grade, level = "I", column = "stat_1"),
    "46 (36, 60)"
  )
})

# inline_text.tbl_cross() tests ------------------------------------------------
test_that("inline_text.tbl_cross()", {
  tbl_cross <-
    tbl_cross(trial, row = trt, col = response) %>%
    add_p()

  expect_equal(
    inline_text(tbl_cross, row_level = "Drug A", col_level = "1"),
    "28"
  )
  expect_equal(
    inline_text(tbl_cross, row_level = "Total", col_level = "1"),
    "61"
  )
  expect_equal(
    inline_text(tbl_cross, col_level = "p.value"),
    "p=0.7"
  )
})



test_that("inline_text.tbl_cross() messaging", {
  tbl_cross <-
    tbl_cross(trial, row = trt, col = response) %>%
    add_p()

  expect_snapshot(
    error = TRUE,
    inline_text(tbl_cross, row_level = "Drug A")
  )

  expect_snapshot(
    error = TRUE,
    inline_text(tbl_cross)
  )
})

# skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))
#
# # inline_text.tbl_svysummary tests --------------
# test_inline1 <- trial %>%
#   survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
#   tbl_svysummary()
# test_inline2 <- trial %>%
#   survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
#   tbl_svysummary(by = trt)
# test_inline2b <- trial %>%
#   survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
#   tbl_svysummary(by = trt) %>%
#   add_p()

# test_that("inline_text.tbl_svysummary: no by", {
#   expect_error(
#     inline_text(test_inline1, variable = "age"),
#     NA
#   )
#   expect_error(
#     inline_text(test_inline1, variable = "stage", level = "T1"),
#     NA
#   )
#
#   expect_equal(
#     inline_text(test_inline1, variable = "stage", level = "T1", pattern = "{p}%"),
#     "27%"
#   )
#   expect_equal(
#     inline_text(test_inline1, variable = "age", pattern = "The median is {median}"),
#     "The median is 47"
#   )
# })

# test_that("inline_text.tbl_svysummary: with by", {
#   expect_error(
#     inline_text(test_inline2, variable = "age", column = "Drug B"),
#     NA
#   )
#   expect_error(
#     inline_text(test_inline2, variable = "stage", level = "T1", column = "Drug B"),
#     NA
#   )
#   expect_error(
#     inline_text(test_inline2b, variable = "stage", column = "p.value"),
#     NA
#   )
# })


# test_that("inline_text.tbl_svysummary: with by -  expect errors", {
#   expect_error(
#     inline_text(test_inline2, variable = "age", column = "Pla5cebo"),
#     NULL
#   )
#   expect_error(
#     inline_text(test_inline2, variable = "stage", level = "Tsdfgsdfg1", column = "Drug B"),
#     NULL
#   )
#
#   expect_error(
#     inline_text(test_inline2, variable = "st55age", level = "T1", column = "Drug B"),
#     NULL
#   )
# })

# test_that("inline_text.tbl_svysummary: no errors with empty string selection", {
#   skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))
#   expect_error(
#     trial %>%
#       select(grade) %>%
#       mutate(grade = ifelse(grade == "I", "", as.character(grade))) %>%
#       survey::svydesign(data = ., ids = ~1, weights = ~1) %>%
#       tbl_svysummary() %>%
#       inline_text(variable = grade, level = "III"),
#     NA
#   )
# })

test_that("check for messaging about duplicate variables", {
  t1 <- lm(age ~ marker, trial) %>% tbl_regression()

  expect_snapshot(
    tbl_stack(list(t1, t1)) %>%
      inline_text(variable = marker, column = estimate)
  )
})

test_that("inline_text.gtsummary() messaging", {
  tbl <-
    .create_gtsummary_object(table_body = head(mtcars)) |>
    modify_column_unhide(everything())

  expect_error(
    inline_text(tbl, variable = "mpg"),
    'data frame does not have the required'
  )

  tbl <-
    head(mtcars) %>%
    dplyr::mutate(variable = "one") %>%
    .create_gtsummary_object() |>
    modify_column_unhide(everything())

  expect_error(
    inline_text(tbl, variable = "one", column = "mpg", level = "21"),
    "The gtsummary table does not have the required "
  )
})

# testing mixed class inline text with patterns
test_that("works with mixed class stacking", {
  expect_equal(
    trial %>%
      mutate(
        var_duration = Sys.Date() - Sys.Date()
      ) %>%
      tbl_summary(
        include = c(age, var_duration),
        type = ~"continuous"
      ) %>%
      inline_text(variable = var_duration, pattern = "{median}"),
    "0.0000 days"
  )
})
