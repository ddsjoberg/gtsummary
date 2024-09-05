skip_on_cran()
skip_if_not(is_pkg_installed(c("broom", "lme4", "broom.helpers"), reference_pkg = "cardx"))

test_that("add_p.tbl_summary() snapshots of common outputs", {
  expect_snapshot(
    tbl_summary(trial, by = grade) |>
      add_p() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )

  expect_snapshot(
    tbl_summary(mtcars, by = am) |>
      add_p() |>
      as.data.frame()
  )

  expect_snapshot(
    trial |>
      tbl_summary(by = trt) |>
      add_p() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )
})

test_that("add_p.tbl_summary() error messaging with bad inputs", {
  # no by arg in tbl_summary
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial[c("trt", "age")]) |>
      add_p()
  )

  # bad test argument values
  expect_error(
    tbl_summary(trial[c("trt", "age")], by = trt) |>
      add_p(test = mtcars)
  )
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial[c("trt", "age")], by = trt) |>
      add_p(test = list(age = \(...) mtcars))
  )
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial[c("trt", "age")], by = trt) |>
      add_p(test = list(age = \(...) letters))
  )

  expect_error(
    tbl_summary(trial[c("trt", "age")], by = trt) |>
      add_p(pvalue_fun = mtcars)
  )

  expect_error(
    tbl_summary(trial[c("trt", "age")], by = trt) |>
      add_p(group = \(x) round(x))
  )
  expect_error(
    tbl_summary(trial[c("trt", "age")], by = trt) |>
      add_p(group = c("trt", "age"))
  )
})

test_that("add_p.tbl_summary() & lme4", {
  skip_if_not(is_pkg_installed("lme4", reference_pkg = "cardx"))

  # errors with expected use
  expect_snapshot(
    tbl_summary(trial, by = trt) |>
      add_p(test = everything() ~ "lme4", group = response) |>
      modify_column_hide(all_stat_cols()) |>
      as.data.frame()
  )

  # we see appropriate messaging when using incorrectly (no group variable)
  expect_snapshot(
    tbl_summary(trial, by = trt) |>
      add_p(test = everything() ~ "lme4") |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )
})

test_that("add_p.tbl_summary() creates output without error/warning for continuous2", {
  expect_snapshot(
    tbl_summary(trial, by = grade, include = c(age, marker, response), type = all_continuous() ~ "continuous2") |>
      add_p() |>
      as.data.frame(col_labels = FALSE) |>
      select(-all_stat_cols())
  )
})

test_that("add_p() creates errors with bad args", {
  expect_error(
    tbl_summary(mtcars, by = am) |>
      add_p(pvalue_fun = mtcars),
    NULL
  )

  expect_error(
    tbl_summary(trial, by = grade, include = -response) |>
      add_p(group = response),
    NULL
  )
})

test_that("add_p.tbl_summary() works well", {
  expect_snapshot(
    tbl_summary(mtcars, by = am) |>
      add_p(
        test = list(
          mpg = "t.test",
          hp = "oneway.test",
          cyl = "chisq.test.no.correct",
          carb = "mood.test"
        )
      ) |>
      as.data.frame()
  )

  expect_snapshot(
    tbl_summary(mtcars, by = am, include = c(mpg, disp)) |>
      add_p(
        test = list(
          mpg = t.test,
          disp = oneway.test
        )
      ) |>
      as.data.frame()
  )
})


test_that("add_p with custom p-value function", {
  withr::local_package("broom")
  my_mcnemar <- function(data, variable, by, ...) {
    stats::mcnemar.test(data[[variable]], data[[by]]) |> tidy()
  }

  expect_error(
    tbl <-
      trial[c("response", "trt")] |>
      tbl_summary(by = trt) |>
      add_p(test = response ~ "my_mcnemar"),
    NA
  )
  expect_equal(
    as.data.frame(tbl),
    trial[c("response", "trt")] |>
      tbl_summary(by = trt) |>
      add_p(test = response ~ my_mcnemar) |>
      as.data.frame()
  )


  expect_equal(
    tbl$cards$add_p$response$p.value,
    stats::mcnemar.test(trial[["response"]], trial[["trt"]])$p.value
  )
})

test_that("Wilcoxon and Kruskal-Wallis p-values match ", {
  t1 <- trial[c("trt", "age", "marker")] |>
    tbl_summary(by = trt) |>
    add_p(test = all_continuous() ~ wilcox.test)
  t2 <- trial[c("trt", "age", "marker")] |>
    tbl_summary(by = trt) |>
    add_p(test = all_continuous() ~ kruskal.test)
  expect_true(
    all(
      (t1$cards$add_p |> dplyr::bind_rows() |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist()) -
        (t2$cards$add_p |> dplyr::bind_rows() |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist()) < 0.001
    )
  )
})




test_that("p-values are replicated within tbl_summary()", {
  tbl_test.args <-
    trial |>
    dplyr::select(trt,
      var_t.test = age,
      var_t.test_dots = age,
      var_kruskal.test = age,
      var_wilcox.test = age,
      var_wilcox.test_dots = age,
      var_oneway.test = age,
      var_chisq.test = response,
      var_chisq.test_dots = response,
      var_chisq.test.no.correct = response,
      var_fisher.test = response,
      var_fisher.test_dots = response,
      var_mcnemar.test = response,
      var_mcnemar.test_dots = response,
    ) |>
    tbl_summary(by = trt, missing = "no") |>
    add_p(
      test = list(
        contains("t.test") ~ t.test,
        contains("kruskal.test") ~ kruskal.test,
        contains("wilcox.test") ~ wilcox.test,
        contains("oneway.test") ~ oneway.test,
        contains("chisq.test") ~ chisq.test,
        contains("chisq.test.no.correct") ~ "chisq.test.no.correct",
        contains("fisher.test") ~ fisher.test,
        contains("mcnemar.test") ~ "mcnemar.test.wide"
      ),
      test.args = list(
        var_t.test_dots = list(var.equal = TRUE),
        var_wilcox.test_dots = list(correct = FALSE),
        var_chisq.test_dots = list(correct = FALSE),
        var_fisher.test_dots = list(alternative = "greater"),
        var_mcnemar.test_dots = list(correct = FALSE)
      )
    )

  expect_equal(
    tbl_test.args$cards$add_p[["var_t.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    t.test(age ~ as.factor(trt), data = trial)$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_t.test_dots"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    t.test(age ~ as.factor(trt), data = trial, var.equal = TRUE)$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_kruskal.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    kruskal.test(trial$age, as.factor(trial$trt))$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_wilcox.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    wilcox.test(age ~ trt, data = trial)$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_wilcox.test_dots"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    wilcox.test(age ~ trt, data = trial, correct = FALSE)$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_oneway.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    stats::oneway.test(age ~ as.factor(trt), data = trial)$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_chisq.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    stats::chisq.test(x = trial[["response"]], y = as.factor(trial[["trt"]]))$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_chisq.test_dots"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    stats::chisq.test(x = trial[["response"]], y = as.factor(trial[["trt"]]), correct = FALSE)$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_chisq.test.no.correct"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    stats::chisq.test(x = trial[["response"]], y = as.factor(trial[["trt"]]), correct = FALSE)$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_fisher.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    fisher.test(trial[["response"]], as.factor(trial[["trt"]]))$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_fisher.test_dots"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    fisher.test(trial[["response"]], as.factor(trial[["trt"]]), alternative = "greater")$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_mcnemar.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    mcnemar.test(trial[["response"]], as.factor(trial[["trt"]]))$p.value
  )

  expect_equal(
    tbl_test.args$cards$add_p[["var_mcnemar.test_dots"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    mcnemar.test(trial[["response"]], as.factor(trial[["trt"]]), correct = FALSE)$p.value
  )
})

test_that("p-values are replicated within tbl_summary() with groups", {
  trial_group <-
    trial |>
    dplyr::group_by(trt) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::ungroup()

  trial_group_wide <-
    trial_group |>
    dplyr::filter(trt == "Drug A") |>
    dplyr::full_join(
      trial_group |>
        dplyr::filter(trt == "Drug B"),
      by = "id"
    )

  expect_message(
    trial_group |>
      tbl_summary(include = age, by = trt) |>
      add_p(test = age ~ "paired.t.test", include = age)
  )

  expect_message(
    trial_group |>
      dplyr::filter(dplyr::row_number() != 1L) |>
      tbl_summary(include = c(marker, age), by = trt) |>
      add_p(
        test = list(age = "paired.t.test", marker = "paired.wilcox.test"),
        group = id
      )
  )

  tbl_groups <-
    trial_group |>
    select(
      trt, id,
      grade_lme4 = grade,
      grade_mcnemar.test = grade,
      response_mcnemar.test = response,
      response_mcnemar.test_dots = response,
      age_paired.t.test = age,
      age_paired.t.test_dots = age,
      age_paired.wilcox.test = age,
      age_paired.wilcox.test_dots = age
    ) |>
    tbl_summary(by = trt, missing = "no", include = -id) |>
    add_p(
      test = list(
        contains("paired.t.test") ~ "paired.t.test",
        contains("mcnemar.test") ~ "mcnemar.test",
        contains("paired.wilcox.test") ~ "paired.wilcox.test"
      ),
      test.args = list(
        age_paired.t.test_dots ~ list(mu = 1),
        response_mcnemar.test_dots ~ list(correct = FALSE),
        age_paired.wilcox.test_dots ~ list(mu = 1)
      ),
      group = "id"
    )

  expect_equal(
    tbl_groups$cards$add_p[["response_mcnemar.test_dots"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    mcnemar.test(trial_group_wide[["response.x"]], trial_group_wide[["response.y"]],
      correct = FALSE
    )$p.value
  )

  expect_equal(
    tbl_groups$cards$add_p[["response_mcnemar.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    mcnemar.test(trial_group_wide[["response.x"]], trial_group_wide[["response.y"]])$p.value
  )

  expect_equal(
    tbl_groups$cards$add_p[["grade_mcnemar.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    mcnemar.test(trial_group_wide[["grade.x"]], trial_group_wide[["grade.y"]])$p.value
  )

  expect_equal(
    tbl_groups$cards$add_p[["age_paired.t.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    t.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE)$p.value
  )

  expect_equal(
    tbl_groups$cards$add_p[["age_paired.t.test_dots"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    t.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE, mu = 1)$p.value
  )

  expect_equal(
    tbl_groups$cards$add_p[["age_paired.wilcox.test"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    wilcox.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE)$p.value
  )

  expect_equal(
    tbl_groups$cards$add_p[["age_paired.wilcox.test_dots"]] |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist() |> unname(),
    wilcox.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE, mu = 1)$p.value
  )
})

test_that("Groups arg and lme4", {
  skip_if_not(is_pkg_installed(c("lme4", "broom.mixed"), reference_pkg = "cardx"))
  withr::local_package("broom")
  withr::local_package("lme4")

  trial_group <-
    trial |>
    dplyr::group_by(trt) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::ungroup()

  tbl_groups <-
    trial_group %>%
    select(trt, id,
      age_lme4 = age
    ) %>%
    tbl_summary(by = trt, missing = "no", include = -id) %>%
    add_p(
      test = list(contains("lme4") ~ "lme4"),
      group = "id"
    )
  expect_snapshot(as.data.frame(tbl_groups))

  expect_equal(
    tbl_groups$cards$add_p$age_lme4 |> dplyr::filter(stat_name %in% "p.value") |> dplyr::pull(stat) |> unlist(),
    glmer(factor(trt) ~ (1 | id), tidyr::drop_na(trial_group, trt, age, id), family = binomial) |>
      anova(glmer(factor(trt) ~ age + (1 | id), tidyr::drop_na(trial_group, trt, age, id), family = binomial)) |>
      tidy() |>
      dplyr::pull(p.value) |>
      dplyr::last()
  )
})

test_that("no error with missing data", {
  expect_message(
    t1 <-
      mtcars |>
      dplyr::mutate(
        mpg = NA,
        hp = NA,
        has_banana = factor(NA, levels = c("Yes", "No")),
        num_banana = factor(NA, levels = c("Zero", "1+"))
      ) |>
      dplyr::select(has_banana, num_banana, mpg, hp, am) |>
      tbl_summary(by = "am", type = hp ~ "continuous") |>
      add_p()
  )

  expect_equal(
    t1 |> as_tibble(col_labels = FALSE) |> dplyr::pull(p.value),
    rep_len(NA_character_, 10)
  )
})

test_that("add_p.tbl_summary() can be run after add_difference()", {
  expect_error(
    trial |>
      select(age, trt) |>
      tbl_summary(by = trt) |>
      add_difference() |>
      add_p(all_continuous() ~ "t.test")
  )

  expect_snapshot(
    error = TRUE,
    trial |>
      select(age, trt) |>
      tbl_summary(by = trt) |>
      add_p() |>
      add_p()
  )

  expect_error(
    trial |>
      select(age, trt) |>
      tbl_summary(by = trt) |>
      add_difference() |>
      add_difference()
  )

  expect_error(
    tbl <-
      trial |>
      select(age, trt) |>
      tbl_summary(
        by = trt,
        missing = "no",
        statistic = all_continuous() ~ "{mean}",
        digits = all_continuous() ~ 3
      ) %>%
      add_difference(all_continuous() ~ "cohens_d") |>
      add_p(all_continuous() ~ "t.test") |>
      as.data.frame(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl)

  expect_equal(
    tbl |>
      unlist(),
    c(
      label = "Age",
      stat_1 = "47.011",
      stat_2 = "47.449",
      estimate = "-0.03",
      conf.low = "-0.32, 0.25",
      p.value = "0.8"
    )
  )
  expect_true(
    tbl %>%
      select(ends_with(".x") | ends_with(".y")) %>%
      names() %>%
      rlang::is_empty()
  )
})


test_that("addressing GH #1513, where the default test was incorrect", {
  expect_equal(
    # before the fix, this was defaulting to a chi-squared, when it should be fisher
    dplyr::bind_rows(
      tidyr::uncount(dplyr::tibble(type = "A", answer = "C1"), 5),
      tidyr::uncount(dplyr::tibble(type = "B", answer = "C1"), 10),
      tidyr::uncount(dplyr::tibble(type = "A", answer = "C2"), 100),
      tidyr::uncount(dplyr::tibble(type = "B", answer = "C2"), 305),
      tidyr::uncount(dplyr::tibble(type = "A", answer = NA), 400),
      tidyr::uncount(dplyr::tibble(type = "B", answer = NA), 300)
    ) |>
      tbl_summary(by = type) |>
      assign_tests(include = "answer", calling_fun = "add_p") |>
      getElement(1L) |>
      attr("test_name"),
    "fisher.test"
  )
})
