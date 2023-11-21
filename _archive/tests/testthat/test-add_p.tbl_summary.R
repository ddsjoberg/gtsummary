skip_on_cran()

test_that("add_p creates output without error/warning", {
  expect_snapshot(
    tbl_summary(trial, by = grade) %>% add_p() %>% as.data.frame()
  )

  tbl <- tbl_summary(mtcars, by = am) %>%
    add_p() %>%
    as_tibble()
  expect_snapshot(tbl)
  expect_warning(
    tbl_summary(mtcars, by = am) %>% add_p(),
    NA
  )

  expect_snapshot(
    trial %>%
      tbl_summary(by = trt) %>%
      add_p() %>% as.data.frame()
  )

  expect_warning(
    trial %>%
      tbl_summary(by = trt) %>%
      add_p(),
    NA
  )

  expect_message(
    trial %>%
      tbl_summary(by = trt) %>%
      add_p(),
    NA
  )

  expect_snapshot(
    tbl_summary(trial, by = trt, include = -response) %>%
      add_p(group = response) %>% as.data.frame()
  )
})

test_that("add_p & lme4", {
  skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))
  skip_if_not(broom.helpers::.assert_package("broom.mixed", pkg_search = "gtsummary", boolean = TRUE))

  expect_message(
    tbl_summary(trial, by = trt) %>%
      add_p(test = everything() ~ "lme4", group = response),
    NULL
  )

  expect_message(
    tbl_summary(trial, by = trt) %>%
      add_p(test = everything() ~ "lme4")
  )
})

test_that("add_p creates output without error/warning for continuous2", {
  expect_snapshot(
    tbl_summary(trial, by = grade, type = all_continuous() ~ "continuous2") %>% add_p() %>% as_tibble()
  )

  tbl <- tbl_summary(mtcars, by = am, type = all_continuous() ~ "continuous2") %>%
    add_p() %>%
    as_tibble()
  expect_snapshot(tbl)
  expect_warning(
    tbl_summary(mtcars, by = am, type = all_continuous() ~ "continuous2") %>% add_p(),
    NA
  )

  expect_snapshot(
    trial %>%
      tbl_summary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p() %>% as.data.frame()
  )

  expect_warning(
    trial %>%
      tbl_summary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(),
    NA
  )

  expect_message(
    trial %>%
      tbl_summary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(),
    NA
  )

  expect_snapshot(
    tbl_summary(trial, by = trt, include = -response, type = all_continuous() ~ "continuous2") %>%
      add_p(group = response) %>% as.data.frame()
  )
})

test_that("add_p creates errors with bad args", {
  expect_error(
    tbl_summary(mtcars, by = am) %>%
      add_p(pvalue_fun = mtcars),
    NULL
  )

  expect_error(
    tbl_summary(trial, by = grade, include = -response) %>%
      add_p(group = response),
    NULL
  )
})


test_that("add_p works well", {
  expect_error(
    tbl <-
      tbl_summary(mtcars, by = am) %>%
      add_p(
        test = list(
          vars(mpg) ~ "t.test",
          disp ~ "aov",
          hp ~ "oneway.test",
          cyl ~ "chisq.test.no.correct",
          carb ~ "mood.test"
        )
      ) %>%
      as_tibble(),
    NA
  )
  expect_snapshot(tbl)

  expect_error(
    tbl <-
      tbl_summary(mtcars, by = am) %>%
      add_p(
        test = list(
          vars(mpg) ~ t.test,
          disp ~ aov
        )
      ) %>%
      as_tibble(),
    NA
  )
  expect_snapshot(tbl)
})

test_that("add_p with custom p-value function", {
  my_mcnemar <- function(data, variable, by, ...) {
    result <- list()
    result$p <- stats::mcnemar.test(data[[variable]], data[[by]])$p.value
    result$test <- "McNemar's test"
    result
  }

  my_mcnemar2 <- function(data, variable, by, ...) {
    stats::mcnemar.test(data[[variable]], data[[by]])$p.value
  }

  expect_snapshot(
    trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ "my_mcnemar") %>% as.data.frame()
  )
  expect_snapshot(
    trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ "my_mcnemar2") %>% as.data.frame()
  )

  expect_error(
    tbl_mcnemar <-
      trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ my_mcnemar),
    NA
  )
  expect_snapshot(tbl_mcnemar %>% as.data.frame())

  expect_equal(
    tbl_mcnemar$meta_data$p.value,
    stats::mcnemar.test(trial[["response"]], trial[["trt"]])$p.value
  )
})

test_that("Wilcoxon and Kruskal-Wallis p-values match ", {
  t1 <- trial[c("trt", "age", "marker")] %>%
    tbl_summary(by = trt) %>%
    add_p(test = all_continuous() ~ wilcox.test)
  t2 <- trial[c("trt", "age", "marker")] %>%
    tbl_summary(by = trt) %>%
    add_p(test = all_continuous() ~ kruskal.test)
  expect_true(
    all(t1$meta_data$p.value - t2$meta_data$p.value < 0.001)
  )
})


trial_group <-
  trial %>%
  dplyr::group_by(trt) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::ungroup()
trial_group_wide <-
  trial_group %>%
  dplyr::filter(trt == "Drug A") %>%
  dplyr::full_join(
    trial_group %>%
      filter(trt == "Drug B"),
    by = "id"
  )

test_that("p-values are replicated within tbl_summary()", {
  tbl_test.args <-
    trial %>%
    dplyr::select(trt,
      var_t.test = age,
      var_t.test_dots = age,
      var_kruskal.test = age,
      var_wilcox.test = age,
      var_wilcox.test_dots = age,
      var_aov = age,
      var_chisq.test = response,
      var_chisq.test_dots = response,
      var_chisq.test.no.correct = response,
      var_fisher.test = response,
      var_fisher.test_dots = response,
      var_mcnemar.test = response,
      var_mcnemar.test_dots = response,
    ) %>%
    tbl_summary(by = trt, missing = "no") %>%
    add_p(
      test = list(
        contains("t.test") ~ t.test,
        contains("kruskal.test") ~ kruskal.test,
        contains("wilcox.test") ~ wilcox.test,
        contains("aov") ~ aov,
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
  expect_snapshot(tbl_test.args %>% as.data.frame())

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_t.test")$p.value,
    t.test(age ~ as.factor(trt), data = trial)$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_t.test_dots")$p.value,
    t.test(age ~ as.factor(trt), data = trial, var.equal = TRUE)$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_kruskal.test")$p.value,
    kruskal.test(trial$age, as.factor(trial$trt))$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_wilcox.test")$p.value,
    wilcox.test(age ~ trt, data = trial)$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_wilcox.test_dots")$p.value,
    wilcox.test(age ~ trt, data = trial, correct = FALSE)$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_aov")$p.value,
    stats::aov(age ~ as.factor(trt), data = trial) %>%
      summary() %>%
      pluck(1, "Pr(>F)", 1)
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_chisq.test")$p.value,
    stats::chisq.test(x = trial[["response"]], y = as.factor(trial[["trt"]]))$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_chisq.test_dots")$p.value,
    stats::chisq.test(x = trial[["response"]], y = as.factor(trial[["trt"]]), correct = FALSE)$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_chisq.test.no.correct")$p.value,
    stats::chisq.test(x = trial[["response"]], y = as.factor(trial[["trt"]]), correct = FALSE)$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_fisher.test")$p.value,
    fisher.test(trial[["response"]], as.factor(trial[["trt"]]))$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_fisher.test_dots")$p.value,
    fisher.test(trial[["response"]], as.factor(trial[["trt"]]), alternative = "greater")$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_mcnemar.test")$p.value,
    mcnemar.test(trial[["response"]], as.factor(trial[["trt"]]))$p.value
  )

  expect_equal(
    dplyr::filter(tbl_test.args$meta_data, variable == "var_mcnemar.test_dots")$p.value,
    mcnemar.test(trial[["response"]], as.factor(trial[["trt"]]), correct = FALSE)$p.value
  )

  expect_message(
    trial_group %>%
      tbl_summary(include = age, by = trt) %>%
      add_p(test = age ~ "paired.t.test", include = age)
  )

  expect_message(
    trial_group %>%
      dplyr::filter(dplyr::row_number() != 1L) %>%
      tbl_summary(include = c(marker, age), by = trt) %>%
      add_p(
        test = list(age = "paired.t.test", marker = "paired.wilcox.test"),
        include = age, group = id
      )
  )


  tbl_groups <-
    trial_group %>%
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
    ) %>%
    tbl_summary(by = trt, missing = "no", include = -id) %>%
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
  expect_snapshot(tbl_groups %>% as.data.frame())

  expect_equal(
    dplyr::filter(tbl_groups$meta_data, variable == "response_mcnemar.test_dots")$p.value,
    mcnemar.test(trial_group_wide[["response.x"]], trial_group_wide[["response.y"]],
      correct = FALSE
    )$p.value
  )

  expect_equal(
    dplyr::filter(tbl_groups$meta_data, variable == "response_mcnemar.test")$p.value,
    mcnemar.test(trial_group_wide[["response.x"]], trial_group_wide[["response.y"]])$p.value
  )

  expect_equal(
    dplyr::filter(tbl_groups$meta_data, variable == "grade_mcnemar.test")$p.value,
    mcnemar.test(trial_group_wide[["grade.x"]], trial_group_wide[["grade.y"]])$p.value
  )

  expect_equal(
    dplyr::filter(tbl_groups$meta_data, variable == "age_paired.t.test")$p.value,
    t.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE)$p.value
  )

  expect_equal(
    dplyr::filter(tbl_groups$meta_data, variable == "age_paired.t.test_dots")$p.value,
    t.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE, mu = 1)$p.value
  )

  expect_equal(
    dplyr::filter(tbl_groups$meta_data, variable == "age_paired.wilcox.test")$p.value,
    wilcox.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE)$p.value
  )

  expect_equal(
    dplyr::filter(tbl_groups$meta_data, variable == "age_paired.wilcox.test_dots")$p.value,
    wilcox.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE, mu = 1)$p.value
  )
})

test_that("Groups arg and lme4", {
  skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))
  skip_if_not(broom.helpers::.assert_package("broom.mixed", pkg_search = "gtsummary", boolean = TRUE))

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
  expect_snapshot(tbl_groups %>% as.data.frame())

  expect_equal(
    dplyr::filter(tbl_groups$meta_data, variable == "age_lme4")$p.value,
    lme4::glmer(factor(trt) ~ (1 | id), tidyr::drop_na(trial_group, trt, age, id), family = binomial) %>%
      anova(lme4::glmer(factor(trt) ~ age + (1 | id), tidyr::drop_na(trial_group, trt, age, id), family = binomial)) %>%
      pluck("Pr(>Chisq)", 2)
  )
})

test_that("difftime works with Wilcox", {
  expect_equal(
    trial %>%
      dplyr::mutate(
        time_diff = as.difftime(age, units = "mins")
      ) %>%
      dplyr::select(trt, time_diff) %>%
      tbl_summary(by = trt) %>%
      add_p() %>%
      inline_text(variable = time_diff, column = "p.value"),
    "p=0.7"
  )
})

test_that("no error with missing data", {
  expect_message(
    t1 <-
      mtcars %>%
      dplyr::mutate(mpg = NA, hp = NA, has_banana = factor(NA, levels = c("Yes", "No"))) %>%
      dplyr::select(has_banana, mpg, hp, am) %>%
      tbl_summary(by = "am", type = hp ~ "continuous") %>%
      add_p()
  )
  expect_snapshot(t1 %>% as.data.frame())
  expect_equal(
    t1 %>% as_tibble(col_labels = FALSE) %>% dplyr::pull(p.value),
    rep_len(NA_character_, 8)
  )
})

test_that("add_p can be run after add_difference()", {
  expect_error(
    trial %>%
      select(age, trt) %>%
      tbl_summary(by = trt) %>%
      add_difference() %>%
      add_p(all_continuous() ~ "t.test")
  )

  expect_error(
    trial %>%
      select(age, trt) %>%
      tbl_summary(by = trt) %>%
      add_p() %>%
      add_p()
  )

  expect_error(
    trial %>%
      select(age, trt) %>%
      tbl_summary(by = trt) %>%
      add_difference() %>%
      add_difference()
  )

  expect_error(
    tbl <-
      trial %>%
      select(age, trt) %>%
      tbl_summary(
        by = trt,
        missing = "no",
        statistic = all_continuous() ~ "{mean}",
        digits = all_continuous() ~ 3
      ) %>%
      add_difference(all_continuous() ~ "cohens_d") %>%
      add_p(all_continuous() ~ "t.test") %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl)

  expect_equal(
    tbl %>%
      unlist(),
    c(
      label = "Age",
      stat_1 = "47.011",
      stat_2 = "47.449",
      estimate = "-0.03",
      ci = "-0.32, 0.25",
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
    # before the fix, this was defaulting to a chi-sqaured, when it should be fisher
    tibble::tibble(type = character(), answer = character()) %>%
      tibble::add_row(tidyr::uncount(tibble::tibble(type = "A", answer = "C1"), 5)) %>%
      tibble::add_row(tidyr::uncount(tibble::tibble(type = "B", answer = "C1"), 10)) %>%
      tibble::add_row(tidyr::uncount(tibble::tibble(type = "A", answer = "C2"), 100)) %>%
      tibble::add_row(tidyr::uncount(tibble::tibble(type = "B", answer = "C2"), 305)) %>%
      tibble::add_row(tidyr::uncount(tibble::tibble(type = "A", answer = NA), 400)) %>%
      tibble::add_row(tidyr::uncount(tibble::tibble(type = "B", answer = NA), 300)) %>%
      .assign_test_tbl_summary(
        variable = "answer", summary_type = "categorical", by = "type",
        group = NULL, test = NULL
      ),
    "fisher.test"
  )
})
