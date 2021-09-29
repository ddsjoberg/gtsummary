skip_on_cran()
library(dplyr)

test_that("add_p creates output without error/warning", {
  expect_error(
    tbl_summary(trial, by = grade) %>% add_p(),
    NA
  )

  expect_error(
    tbl_summary(mtcars, by = am) %>% add_p(),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = am) %>% add_p(),
    NA
  )

  expect_error(
    trial %>%
      tbl_summary(by = trt) %>%
      add_p(),
    NA
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

  expect_error(
    tbl_summary(trial, by = trt, include = -response) %>%
      add_p(group = response),
    NA
  )
})

test_that("add_p & lme4", {
  skip_if_not(require("lme4"))
  skip_if_not(require("broom.mixed"))
  expect_message(
    tbl_summary(trial, by = trt) %>%
      add_p(test = everything() ~ "lme4", group = response),
    NULL
  )
})

test_that("add_p creates output without error/warning for continuous2", {
  expect_error(
    tbl_summary(trial, by = grade, type = all_continuous() ~ "continuous2") %>% add_p(),
    NA
  )

  expect_error(
    tbl_summary(mtcars, by = am, type = all_continuous() ~ "continuous2") %>% add_p(),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = am, type = all_continuous() ~ "continuous2") %>% add_p(),
    NA
  )

  expect_error(
    trial %>%
      tbl_summary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(),
    NA
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

  expect_error(
    tbl_summary(trial, by = trt, include = -response, type = all_continuous() ~ "continuous2") %>%
      add_p(group = response),
    NA
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
    tbl_summary(mtcars, by = am) %>%
      add_p(test = list(
        vars(mpg) ~ "t.test",
        disp ~ "aov",
        cyl ~ "chisq.test.no.correct"
      )),
    NA
  )

  expect_error(
    tbl_summary(mtcars, by = am) %>%
      add_p(test = list(
        vars(mpg) ~ t.test,
        disp ~ aov
      )),
    NA
  )
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

  expect_error(
    trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ "my_mcnemar"),
    NA
  )
  expect_error(
    trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ "my_mcnemar2"),
    NA
  )

  expect_error(
    tbl_mcnemar <-
      trial[c("response", "trt")] %>%
      tbl_summary(by = trt) %>%
      add_p(test = response ~ my_mcnemar),
    NA
  )

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


trial_group <- trial %>%
  group_by(trt) %>%
  mutate(id = row_number()) %>%
  ungroup()
trial_group_wide <-
  trial_group %>%
  filter(trt == "Drug A") %>%
  full_join(
    trial_group %>%
      filter(trt == "Drug B"),
    by = "id"
  )

test_that("p-values are replicated within tbl_summary()", {
  tbl_test.args <-
    trial %>%
    select(trt,
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
        contains("fisher.test") ~ fisher.test
      ),
      test.args = list(
        var_t.test_dots = list(var.equal = TRUE),
        var_wilcox.test_dots = list(correct = FALSE),
        var_chisq.test_dots = list(correct = FALSE),
        var_fisher.test_dots = list(alternative = "greater")
      )
    )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_t.test")$p.value,
    t.test(age ~ as.factor(trt), data = trial)$p.value
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_t.test_dots")$p.value,
    t.test(age ~ as.factor(trt), data = trial, var.equal = TRUE)$p.value
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_kruskal.test")$p.value,
    kruskal.test(trial$age, as.factor(trial$trt))$p.value
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_wilcox.test")$p.value,
    wilcox.test(age ~ trt, data = trial)$p.value
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_wilcox.test_dots")$p.value,
    wilcox.test(age ~ trt, data = trial, correct = FALSE)$p.value
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_aov")$p.value,
    stats::aov(age ~ as.factor(trt), data = trial) %>%
      summary() %>%
      pluck(1, "Pr(>F)", 1)
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_chisq.test")$p.value,
    stats::chisq.test(x = trial[["response"]], y = as.factor(trial[["trt"]]))$p.value
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_chisq.test_dots")$p.value,
    stats::chisq.test(x = trial[["response"]], y = as.factor(trial[["trt"]]), correct = FALSE)$p.value
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_chisq.test.no.correct")$p.value,
    stats::chisq.test(x = trial[["response"]], y = as.factor(trial[["trt"]]), correct = FALSE)$p.value
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_fisher.test")$p.value,
    fisher.test(trial[["response"]], as.factor(trial[["trt"]]))$p.value
  )

  expect_equal(
    filter(tbl_test.args$meta_data, variable == "var_fisher.test_dots")$p.value,
    fisher.test(trial[["response"]], as.factor(trial[["trt"]]), alternative = "greater")$p.value
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
        response_mcnemar.test_dots ~ list(correct  = FALSE),
        age_paired.wilcox.test_dots ~ list(mu = 1)
      ),
      group = "id"
    )

  expect_equal(
    filter(tbl_groups$meta_data, variable == "response_mcnemar.test_dots")$p.value,
    mcnemar.test(trial_group_wide[["response.x"]], trial_group_wide[["response.y"]],
                 correct  = FALSE)$p.value
  )

  expect_equal(
    filter(tbl_groups$meta_data, variable == "response_mcnemar.test")$p.value,
    mcnemar.test(trial_group_wide[["response.x"]], trial_group_wide[["response.y"]])$p.value
  )

  expect_equal(
    filter(tbl_groups$meta_data, variable == "grade_mcnemar.test")$p.value,
    mcnemar.test(trial_group_wide[["grade.x"]], trial_group_wide[["grade.y"]])$p.value
  )

  expect_equal(
    filter(tbl_groups$meta_data, variable == "age_paired.t.test")$p.value,
    t.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE)$p.value
  )

  expect_equal(
    filter(tbl_groups$meta_data, variable == "age_paired.t.test_dots")$p.value,
    t.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE, mu = 1)$p.value
  )

  expect_equal(
    filter(tbl_groups$meta_data, variable == "age_paired.wilcox.test")$p.value,
    wilcox.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE)$p.value
  )

  expect_equal(
    filter(tbl_groups$meta_data, variable == "age_paired.wilcox.test_dots")$p.value,
    wilcox.test(trial_group_wide[["age.x"]], trial_group_wide[["age.y"]], paired = TRUE, mu = 1)$p.value
  )
})

test_that("Groups arg and lme4", {
  skip_if_not(require("lme4"))
  skip_if_not(require("broom.mixed"))

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

  expect_equal(
    filter(tbl_groups$meta_data, variable == "age_lme4")$p.value,
    lme4::glmer(factor(trt) ~ (1 | id), tidyr::drop_na(trial_group, trt, age, id), family = binomial) %>%
      anova(lme4::glmer(factor(trt) ~ age + (1 | id), tidyr::drop_na(trial_group, trt, age, id), family = binomial)) %>%
      pluck("Pr(>Chisq)", 2)
  )
})

test_that("difftime works with wolcox", {
  expect_equal(
    trial %>%
      mutate(
        time_diff = as.difftime(age, units = "mins")
      ) %>%
      select(trt, time_diff) %>%
      tbl_summary(by=trt) %>%
      add_p() %>%
      inline_text(variable = time_diff, column = "p.value"),
    "p=0.7"
  )
})

test_that("no error with missing data", {
  expect_message(
    t1 <-
      mtcars %>%
      mutate(mpg = NA, hp = NA, has_banana = factor(NA, levels = c("Yes", "No"))) %>%
      select(has_banana, mpg, hp, am) %>%
      tbl_summary(by = "am", type = hp ~ "continuous") %>%
      add_p()
  )
  expect_equal(
    t1 %>% as_tibble(col_labels = FALSE) %>% dplyr::pull(p.value),
    rep_len(NA_character_, 8)
  )
})

test_that("add_p can be run after add_difference", {
  expect_error(
    trial %>%
      select(age, trt) %>%
      tbl_summary(by = trt) %>%
      add_difference() %>%
      add_p(all_continuous() ~ "t.test")
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

  expect_equal(
    tbl %>%
      unlist(),
    c(label = "Age",
      stat_1 = "47.011",
      stat_2 = "47.449",
      estimate = "-0.03",
      ci = "-0.32, 0.25",
      p.value = "0.8")
  )
  expect_true(
    tbl %>%
      select(ends_with(".x") | ends_with(".y")) %>%
      names() %>%
      rlang::is_empty()
  )


})
