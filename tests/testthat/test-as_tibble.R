t1 <- trial %>%
  tbl_summary()

t2 <- glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
  tbl_regression(exponentiate = TRUE)

t3 <- tbl_uvregression(
  trial %>% dplyr::select(response, age, grade),
  method = glm,
  y = response,
  method.args = list(family = binomial),
  exponentiate = TRUE
)

test_that("as_tibble works with standard use", {
  expect_silent(res <- as_tibble(t1))
  expect_silent(as_tibble(t1, return_calls = TRUE))
  expect_snapshot(res %>% as.data.frame())

  expect_silent(res <- as_tibble(t2))
  expect_snapshot(res %>% as.data.frame())

  expect_silent(res <- as_tibble(t3))
  expect_snapshot(res %>% as.data.frame())
})

test_that("as_tibble(col_labels=) works", {
  expect_equal(
    as_tibble(t2, col_labels = FALSE) %>% names(),
    c("label", "estimate", "conf.low", "p.value")
  )

  expect_equal(
    as_tibble(t2) %>% names(),
    c("**Characteristic**", "**OR**", "**95% CI**", "**p-value**")
  )
})

t1 <- t1 %>%
  bold_labels() %>%
  italicize_levels()

t2 <- t2 %>%
  bold_labels() %>%
  italicize_levels()

t3 <- t3 %>%
  bold_levels() %>%
  italicize_labels()

test_that("as_tibble works with bold/italics", {
  expect_silent(res <- as_tibble(t1))
  expect_snapshot(res %>% as.data.frame())

  expect_silent(res <- as_tibble(t2))
  expect_snapshot(res %>% as.data.frame())
  expect_equal(
    as_tibble(t2, col_labels = FALSE)$label,
    c("__Age__", "__Grade__", "_I_", "_II_", "_III_")
  )

  expect_silent(res <- as_tibble(t3))
  expect_snapshot(res %>% as.data.frame())
})

t2 <- t2 %>% bold_p(t = 0.10)

test_that("as_tibble works with bold_p()", {
  bold_p_data <- data.frame(
    x = c(rep("YES", 10), rep("NO", 10)),
    y = c(rep("YES", 9), rep("NO", 11)),
    a = c(rep(c(rep("YES", 5), rep("NO", 5)))),
    b = c(rep("YES", 4), rep("NO", 16))
  )

  expect_equal(
    bold_p_data %>%
      tbl_summary(by = x) %>%
      add_p() %>%
      bold_labels() %>%
      bold_p() %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull(p.value),
    c("__<0.001__", ">0.9", "0.087")
  )

  expect_equal(
    as_tibble(t2, col_labels = FALSE)$p.value,
    c("__0.10__", NA, NA, "0.7", ">0.9")
  )
})

test_that("as_tibble works with formatting functions", {
  t2_modify_fmt <- t2 %>%
    modify_fmt_fun(
      p.value ~ function(x) style_pvalue(x, digits = 3),
      rows = variable == "grade"
    ) %>%
    modify_fmt_fun(
      estimate ~ function(x) style_ratio(x, digits = 4, decimal.mark = ",")
    )

  expect_silent(res <- as_tibble(t2_modify_fmt))
  expect_snapshot(res %>% as.data.frame())
  expect_equal(
    as_tibble(t2_modify_fmt, col_labels = FALSE)$p.value,
    c("__0.10__", NA, NA, "0.688", "0.972")
  )
  expect_equal(
    as_tibble(t2_modify_fmt, col_labels = FALSE)$estimate,
    c("1,0191", NA, NA, "0,8535", "1,0136")
  )

  t3_modify_fmt <- t3 %>%
    modify_fmt_fun(
      stat_n ~ function(x) style_number(x, digits = 2),
      rows = variable == "age"
    ) %>%
    modify_fmt_fun(
      stat_n ~ styfn_number(digits = 4),
      rows = variable == "grade"
    ) %>%
    modify_fmt_fun(
      c(conf.low, conf.high) ~ styfn_sigfig(digits = 3)
    )

  expect_silent(res <- as_tibble(t3_modify_fmt))
  expect_snapshot(res %>% as.data.frame())
  expect_equal(
    as_tibble(t3_modify_fmt, col_labels = FALSE)$stat_n,
    c("183.00", "193.0000", NA, NA, NA)
  )
  expect_equal(
    as_tibble(t3_modify_fmt, col_labels = FALSE)$conf.low,
    c("0.997, 1.04", NA, NA, "0.446, 2.00", "0.524, 2.29")
  )
})

t4 <- lm(mpg ~ factor(cyl), mtcars) %>%
  tbl_regression()

t5 <- lm(mpg ~ factor(cyl), mtcars %>% dplyr::filter(cyl != 4)) %>%
  tbl_regression()

tbl <- tbl_merge(list(t4, t5))

test_that("as_tibble works with column merging", {
  expect_silent(res <- as_tibble(tbl))
  expect_snapshot(res %>% as.data.frame())
})

test_that("as_tibble(fmt_missing=) works", {
  expect_silent(
    res <- tbl %>% as_tibble(fmt_missing = TRUE, col_labels = FALSE)
  )
  expect_snapshot(res %>% as.data.frame())
  expect_equal(
    res$estimate_1,
    c(NA_character_, "—", "-6.9", "-12")
  )
  expect_equal(
    res$estimate_2,
    c(NA_character_, NA_character_, "—", "-4.6")
  )

  expect_equal(
    trial %>%
      select(age) %>%
      tbl_summary() %>%
      modify_table_body(
        ~ .x %>% mutate(stat_0 = NA_character_)
      ) %>%
      modify_table_styling(stat_0, rows = !is.na(label), missing_symbol = "n / a") %>%
      as_tibble(fmt_missing = TRUE, col_labels = FALSE) %>%
      dplyr::pull(stat_0),
    c("n / a", "n / a")
  )
})
