skip_on_cran()

t1 <- trial %>%
  tbl_summary() %>%
  bold_labels() %>%
  italicize_levels()

t2 <-
  glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
  tbl_regression(exponentiate = TRUE) %>%
  bold_labels() %>%
  italicize_levels() %>%
  bold_p()

t3 <-
  tbl_uvregression(
    trial %>% dplyr::select(response, age, grade),
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) %>%
  bold_levels() %>%
  italicize_labels() %>%
  bold_p()

test_that("as_tibble works with standard use", {
  expect_error(as_tibble(t1), NA)
  expect_warning(as_tibble(t1), NA)
  expect_snapshot(as_tibble(t1) %>% as.data.frame())

  expect_error(as_tibble(t1, return_calls = TRUE), NA)
  expect_warning(as_tibble(t1, return_calls = TRUE), NA)

  expect_error(as_tibble(t2), NA)
  expect_warning(as_tibble(t2), NA)
  expect_snapshot(as_tibble(t3) %>% as.data.frame())

  expect_error(as_tibble(t3), NA)
  expect_warning(as_tibble(t3), NA)
  expect_snapshot(as_tibble(t3) %>% as.data.frame())
})


test_that("as_tibble works with bold_p()", {
  bold_p_data <- data.frame(
    x = c(rep("YES", 10), rep("NO", 10)),
    y = c(rep("YES", 9), rep("NO", 11)),
    a = c(rep(c(rep("YES", 5), rep("NO", 5)))),
    b = c(rep("YES", 4), rep("NO", 16))
  )

  # checking that the low pvalue is bolded, and the other two are not
  expect_equal(
    bold_p_data %>%
      tbl_summary(by = x) %>%
      add_p() %>%
      bold_labels() %>%
      bold_p() %>%
      as_tibble(col_labels = FALSE) %>%
      pull(p.value),
    c("__<0.001__", ">0.9", "0.087")
  )

  expect_equal(
    as_tibble(t2, col_labels = FALSE)$label,
    c("__Age__", "__Grade__", "_I_", "_II_", "_III_")
  )

  t1 <- trial %>%
    select(age) %>%
    tbl_summary()
  expect_equal(
    list(t1, t1) %>%
      tbl_stack(group_header = factor(c("tt", "yy"))) %>%
      as_tibble(col_labels = FALSE) %>%
      purrr::pluck("groupname_col"),
    c("tt", NA, "yy", NA)
  )
})


test_that("as_tibble(fmt_missing=) works", {
  t1 <-
    lm(mpg ~ factor(cyl), mtcars) %>%
    tbl_regression()

  t2 <-
    lm(mpg ~ factor(cyl), mtcars %>% dplyr::filter(cyl != 4)) %>%
    tbl_regression()

  expect_error(
    tbl <-
      tbl_merge(list(t1, t2)) %>%
      as_tibble(fmt_missing = TRUE, col_labels = FALSE),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())
  expect_equal(
    tbl$estimate_1,
    c(NA_character_, "—", "-6.9", "-12")
  )
  expect_equal(
    tbl$estimate_2,
    c(NA_character_, NA_character_, "—", "-4.6")
  )

  expect_equal(
    trial %>%
      select(age) %>%
      tbl_summary() %>%
      modify_table_body(
        ~ .x %>% mutate(stat_0 = NA_character_)
      ) %>%
      modify_table_styling(stat_0, rows = !is.na(label), missing_symbol = "-") %>%
      as_tibble(fmt_missing = TRUE, col_labels = FALSE) %>%
      dplyr::pull(stat_0),
    c("-", "-")
  )
})
