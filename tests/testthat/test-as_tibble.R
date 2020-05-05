context("test-as_tibble")

t1 <- trial %>% tbl_summary()
t2 <-
  glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
  tbl_regression(exponentiate = TRUE)
t3 <-
  tbl_uvregression(
    trial %>% dplyr::select(response, age, grade),
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE
  )

test_that("as_tibble works with standard use", {
  expect_error(as_tibble(t1), NA)
  expect_warning(as_tibble(t1), NA)
  expect_error(as_tibble(t1, return_calls = TRUE), NA)
  expect_warning(as_tibble(t1, return_calls = TRUE), NA)
  expect_error(as_tibble(t2), NA)
  expect_warning(as_tibble(t2), NA)
  expect_error(as_tibble(t3), NA)
  expect_warning(as_tibble(t3), NA)
})


test_that("as_tibble works with bold_p()", {
  bold_p_data = data.frame(
    x = c(rep("YES",10), rep("NO",10)),
    y = c(rep("YES",9), rep("NO", 11)),
    a = c(rep(c(rep("YES",5), rep("NO",5)))),
    b = c(rep("YES",4), rep("NO", 16))
  )

  # checking that the low pvalue is bolded, and the other two are not
  expect_equal(
    bold_p_data %>%
      tbl_summary(by = x) %>%
      add_p %>%
      bold_labels() %>%
      bold_p() %>%
      as_tibble(col_labels = FALSE) %>%
      pull(p.value),
    c("__<0.001__", ">0.9", "0.087")
  )
})
